{-|
    This module is about continuous signals.

    For a general introduction to signals, see the documentation of "FRP.Grapefruit.Signal".
-}
module FRP.Grapefruit.Signal.Continuous (

    -- * Continuous signal type
    CSignal,

    -- * Conversion
    fromSSignal,

    -- * Connectors
    producer

) where

    -- Control
    import Control.Applicative as Applicative
    import Control.Arrow       as Arrow
    import Control.Compose     as Compose

    -- Data
    import Data.Unique as Unique

    -- Internal
    import           Internal.Signal                    as Signal
    import           Internal.Signal.Continuous.Segment as CSeg    hiding (producer)
    import qualified Internal.Signal.Continuous.Segment as CSeg
    import           Internal.Signal.Discrete.Capsule   as Capsule
    import           Internal.Signal.Discrete           as DSignal (DSignal)
    import qualified Internal.Signal.Discrete           as DSignal
    import           Internal.Signal.Segmented          as SSignal hiding (producer)

    -- Internal
    import Internal.Circuit as Circuit

    -- * Continuous signal type
    {-|
        The type of continuous signals.

        A continuous signal denotes a mapping from times to values. You can think of @CSignal /era/
        /val/@ as being equivalent to @Time /era/ -> /val/@ where @Time /era/@ is the type of all
        times of the given era.

        Continuous signals are used to describe continuously changing values. They are also used for
        values changing at discrete times if there is no possibility of being notified about such
        changes. If there is a notification mechanism then segemented signals, provided by
        "FRP.Grapefruit.Signal.Segmented", should be used.
    -}
    data CSignal era val = CSignal (Capsule val) !(SSignal era (CSeg val))
    {-
        The strictness annotation ensures that reducing the CSignal reduces the SSignal, thereby
        triggering reading of continous sources the SSignal depends on.
    -}

    instance Functor (CSignal era) where

        fmap fun (CSignal initCap segs) = CSignal (fmap fun initCap) ((fmap . fmap) fun segs)

    instance Applicative (CSignal era) where

        pure val = CSignal (pure val) ((pure . pure) val)

        CSignal funInitCap funSegs <*> CSignal argInitCap argSegs = CSignal initCap' segs' where

            initCap' = funInitCap <*> argInitCap

            segs'    = liftA2 (<*>) funSegs argSegs

    instance Signal CSignal where

        osfSwitch signal@(SSignal init _) = CSignal ((initCap . unPolyOSF) init) segs' where

            segs' = osfSwitch (segsSignal signal)

        ssfSwitch (SSignal init upd) (CSignal initCap segs) = ssfSwitch sampler segs where

            sampler = SSignal (fixInitCapForInit init initCap)
                              (DSignal.timeIDApp (fixInitCapForUpd <$> upd) <#> segs)

    initCap :: CSignal era val -> Capsule val
    initCap (CSignal initCap _) = initCap

    segsSignal :: SSignal era (PolyOSF CSignal val)
               -> SSignal era (PolyOSF SSignal (CSeg val))
    segsSignal = fmap (\polyOSF -> PolyOSF (segs (unPolyOSF polyOSF)))

    segs :: CSignal era' val -> SSignal era' (CSeg val)
    segs (CSignal _ segs) = segs

    fixInitCapForInit :: PolySSF CSignal val shape -> Capsule val -> PolySSF SSignal (CSeg val) shape
    fixInitCapForInit fun initCap = PolySSF (\segs -> unPolySSF fun (CSignal initCap segs))

    fixInitCapForUpd :: PolySSF CSignal val shape
                     -> Unique
                     -> CSeg val
                     -> PolySSF SSignal (CSeg val) shape
    fixInitCapForUpd fun timeID initSeg = result where

        result = PolySSF (\segs -> unPolySSF fun (CSignal (currentValCapsule timeID initSeg) segs))

    instance Samplee CSignal where

        dSample sampler (CSignal _ segs) = (DSignal.crackCapsules . DSignal.timeIDApp) $
                                           timeIDToCapsule <$> sampler <#> segs where

            timeIDToCapsule fun seg = fmap fun . flip currentValCapsule seg

        sSample (SSignal samplerInit samplerUpd) signal@(CSignal (Capsule init) _) = signal' where

            signal' = SSignal (samplerInit init) (samplerUpd <#> signal)

    -- * Conversion
    {-|
        Converts a segmented signal into a continous signal, dropping the information about update
        points.
    -}
    fromSSignal :: SSignal era val -> CSignal era val
    fromSSignal signal@(SSignal init _) = CSignal (pure init) (fmap pure signal)

    -- * Connectors
    {-|
        Converts a value read action into a continuous signal producer.

        The producer @producer /readVal/@ produces a continuous signal whose current value is
        determined by executing @/readVal/@.
    -}
    producer :: IO val -> Producer CSignal val
    producer readVal = Producer $
                       proc _ -> do
                           seg <- CSeg.producer readVal -< ()
                           startTimeID <- getStartTimeID -< ()
                           returnA -< CSignal (currentValCapsule startTimeID seg) (pure seg)
