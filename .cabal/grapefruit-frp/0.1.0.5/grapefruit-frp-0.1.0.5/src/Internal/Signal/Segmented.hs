module Internal.Signal.Segmented (

    -- * Segmented signal type
    SSignal (SSignal),

    -- * Construction
    construct,
    fromInitAndUpdate,

    -- * Queries
    withInit,
    updates,

    -- * Stateful signals
    scan,

    -- * Capsules
    crackCapsules,

    -- * Connectors
    consumer,
    producer

) where

    -- Prelude
    import Prelude hiding (init)

    -- Control
    import Control.Applicative as Applicative
    import Control.Arrow       as Arrow

    -- Internal
    import           Internal.Signal                    as Signal
    import           Internal.Signal.Discrete.Capsule   as Capsule
    import           Internal.Signal.Discrete           as DSignal (DSignal)
    import qualified Internal.Signal.Discrete           as DSignal

    -- FRP.Grapefruit
    import FRP.Grapefruit.Setup   as Setup
    import FRP.Grapefruit.Circuit as Circuit

    -- * Segmented signal type
    {-|
        The type of segmented signals.

        A segmented signal maps times to values like a continuous signal. However, it also comprises
        a set of discrete times, called /update points/. The signal can only change its value at its
        update points. As a special case, the starting time of the era is always considered an
        update point. So a segmented signal is composed of constant segments which are either
        bounded by adjacent update points or left-bounded by a last update point and
        right-unbounded. Note that value updates already take effect at the update point so that the
        segments are left-closed.

        It follows that a segmented signal is completely determined by the update points and the
        values assigned to them. Therefore, a segmented signal can also be seen as a kind of
        discrete signal with occurences at the update points. The only difference to a discrete
        signal is that a segmented signal always has an occurence at the starting time of the era
        whereas a discrete signal never has one.

        The dual nature of segmented signals is reflected by the class instances of @SSignal@.
        @SSignal@ is an instance of 'Samplee' as well as of 'Sampler'. The first means that it can
        be sampled and therefore has a continuous aspect. The second means that it can be used to
        sample a signal and therefore has a discrete aspect.
    -}
    data SSignal era val = SSignal val (DSignal era val)
    {-
        Reducing the signal (matching against (SSignal _ _)) forces all continous sources, the
        signal depends on, to be read. Similar for reducing DSignal values (means reduction of the
        map) and continous sources. Note that in the latter case, the initial value is not
        necessarily reduced but initial values of other continous signals which the continous
        signal’s internal SSignal depends on.

        In the case of SSignal, continuous sources have to be read at the beginning. This can be
        illustrated by thinking of the initial value as an occurence at starting time.

        It is important that upon construction of an SSignal/CSignal via a function the SSignal and
        CSignal constructors of arguments have to be reduced during reduction of the result.
        Otherwise triggering of continuous source reads would not work properly.
    -}

    instance Functor (SSignal era) where

        fmap fun (SSignal init upd) = SSignal (fun init) (fmap fun upd)

    instance Applicative (SSignal era) where

        pure val = SSignal val DSignal.empty

        SSignal funInit funUpd <*> SSignal argInit argUpd = SSignal init' upd' where

            init' = funInit argInit

            upd'  = fmap (uncurry ($)) $
                    DSignal.scan (funInit,argInit) (flip ($)) $
                    DSignal.transUnion (first . const)
                                       (second . const)
                                       ((const .) . (,))
                                       funUpd
                                       argUpd

    instance Signal SSignal where

        osfSwitch signal@(SSignal init upd) = case unPolyOSF init of
                                                  SSignal init' _ -> SSignal init' upd'
                                              where

            upd'  = initUpdate upd `DSignal.union` osfSwitch (updateSignal signal)

        ssfSwitch signal arg@(SSignal _ argUpd) = ssfSwitch (fixInit <$> signal <#> arg) argUpd

    initUpdate :: DSignal era (PolyOSF SSignal val) -> DSignal era val
    initUpdate = DSignal.crackCapsules . fmap (initCapsule . unPolyOSF)

    initCapsule :: SSignal era' val -> Capsule val
    initCapsule (SSignal init _) = Applicative.pure init

    updateSignal :: SSignal era (PolyOSF SSignal val)
                 -> SSignal era (PolyOSF DSignal val)
    updateSignal signal = crackCapsules (fmap updateCapsule signal)

    updateCapsule :: PolyOSF SSignal val -> Capsule (PolyOSF DSignal val)
    updateCapsule signal = unPolyOSF signal `seq` Capsule (PolyOSF (updates (unPolyOSF signal)))

    fixInit :: PolySSF SSignal val shape -> val -> PolySSF DSignal val shape
    fixInit fun init = PolySSF (unPolySSF fun . SSignal init)

    instance Sampler SSignal where

        sample = sSample

        samplerMap = fmap

    instance Samplee SSignal where

        dSample funs (SSignal argInit argUpd) = dSignal' where

            dSignal' = DSignal.catMaybes $
                       DSignal.stateful argInit $
                       DSignal.transUnion (\fun currentArg -> (Just (fun currentArg),currentArg))
                                          (\nextArg _      -> (Nothing,nextArg))
                                          (\fun nextArg _  -> (Just (fun nextArg),nextArg))
                                          funs
                                          argUpd

        sSample (SSignal samplerInit samplerUpd) signal@(SSignal init _) = SSignal init' upd' where

            init' = samplerInit init

            upd'  = samplerUpd <#> signal

    -- * Construction
    {-|
        Constructs a segmented signal from an initial value and a series of updates.

        A signal @construct /init/ /upd/@ has initially the value @/init/@. At each occurence in
        @/upd/@, it has an update point and changes its value to the value occuring in @/upd/@. If
        the segmented signal is interpreted as a kind of discrete signal, @fromInitAndUpdate@ just
        adds an initial occurence of @/init/@ to the signal @/upd/@.
    -}
    construct :: val -> DSignal era val -> SSignal era val
    construct val upd = SSignal val upd

    {-# DEPRECATED fromInitAndUpdate "fromInitAndUpdate is replaced by construct." #-}
    -- |Same as 'construct'.
    fromInitAndUpdate :: val -> DSignal era val -> SSignal era val
    fromInitAndUpdate val upd = SSignal val upd

    -- * Queries
    -- FIXME: Is it safe to support arbitrary signal types here?
    {-|
        Applies the second argument to the initial value of the first argument.

        Using @withInit@, it is possible to create a signal which is dependent on the initial value
        of a segmented signal but it is not possible to extract the initial value itself. The reason
        for this restriction is that the initial value may depend on values of continuous signals
        and therefore its calculation might involve doing I/O to read external continuous sources.
    -}
    withInit :: (Signal signal) => SSignal era val -> (val -> signal era val') -> signal era val'
    withInit (SSignal init _) cont = cont init

    -- Should be safe w.r.t. continous source fetching.
    {-|
        Yields the sequence of updates of a segmented signal.

        If the segmented signal is interpreted as a discrete signal with an additional occurence at
        the start then @update@ just drops this occurence.
    -}
    updates :: SSignal era val -> DSignal era val
    updates (SSignal _ upd) = upd

    -- * Stateful signals
    {-|
        Accumulates the values of a discrete signal.

        Applying @scan /init/ /fun/@ to a discrete signal replaces its occurence values @/val_1/@,
        @/val_2/@ and so on by the values @/init/ &#x60;/fun/&#x60; /val_1/@, @(/init/
        &#x60;/fun/&#x60; /val_1/) &#x60;/fun/&#x60; /val_2/@ and so on and adds an occurence of
        the value @/init/@ at the beginning.
    -}
    scan :: accu -> (accu -> val-> accu) -> (DSignal era val -> SSignal era accu)
    scan init fun upd = fromInitAndUpdate init (DSignal.scan init fun upd)

    -- * Capsules
    crackCapsules :: SSignal era (Capsule val) -> SSignal era val
    crackCapsules (SSignal (Capsule init) capUpd) = SSignal init (DSignal.crackCapsules capUpd)

    -- * Connectors
    {-|
        Converts an event handler into a segmented signal consumer.

        If a segmented signal is consumed with such a consumer, the handler is called at the
        starting time of the era and at each update with the current value of the signal as its
        argument. If the segmented signal is seen as a discrete signal with an additional occurence
        at the start then @consumer@ behaves analogous to the 'DSignal.consumer' function of
        "FRP.Grapefruit.Signal.Discrete".
    -}
    consumer :: (val -> IO ()) -> Consumer SSignal val
    consumer handler = Consumer $
                       proc (SSignal init upd) -> do
                           putSetup                           -< Setup.fromIO $ handler init
                           consume (DSignal.consumer handler) -< upd

    -- FIXME: Simplify the other consumer and producer docs by documenting function arguments.
    {-|
        Converts a value read action and a change event handler registration into a segmented signal
        producer.
    -}
    producer :: IO val
                -- ^an action reading the current value of the signal
             -> (IO () -> Setup)
                -- ^ an action which registers a given event handler so that it is called everytime
                --   the value of the signal has changed
             -> Producer SSignal val
    producer readVal changeReg = Producer $
                                 proc _ -> do
                                     init <- act                               -< readVal
                                     upd  <- produce (DSignal.producer updReg) -< ()
                                     returnA -< SSignal init upd where

        updReg handler = changeReg (readVal >>= handler)
