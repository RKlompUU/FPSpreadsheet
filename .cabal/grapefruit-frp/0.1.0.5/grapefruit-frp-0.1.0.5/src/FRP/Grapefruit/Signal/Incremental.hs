module FRP.Grapefruit.Signal.Incremental (

    -- * Incremental signal type
    ISignal,
    Incremental (type Diff, patch, type ValidationState, validationInit, validationStep),
    Diff (Replacement),

    -- * Monolithic values
    Monolithic (Monolithic),

    -- * Construction
    construct,

    -- * Queries
    withInit,
    updates,

    -- * Conversion
    toSSignal,
    monolithicFromSSignal,
    monolithicToSSignal,

    -- * Composition
    const,
    map,
    combine,

    -- * Connectors
    consumer

) where

    -- Prelude
    import Prelude hiding (const, map)

    -- Data
    import Data.Semigroup as Semigroup

    -- Internal
    import Internal.Signal.Segmented as SSignal (SSignal (SSignal), scan)

    -- FRP.Grapefruit
    import           FRP.Grapefruit.Setup           as Setup
    import           FRP.Grapefruit.Circuit         as Circuit
    import           FRP.Grapefruit.Signal          as Signal
    import           FRP.Grapefruit.Signal.Discrete as DSignal hiding (map, consumer)
    import qualified FRP.Grapefruit.Signal.Discrete as DSignal

    {-
        Since an ISignal may be derived from an SSignal, it can indirectly depend on a CSignal. So
        access to the initial value must be forbidden for the same reason it is forbidden for
        SSignals.
    -}

    -- #> should also work for ISignals.

    -- * Incremental signal type
    data ISignal era val = ISignal val (DSignal era (Diff val))

    class (Semigroup (Diff val)) => Incremental val where

        data Diff val :: *

        patch :: val -> Diff val -> val

        type ValidationState val :: *

        validationInit :: val -> ValidationState val

        validationStep :: Diff val -> ValidationState val -> Maybe (ValidationState val)

    -- * Monolithic values
    newtype Monolithic val = Monolithic val

    instance Semigroup (Diff (Monolithic val)) where

        _ <> monolithic2 = monolithic2

    instance Incremental (Monolithic val) where

        data Diff (Monolithic val) = Replacement val

        patch _ (Replacement val) = Monolithic val

        type ValidationState (Monolithic val) = ()

        validationInit _ = ()

        validationStep _ _ = Just ()

    -- * Construction
    construct :: (Incremental val) => val -> DSignal era (Diff val) -> ISignal era val
    construct init diffs = ISignal init (DSignal.stateful (validationInit init)
                                                          (fmap diffToTrans diffs)) where

        diffToTrans diff state = case validationStep diff state of
                                     Nothing     -> error $ "grapefruit-frp: " ++
                                                            "incremental signal validation failure"
                                     Just state' -> (diff,state')

    -- * Queries
    withInit :: (Signal signal) => ISignal era val -> (val -> signal era val') -> signal era val'
    withInit (ISignal init _) cont = cont init

    updates :: ISignal era val -> DSignal era (Diff val)
    updates (ISignal _ upd) = upd

    -- * Conversion
    toSSignal :: (Incremental val) => ISignal era val -> SSignal era val
    toSSignal (ISignal init upd) = SSignal.scan init patch upd

    monolithicFromSSignal :: SSignal era val -> ISignal era (Monolithic val)
    monolithicFromSSignal (SSignal init upd) = ISignal (Monolithic init) (fmap Replacement upd)

    monolithicToSSignal :: ISignal era (Monolithic val) -> SSignal era val
    monolithicToSSignal = fmap (\(Monolithic val) -> val) . toSSignal

    -- * Composition
    -- analogous to pure
    const :: (Incremental val) => val -> ISignal era val
    const val = ISignal val DSignal.empty

    -- analogous to fmap
    map :: (Incremental val, Incremental val')
        => (val -> (val',state))
        -> (Diff val -> state -> (Diff val',state))
        -> (ISignal era val -> ISignal era val')
    map start step (ISignal init upd) = ISignal init' upd' where

        (init',initState) = start init

        upd'              = DSignal.stateful initState (fmap step upd)

    -- analogous to liftA2
    combine :: (Incremental val1, Incremental val2, Incremental val')
            => (val1 -> val2 -> (val',state))
            -> (Diff val1 -> state -> (Diff val',state))
            -> (Diff val2 -> state -> (Diff val',state))
            -> (ISignal era val1 -> ISignal era val2 -> ISignal era val')
    combine start step1 step2 (ISignal init1 upd1) (ISignal init2 upd2) = ISignal init' upd' where

        (init',initState)                = start init1 init2

        upd'                             = DSignal.stateful initState (unionWith transCombine
                                                                                 (fmap step1 upd1)
                                                                                 (fmap step2 upd2))
        transCombine trans1 trans2 state = let

                                               (diff1',state')  = trans1 state

                                               (diff2',state'') = trans2 state'

                                           in (diff1' <> diff2',state'')

    -- * Connectors
    consumer :: (val -> IO ()) -> (Diff val -> IO ()) -> Consumer ISignal val
    consumer initHdlr updHdlr = Consumer $
                                proc (ISignal init upd) -> do
                                    putSetup                           -< Setup.fromIO $
                                                                          initHdlr init
                                    consume (DSignal.consumer updHdlr) -< upd
