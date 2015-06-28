module Internal.Signal.Discrete.Vista (

    Vista,
    empty,
    transUnion,
    stateful,
    mapMaybe,
    baseSwitch,
    reducedFunUpdate,
    timeIDApp,
    crackCapsules,
    consumer,
    producer

) where

    -- Prelude
    import Prelude hiding (filter)

    -- Control
    import Control.Arrow           as Arrow
    import Control.Monad           as Monad
    import Control.Concurrent.Chan as Chan

    -- Data
    import           Data.Function  as Function
    import           Data.Maybe     as Maybe hiding (mapMaybe)
    import           Data.Map (Map)
    import qualified Data.Map       as Map
    import           Data.IORef     as IORef
    import           Data.Unique    as Unique

    -- System
    import System.IO.Unsafe as UnsafeIO

    -- Internal
    import           Internal.Signal.Discrete.ListenerSet as ListenerSet (ListenerSet)
    import qualified Internal.Signal.Discrete.ListenerSet as ListenerSet
    import           Internal.Signal.Discrete.Capsule     as Capsule
    import           Internal.Circuit                     as Circuit

    -- FRP.Grapefruit
    import FRP.Grapefruit.Setup   as Setup
    import FRP.Grapefruit.Circuit as Circuit

    {- FIXME:

        This implementation is a bit inefficient because there is a complete re-registration after
        every event the sink listens to. We can make it more efficient by including two additional
        fields into Variant describing the difference between the old and the new set of discrete
        sources: one set covering the added sources and one covering the removed sources.

    -}

    newtype Vista val = Vista (VistaMap val)

    type VistaMap val = Map DSource (Variant val)

    data DSource = DSource Unique (IORef ListenerSet)

    instance Eq DSource where

        DSource id1 _ == DSource id2 _ = id1 == id2

    instance Ord DSource where

        compare (DSource id1 _) (DSource id2 _) = compare id1 id2

    data Variant val = Variant Unique (Maybe val) (Vista val)

    empty :: Vista val
    empty = Vista Map.empty

    mapTransUnion :: (Ord key)
                  => (val1 -> val')
                  -> (val2 -> val')
                  -> (val1 -> val2 -> val')
                  -> (Map key val1 -> Map key val2 -> Map key val')
    mapTransUnion conv1 conv2 comb map1 map2 = map' where

        map'            = convMap1 `Map.union` convMap2 `Map.union` intersectionMap

        convMap1        = Map.map conv1 (map1 `Map.difference` intersectionMap)

        convMap2        = Map.map conv2 (map2 `Map.difference` intersectionMap)

        intersectionMap = Map.intersectionWith comb map1 map2

    {-
        Maybe it’s better if unionWith isn’t implemented on top of transUnion. Consider the case
        that we merge many signals whose discrete source sets don’t overlap. transUnion applies id
        linearily many times to the values while a directly implemented unionWith wouldn’t do so.
    -}
    transUnion :: (val1 -> val')
               -> (val2 -> val')
               -> (val1 -> val2 -> val')
               -> (Vista val1 -> Vista val2 -> Vista val')
    transUnion conv1 conv2 comb vista1@(Vista map1) vista2@(Vista map2) = Vista map' where

        map'                                        = mapTransUnion variantConv1
                                                                    variantConv2
                                                                    variantComb
                                                                    map1
                                                                    map2

        variantConv1 (Variant timeID1 maybeVal1 nextVista1) = Variant timeID1
                                                                      (fmap conv1 maybeVal1)
                                                                      (this nextVista1 vista2)

        variantConv2 (Variant timeID2 maybeVal2 nextVista2) = Variant timeID2
                                                                      (fmap conv2 maybeVal2)
                                                                      (this vista1 nextVista2)

        variantComb (Variant timeID1 maybeVal1 nextVista1)
                    (Variant timeID2 maybeVal2 nextVista2)  = Variant timeID1
                                                                      (maybeComb maybeVal1
                                                                                 maybeVal2)
                                                                      (this nextVista1 nextVista2)

        maybeComb Nothing     Nothing                       = Nothing
        maybeComb Nothing     (Just val2)                   = Just (conv2 val2)
        maybeComb (Just val1) Nothing                       = Just (conv1 val1)
        maybeComb (Just val1) (Just val2)                   = Just (comb val1 val2)

        this                                                = transUnion conv1 conv2 comb

    stateful :: state -> Vista (state -> (val',state)) -> Vista val'
    stateful initState (Vista transMap) = Vista $ Map.map variantConv transMap where

        variantConv (Variant timeID Nothing      nextVista) = Variant timeID
                                                                      Nothing
                                                                      (stateful initState nextVista)
        variantConv (Variant timeID (Just trans) nextVista) = let

                                                                  (val',nextState) = trans initState

                                                              in Variant timeID
                                                                         (Just val')
                                                                         (stateful nextState
                                                                                   nextVista)

    mapMaybe :: (val -> Maybe val') -> (Vista val -> Vista val')
    mapMaybe fun (Vista map) = Vista (Map.map variantConv map) where

        variantConv (Variant timeID maybeVal nextVista) = Variant timeID
                                                                  (maybeVal >>= fun)
                                                                  (mapMaybe fun nextVista)

    baseSwitch :: Vista val -> Vista (Vista val) -> Vista val
    baseSwitch valVista@(Vista valMap) switchVista@(Vista switchMap) = Vista map' where

        map'                                    = mapTransUnion valConv
                                                                switchConv
                                                                comb
                                                                valMap
                                                                switchMap

        valConv    (Variant valTimeID
                            maybeVal
                            nextValVista)       = Variant valTimeID
                                                          maybeVal
                                                          (baseSwitch nextValVista switchVista)

        switchConv (Variant switchTimeID
                            Nothing
                            nextSwitchVista)    = Variant switchTimeID
                                                          Nothing
                                                          (baseSwitch valVista nextSwitchVista)
        switchConv (Variant switchTimeID
                            (Just nextValVista)
                            nextSwitchVista)    = Variant switchTimeID
                                                          Nothing
                                                          (baseSwitch nextValVista nextSwitchVista)

        comb       (Variant valTimeID
                            maybeVal
                            nextValVista)
                   (Variant switchTimeID
                            Nothing
                            nextSwitchVista)    = Variant valTimeID
                                                          maybeVal
                                                          (baseSwitch nextValVista nextSwitchVista)
        comb       (Variant valTimeID
                            maybeVal
                            _)
                   (Variant _
                            (Just nextValVista)
                            nextSwitchVista)    = Variant valTimeID
                                                          maybeVal
                                                          (baseSwitch nextValVista nextSwitchVista)

    reducedFunUpdate :: Vista (Vista val -> fun) -> Vista val -> Vista fun
    reducedFunUpdate funUpdVista@(Vista funUpdMap)
                     argVista@(Vista argMap)       = Vista $ reducedMap funUpdMap argMap where

        reducedMap                                       = mapTransUnion funUpdConv argConv comb

        funUpdConv (Variant funTimeID
                            maybeFunUpd
                            nextFunUpdVista) = Variant funTimeID
                                                       (fmap ($ argVista) maybeFunUpd)
                                                       (reducedFunUpdate nextFunUpdVista argVista)

        argConv    (Variant argTimeID
                            maybeArg
                            nextArgVista)    = Variant argTimeID
                                                       Nothing
                                                       (reducedFunUpdate funUpdVista nextArgVista)

        comb       (Variant funTimeID
                            maybeFunUpd
                            nextFunUpdVista)
                   (Variant argTimeID
                            _
                            nextArgVista)    = Variant funTimeID
                                                       (fmap ($ nextArgVista) maybeFunUpd)
                                                       (reducedFunUpdate nextFunUpdVista
                                                                         nextArgVista)

    timeIDApp :: Vista (Unique -> val) -> Vista val
    timeIDApp (Vista map) = Vista $ Map.map variantConv map where

        variantConv (Variant timeID maybeFun nextVista) = Variant timeID
                                                                  (fmap ($ timeID) maybeFun)
                                                                  (timeIDApp nextVista)

    -- Reducing the resulting Variant means reducing the capsule.
    crackCapsules :: Vista (Capsule val) -> Vista val
    crackCapsules (Vista map) = Vista $ Map.map variantConv map where

        variantConv (Variant timeID
                             Nothing
                             nextVista)           = Variant timeID
                                                            Nothing
                                                            (crackCapsules nextVista)
        variantConv (Variant timeID
                             (Just (Capsule val))
                             nextVista)           = Variant timeID
                                                            (Just val)
                                                            (crackCapsules nextVista)

    consumer :: (val -> IO ()) -> Circuit era (Vista val) ()
    consumer handler = proc vista -> putSetup -< setup $
                                                 do
                                                     unregRef <- newIORef undefined
                                                     setDSourceSet handler unregRef vista
                                                     return $ join (readIORef unregRef)

    setDSourceSet :: (val -> IO ()) -> IORef (IO ()) -> Vista val -> IO ()
    setDSourceSet handler unregRef (Vista map) = do
                                                     unreg <- mapM (uncurry sourceReg)
                                                                   (Map.assocs map)
                                                     writeIORef unregRef (sequence_ unreg) where

        sourceReg (DSource _ listenersRef) variant = ListenerSet.add listenersRef (handle variant)

        handle (Variant _ maybeVal nextVista)      = do
                                                         when (isJust maybeVal)
                                                              (handler (fromJust maybeVal))
                                                         join (readIORef unregRef)
                                                         setDSourceSet handler unregRef nextVista

    producer :: ((val -> IO ()) -> Setup) -> Circuit era () (Vista val)
    producer register = proc _ -> do
                            sourceID <- act -< newUnique
                            listenersRef <- act -< newIORef ListenerSet.empty
                            timeIDs <- act -< fix (unsafeInterleaveIO . liftM2 (:) newUnique)
                            chan <- act -< newChan
                            vals <- act -< getChanContents chan
                            ecFinalization <- getECFinalization -< ()
                            putSetup -< register $ \val -> do
                                                               writeChan chan val
                                                               listeners <- readIORef listenersRef
                                                               ListenerSet.notify listeners
                                                               ecFinalization
                            returnA -< sourceVista (DSource sourceID listenersRef) timeIDs vals

    sourceVista :: DSource -> [Unique] -> [val] -> Vista val
    sourceVista source timeIDs vals = Vista $
                                      Map.singleton source (sourceVariant source timeIDs vals)

    sourceVariant :: DSource -> [Unique] -> [val] -> Variant val
    sourceVariant source (timeID : nextTimeIDs) (val : nextVals) = variant where

        variant = Variant timeID (Just val) (sourceVista source nextTimeIDs nextVals)
