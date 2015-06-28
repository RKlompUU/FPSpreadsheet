module Internal.Signal.Discrete (

    -- * Discrete signal type
    DSignal (DSignal),

    -- * Empty signal
    empty,

    -- * Combination
    -- ** Union
    union,
    unionWith,
    transUnion,

    unions,
    unionsWith,

    -- ** Difference
    difference,
    differenceWith,

    -- ** Intersection
    intersection,
    intersectionWith,

    -- * Mapping and filtering
    map,
    filter,
    catMaybes,
    mapMaybe,

    -- * Stateful signals
    scan,
    scan1,
    stateful,

    -- * Time IDs and capsules
    timeIDApp,
    crackCapsules,

    -- * Connectors
    consumer,
    producer

) where

    -- Prelude
    import Prelude hiding (map, filter)

    -- Control
    import Control.Arrow as Arrow
    import Control.Monad as Monad

    -- Data
    import Data.Monoid    as Monoid
    import Data.Unique    as Unique
    import Data.Map       as Map    (Map) -- for documentation only

    -- Internal
    import                Internal.Signal                  as Signal
    import                Internal.Signal.Discrete.Capsule as Capsule
    import                Internal.Signal.Discrete.Vista   as Vista (Vista)
    import qualified      Internal.Signal.Discrete.Vista   as Vista
    import {-# SOURCE #-} Internal.Signal.Segmented        as SSignal

    -- FRP.Grapefruit
    import FRP.Grapefruit.Setup   as Setup
    import FRP.Grapefruit.Circuit as Circuit

    -- * Discrete signal type
    {-|
        The type of discrete signals.

        A discrete signal is a sequence of values assigned to discrete times. A pair of a time and a
        corresponding value is called an occurrence. You can think of @DSignal /era/ /val/@ as being
        equivalent to @'Map' (Time /era/) /val/@ where @Time /era/@ is the type of all times of
        the given era. However, an occurence at the starting time of the era is not possible. In
        contrast to 'Map', a discrete signal may cover infinitely many values.

        Discrete signals can describe sequences of events. For example, the sequence of all key
        presses could be described by a discrete signal of characters. Discrete signals are also
        used in conjunction with sampling.

        The discrete signal instances of 'Functor' and 'Monoid' provide the following method
        definitions:

        @
        'fmap'    = 'map'
        'mempty'  = 'empty'
        'mappend' = 'union'
        'mconcat' = 'unions'
        @
    -}
    newtype DSignal era val = DSignal (Vista val)

    instance Functor (DSignal era) where

        fmap = map

    instance Monoid (DSignal era val) where

        mempty  = empty

        mappend = union

    instance Signal DSignal where

        osfSwitch (SSignal init upd) = DSignal vista' where

            vista' = Vista.baseSwitch ((vista . unPolyOSF) init) (vista (fmap (vista . unPolyOSF) upd))

        ssfSwitch (SSignal init upd) arg = signalFun' where

            signalFun'  = polySwitch (SSignal.fromInitAndUpdate reducedInit reducedUpd)

            reducedInit = appToVista init (vista arg)

            reducedUpd  = DSignal (Vista.reducedFunUpdate (vista (fmap appToVista upd)) (vista arg))

    instance Sampler DSignal where

        sample = dSample

        samplerMap = fmap

    vista :: DSignal era val -> Vista val
    vista (DSignal vista) = vista

    appToVista :: PolySSF DSignal val shape -> Vista val -> PolySignalFun shape
    appToVista fun vista = PolySignalFun (unPolySSF fun (DSignal vista))

    -- * Empty signal
    -- |A signal with no occurrences.
    empty :: DSignal era val
    empty = DSignal Vista.empty

    -- * Combination
    -- ** Union
    {-|
        Constructs the left-biased union of two discrete signals.

        @union@ is equivalent to @'unionWith' const@.
    -}
    union :: DSignal era val -> DSignal era val -> DSignal era val
    union = unionWith const

    {-|
        Constructs the union of two discrete signals, combining simultaneously occuring values via
        a combining function.

        @unionWith@ is equivalent to @'transUnion' id id@.
    -}
    unionWith :: (val -> val -> val) -> (DSignal era val -> DSignal era val -> DSignal era val)
    unionWith = transUnion id id

    {-|
        Union with conversion and combination.

        At each time, a signal @/dSignal1/@ or a signal @/dSignal2/@ has an occurence, the signal

        @
        transUnion /conv1/ /conv2/ /comb/ /dSignal1/ /dSignal2/
        @

        has an occurence, too. The value of this occurence is formed as follows:

        [@/conv1/ /val1/@]
            if @/dSignal1/@ has an occurence of value @/val1/@ and @/dSignal2/@ has no occurence

        [@/conv2/ /val2/@]
            if @/dSignal2/@ has an occurence of value @/val2/@ and @/dSignal1/@ has no occurence

        [@/comb/ /val1/ /val2/@]
            if @/dSignal1/@ has an occurence of value @/val1/@ and @/dSignal2/@ has an occurence of
            value @/val2/@
    -}
    transUnion :: (val1 -> val')
               -> (val2 -> val')
               -> (val1 -> val2 -> val')
               -> (DSignal era val1 -> DSignal era val2 -> DSignal era val')
    transUnion conv1 conv2 comb (DSignal vista1) (DSignal vista2) = DSignal vista' where

        vista' = Vista.transUnion conv1 conv2 comb vista1 vista2

    {-|
        Repeated left-biased union.

        @unions@ is equivalent to @foldl 'union' 'empty'@ and @'unionsWith' const@.
    -}
    unions :: [DSignal era val] -> DSignal era val
    unions = foldl union empty

    {-|
        Repeated union with a combining function.

        @unionsWith /comb/@ is equivalent to @foldl ('unionWith' /comb/) 'empty'@.
    -}
    unionsWith :: (val -> val -> val) -> [DSignal era val] -> DSignal era val
    unionsWith comb = foldl (unionWith comb) empty

    -- ** Difference
    {-|
        Constructs the difference of two discrete signals.

        @difference@ is equivalent to @'differenceWith' (\\_ _ -> Nothing)@.
    -}
    difference :: DSignal era val1 -> DSignal era val2 -> DSignal era val1
    difference = differenceWith (const (const Nothing))

    {-|
        Constructs a kind of difference of two discrete signals where occurences may be modified
        instead of being dropped.

        At each time, a signal @/dSignal1/@ has an occurence of a value @/val1/@, the signal
        @differenceWith /comb/ /dSignal1/ /dSignal/@ has

        [an occurence of @/val1/@]
            if @/dSignal2/@ has no occurence

        [an occurence of @/val'/@]
            if @/dSignal2/@ has an occurence of a value @/val2/@ and @/comb/ /val1/ /val2/ = Just
            /val'/@

        [no occurence]
            if @/dSignal2/@ has an occurence of a value @/val2/@ and @/comb/ /val1/ /val2/ =
            Nothing@
    -}
    differenceWith :: (val1 -> val2 -> Maybe val1)
                   -> (DSignal era val1 -> DSignal era val2 -> DSignal era val1)
    differenceWith comb = (catMaybes .) . transUnion Just (const Nothing) comb

    -- ** Intersection
    {-|
        Constructs the left-biased intersection of two discrete signals.

        @intersection@ is equivalent to @'intersectionWith' const@.
    -}
    intersection :: DSignal era val1 -> DSignal era val2 -> DSignal era val1
    intersection = intersectionWith const

    {-|
        Constructs the intersection of two discrete signals, combining values via a combining
        function.
    -}
    intersectionWith :: (val1 -> val2 -> val')
                     -> (DSignal era val1 -> DSignal era val2 -> DSignal era val')
    intersectionWith comb = (catMaybes .) .
                            transUnion (const Nothing) (const Nothing) ((Just .) . comb)

    -- * Mapping and filtering
    {-|
        Converts each value occuring in a discrete signal by applying a function to it.
    -}
    map :: (val -> val') -> (DSignal era val -> DSignal era val')
    map fun = mapMaybe (Just . fun)

    {-|
        Drops all occurence of a discrete signal whose values do not fulfill a given predicate.
    -}
    filter :: (val -> Bool) -> (DSignal era val -> DSignal era val)
    filter prd = mapMaybe (\val -> if prd val then Just val else Nothing)

    {-|
        Converts all occurences with values of the form @Just /val/@ into occurences with value
        @/val/@ and drops all occurences with value @Nothing@.
    -}
    catMaybes :: DSignal era (Maybe val) -> DSignal era val
    catMaybes = mapMaybe id

    {-|
        The combination of 'map' and 'catMaybes'.

        @mapMaybe /fun/@ is equivalent to @'catMaybes' . 'map' /fun/@.
    -}
    mapMaybe :: (val -> Maybe val') -> (DSignal era val -> DSignal era val')
    mapMaybe fun (DSignal vista) = DSignal $ Vista.mapMaybe fun vista

    -- * Stateful signals
    {-|
        Accumulates the values of a discrete signal, starting with a given initial value.

        Applying @scan /init/ /fun/@ to a discrete signal replaces its occurence values @/val_1/@,
        @/val_2/@ and so on by the values @/init/ &#x60;/fun/&#x60; /val_1/@, @(/init/
        &#x60;/fun/&#x60; /val_1/) &#x60;/fun/&#x60; /val_2/@ and so on.
    -}
    scan :: accu -> (accu -> val -> accu) -> (DSignal era val -> DSignal era accu)
    scan initAccu trans = stateful initAccu .
                          fmap (\val currentAccu -> join (,) (trans currentAccu val))

    {-|
        Accumulates the values of a discrete signal, starting with the first occuring value.

        Applying @scan1 /init/ /fun/@ to a discrete signal replaces its occurence values @/val_1/@,
        @/val_2/@, @/val_3/@ and so on by the values @/val_1/@, @/val_1/ &#x60;/fun/&#x60; /val_2/@,
        @(/val_1/ &#x60;/fun/&#x60; /val_2/) &#x60;/fun/&#x60; /val_3/@ and so on.
    -}
    scan1 :: (val -> val -> val) -> (DSignal era val -> DSignal era val)
    scan1 trans = stateful Nothing . fmap statefulTrans where

        statefulTrans val currentAccu = let

                                            nextAccu = maybe val (flip trans val) currentAccu

                                        in (nextAccu,Just nextAccu)

    {-|
        Constructs a discrete signal by repeatedly applying state transformers.

        Applying @stateful /init/@ to a discrete signal replaces its occurence values @/trans_1/@,
        @/trans_2/@, @/trans_3/@ and so on by the values @fst . /trans_1/ $ /init/@, @fst .
        /trans_2/ $ snd . /trans_1/ $ /init/@, @fst . /trans_3/ $ snd . /trans_2/ $ snd . /trans_1/
        $ /init/@ and so on.
    -}
    stateful :: state -> DSignal era (state -> (val,state)) -> DSignal era val
    stateful initState (DSignal transVista) = DSignal $ Vista.stateful initState transVista

    -- * Time IDs and capsules
    timeIDApp :: DSignal era (Unique -> val) -> DSignal era val
    timeIDApp (DSignal vista) = DSignal $ Vista.timeIDApp vista

    crackCapsules :: DSignal era (Capsule val) -> DSignal era val
    crackCapsules (DSignal vista ) = DSignal $ Vista.crackCapsules vista

    -- * Connectors
    {-|
        Converts an event handler into a discrete signal consumer.

        If a discrete signal is consumed with such a consumer, the handler is called at each
        occurence with the occuring value as its argument.
    -}
    consumer :: (val -> IO ()) -> Consumer DSignal val
    consumer handler = Consumer (arr vista >>> Vista.consumer handler)

    {-|
        Converts an event handler registration into a discrete signal producer.

        Applying the argument of @producer@ to an event handler has to yield a setup which makes the
        handler be called with a certain value everytime the produced signal shall have an
        occurence of this value.
    -}
    producer :: ((val -> IO ()) -> Setup) -> Producer DSignal val
    producer reg = Producer $ Vista.producer reg >>> arr DSignal
