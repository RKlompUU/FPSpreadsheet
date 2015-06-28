{-|
    This module is about discrete signals.

    For a general introduction to signals, see the documentation of "FRP.Grapefruit.Signal".
-}
module FRP.Grapefruit.Signal.Discrete (

    -- * Discrete signal type
    DSignal,

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

    -- * Connectors
    consumer,
    producer

) where

    -- Prelude
    import Prelude ()

    -- Internal
    import Internal.Signal.Discrete as DSignal
