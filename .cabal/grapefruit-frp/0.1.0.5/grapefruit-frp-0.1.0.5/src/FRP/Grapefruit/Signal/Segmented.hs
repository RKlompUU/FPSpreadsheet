{-|
    This module is about segmented signals.

    For a general introduction to signals, see the documentation of "FRP.Grapefruit.Signal".
-}
module FRP.Grapefruit.Signal.Segmented (

    -- * Segmented signal type
    SSignal,

    -- * Construction
    construct,
    fromInitAndUpdate,

    -- * Queries
    withInit,
    updates,

    -- * Stateful signals
    scan,

    -- * Connectors
    consumer,
    producer

) where

    -- Internal
    import Internal.Signal.Segmented as Segmented
