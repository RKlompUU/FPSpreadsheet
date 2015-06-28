module Internal.Signal.Discrete (

    -- * Discrete signal type
    DSignal

) where

    -- * Discrete signal type
#if MIN_VERSION_base(4,7,0)
    -- GHC >= 7.8
    type role DSignal phantom representational
#endif
    data DSignal era val
