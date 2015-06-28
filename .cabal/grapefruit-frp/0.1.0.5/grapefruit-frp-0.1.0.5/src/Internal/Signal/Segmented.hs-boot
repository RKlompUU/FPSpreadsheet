module Internal.Signal.Segmented (

    -- * Segmented signal type
    SSignal (SSignal),

    -- * Introduction
    fromInitAndUpdate

) where

    -- Internal
    import {-# SOURCE #-} Internal.Signal.Discrete as DSignal

    -- * Segmented signal type
#if MIN_VERSION_base(4,7,0)
    -- GHC >= 7.8
    type role SSignal phantom representational
#endif
    data SSignal era val = SSignal val (DSignal era val)

    instance Functor (SSignal era)

    -- * Introduction
    fromInitAndUpdate :: val -> DSignal era val -> SSignal era val
