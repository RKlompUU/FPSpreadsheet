{-|
    Signals are the key concept of Functional Reactive Programming. They describe behavior over
    time. This module provides general support for signals. Individual kinds of signals are provided
    by the submodules "FRP.Grapefruit.Signal.Disrete", "FRP.Grapefruit.Signal.Segmented" and
    "FRP.Grapefruit.Signal.Continuous".

    A signal type has kind @* -> * -> *@. Its first parameter denotes the time interval in which the
    signal is alive. This is called the /era/ of the signal. An era is left-closed (contains a
    starting time) but right-open or right-unbounded (does not contain an ending time).

    The era type parameter is not intended to be instantiated with concrete types. Instead, it is
    used to force equality of eras or independence of eras at compile time. Its use is very similar
    to that of the first type parameter of 'ST' and the first parameter of 'STRef'.
-}
module FRP.Grapefruit.Signal (

    {-FIXME:
        Unfortunately, it seems that we have to export polySwitch and PolySignalFun here and use
        them in the Switching example, since we might not be able to construct the argument signals
        for switch (which must have impredicative types). This was not the case before GHC 7.
    -}

    -- * Signals
    Signal,

    -- * Switching
    switch,
    polySwitch,

    -- * Signal functions
    SignalFun (OSF, SSF),
    unOSF,
    unSSF,
    sfApp,
    (:->),
    PolySignalFun (PolySignalFun),

    -- * Signal shapes
    Of,

    -- * Sampling
    Sampler,
    Samplee,
    (<#>),
    (#>),
    (<#),

    -- * Connectors
    Consumer (Consumer),
    consume,
    Producer (Producer),
    produce

) where

    -- Control
    import Control.Monad.ST as ST          -- for documentation only

    -- Data
    import Data.STRef as STRef -- for documentation only

    -- Internal
    import Internal.Signal as Signal
