module Internal.Signal (

    -- * Signals
    Signal (osfSwitch, ssfSwitch),

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
    PolyOSF (PolyOSF),
    PolySSF (PolySSF),
    polyOSF,
    polySSF,
    unPolyOSF,
    unPolySSF,

    -- * Signal shapes
    Of,

    -- * Sampling
    Sampler (sample, samplerMap),
    Samplee (dSample, sSample),
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
    import Control.Applicative as Applicative -- for documentation only
    import Control.Monad.ST    as ST          -- for documentation only


    -- Internal
    import {-# SOURCE #-} Internal.Signal.Discrete  as DSignal
    import {-# SOURCE #-} Internal.Signal.Segmented as SSignal

    -- FRP.Grapefruit
    import FRP.Grapefruit.Circuit as Circuit

    -- Fixities
    infixl 4 <#>
    infixl 4 #>
    infixl 4 <#

    -- * Signals
    -- |The class of all signal types.
    class Signal signal where

        osfSwitch :: SSignal era (PolyOSF signal val) ->
                     signal era val

        ssfSwitch :: SSignal era (PolySSF signal val shape) ->
                     (signal era val -> SignalFun era shape)

    -- * Switching
    {-|
        This function generates a signal whose behavior switches between that of different other
        signals over time.

        Since the result type @'SignalFun' era shape@ is isomorphic to an n-ary function type, we
        can see @switch@ as a function which takes a first argument, called the function signal,
        and /n/ further arguments, called the argument signals, and yields a signal, called the
        result signal.

        The result signal is composed of different sections. There is one section for each segment
        of the function signal. Such a section is formed as follows: For each argument signal, the
        part which corresponds to the time intervall of the functions signal&#x2019;s segment is cut
        out of the argument signal. The value of the function signal is applied to the resulting /n/
        signal parts. The result of this application is the desired section of the result signal.

        The signal functions which are applied to the parts of the argument signals use an
        universally quantified era parameter. This ensures that the results of these functions do
        not depend on signals from the outside but only on the parts of the argument signals. This
        is important since operations on signals require that their argument and result signals are
        of the same era. The usage of universial quantification in the type of @switch@
        corresponds to the usage of rank 2 polymorphism in the type of 'runST'.
    -}
    switch :: SSignal era (forall era'. SignalFun era' shape) -> SignalFun era shape
    switch = polySwitch . fmap PolySignalFun

    polySwitch :: SSignal era (PolySignalFun shape) -> SignalFun era shape
    polySwitch funSignal@(SSignal (PolySignalFun init) _) = fun' where

        fun' = case init of
                   OSF _ -> OSF $ osfSwitch (fmap polyOSF funSignal)
                   SSF _ -> SSF $ ssfSwitch (fmap polySSF funSignal)

    -- * Signal functions
    -- FIXME: Hyperlink to :-> and document the data constructors seperately as soon as this works.
    {-|
        A signal function is a function which maps a certain number of signals to one signal whereby
        all argument signals and the result signal have the same era.

        The @era@ parameter of @SignalFun@ denotes the era of all argument signals and the result
        signal. The @shape@ parameter is a phantom parameter which specifies the number of argument
        signals as well as the types of the argument signals and the result signal without their era
        parameters. It has the following form:

        @
        /signal_1/ &#x60;'Of'&#x60; /val_1/ :-> ... :-> /signal_n/ &#x60;'Of'&#x60; /val_n/ :-> /signal'/ &#x60;'Of'&#x60; /val'/
        @

        The data constructors 'OSF' and 'SSF' construct signal functions of zero and non-zero arity,
        respectively. (The @O@ stands for &#x201C;zero&#x201D; and the @S@ stands for
        &#x201C;successor&#x201D;.) A signal function is typically formed by an expression like

        @
        'SSF' $ &#x5C;/signal_1/ ->
        ...
        'SSF' $ &#x5C;/signal_n/ ->
        'OSF' $ /signal'/
        @

        where @/signal'/@ is an expression that might use @/signal_1/@ to @/signal_n/@. Signal
        functions are usually applied like this:

        @
        'unOSF' $ /signalFun/ &#x60;'sfApp'&#x60; /signal_1/ &#x60;'sfApp'&#x60; ... &#x60;'sfApp'&#x60; /signal_n/
        @
    -}
    data SignalFun era shape where

        OSF :: (Signal signal) =>
               signal era val -> SignalFun era (signal `Of` val)

        SSF :: (Signal signal) =>
               (signal era val -> SignalFun era shape) -> SignalFun era (signal `Of` val :-> shape)

    -- |Converts a nullary signal function into its corresponding signal.
    unOSF :: SignalFun era (signal `Of` val) -> signal era val
    unOSF (OSF signal) = signal

    -- |Converts a signal function of non-zero arity into a true function.
    unSSF :: SignalFun era (signal `Of` val :-> shape) -> (signal era val -> SignalFun era shape)
    unSSF (SSF fun) = fun

    infixl 4 `sfApp`
    {-|
        Applies a signal function to a signal.

        @sfApp@ is equivalent to 'unSSF'.
    -}
    sfApp :: SignalFun era (signal `Of` val :-> shape) -> signal era val -> SignalFun era shape
    sfApp = unSSF

    infixr 1 :->
    {-|
        The @:->@ operator is used to form signal function shapes for 'SignalFun'. The shape
        @/argShape/ :-> /resultShape/@ stands for functions which map signals of shape @/argShape/@
        to signal functions of shape @/resultShape/@.
    -}
    data argShape :-> resultShape

    newtype PolySignalFun shape = PolySignalFun (forall era. SignalFun era shape)

    newtype PolyOSF signal val = PolyOSF (forall era. signal era val)

    newtype PolySSF signal val shape = PolySSF (forall era. signal era val -> SignalFun era shape)

    polyOSF :: PolySignalFun (signal `Of` val) -> PolyOSF signal val
    polyOSF (PolySignalFun signalFun) = PolyOSF (unOSF signalFun)

    polySSF :: PolySignalFun (signal `Of` val :-> shape) -> PolySSF signal val shape
    polySSF (PolySignalFun signalFun) = PolySSF (unSSF signalFun)

    unPolyOSF :: PolyOSF signal val -> signal era val
    unPolyOSF (PolyOSF signal) = signal

    unPolySSF :: PolySSF signal val shape -> signal era val -> SignalFun era shape
    unPolySSF (PolySSF fun) = fun

    -- * Signal shapes
    -- FIXME: Make :-> a hyperlink when this works.
    {-|
        @Of@ is used to form signal shapes. Signal shapes are used as phantom types and denote a
        signal type except its era parameter.

        A signal shape @/signal/ &#x60;Of&#x60; /val/@ stands for a signal of type @/signal/ /era/
        /val/@ where the era parameter is provided by an external source. Signal shapes are used as
        signal function shapes of nullary functions and as argument shapes for @:->@. In this case,
        the era parameter is the era parameter of 'SignalFun'. Signal shapes are also used in
        records as defined by the module @FRP.Grapefruit.Record@ of package grapefruit-records.
    -}
    data (signal :: * -> * -> *) `Of` val

    -- * Sampling
    {-|
        The class of all signals which can be seen as discrete sequences of values. Such signals can
        be used to sample signals of class 'Samplee'.
    -}
    class Sampler sampler where

        sample :: (Samplee samplee) =>
                  sampler era (val -> val') -> samplee era val -> sampler era val'

        -- for internal use only
        samplerMap :: (val -> val') -> (sampler era val -> sampler era val')

    -- Samplee could also be called “dense signal”.
    {-|
        The class of all signals which assign a value to each time of their era. Such signals can be
        sampled by signals of class 'Sampler'.
    -}
    class Samplee samplee where

        -- for internal use only
        dSample :: DSignal era (val -> val') -> samplee era val -> DSignal era val'

        -- for internal use only
        sSample :: SSignal era (val -> val') -> samplee era val -> SSignal era val'

    {-|
        Sampling of signals.

        A signal @/sampler/ &#x3C;#> /samplee/@ has a value at each time where @/sampler/@ has a
        value. The value of @/sampler/ &#x3C;#> /samplee/@ is formed by applying the value of
        @/sampler/@ to the value, @/samplee/@ has at this time.

        This function has similarities with '<*>'.
    -}
    (<#>) :: (Sampler sampler, Samplee samplee) =>
             sampler era (val -> val') -> samplee era val -> sampler era val'
    (<#>) = sample

    {-|
        Sampling of signals where the values of the sampler are ignored.

        The following equation holds:

        @
        /sampler/ &#x23;> /samplee/ = id '<$' /sampler/ '<#>' /samplee/
        @

        This function has similarities with '*>'.
    -}
    (#>) :: (Sampler sampler, Samplee samplee) =>
            sampler era dummy -> samplee era val -> sampler era val
    (#>) = (<#>) . samplerMap (const id)

    {-|
        Sampling of signals where the values of the samplee are ignored.

        The following equation holds:

        @
        /sampler/ &#x3C;&#x23; /samplee/ = const '<$>' /sampler/ '<#>' /samplee/
        @

        This function has similarities with '<*'.
    -}
    (<#) :: (Sampler sampler, Samplee samplee) =>
            sampler era val -> samplee era dummy -> sampler era val
    (<#) = (<#>) . samplerMap const

    -- * Connectors
    -- |A consumer says what to do with a given signal.
    newtype Consumer signal val = Consumer (forall era. Circuit era (signal era val) ())
                                  -- ^A consumer, represented by a circuit that consumes a signal.

    -- |Yields a circuit which consumes a signal.
    consume :: Consumer signal val -> Circuit era (signal era val) ()
    consume (Consumer circuit) = circuit

    -- |A producer says how to produce a certain signal.
    newtype Producer signal val = Producer (forall era. Circuit era () (signal era val))
                                  -- ^A producer, represented by a circuit that produces a signal.

    -- |Yields a circuit which produces a signal.
    produce :: Producer signal val -> Circuit era () (signal era val)
    produce (Producer circuit) = circuit
