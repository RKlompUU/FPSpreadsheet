module Internal.UICircuit (

    UICircuit (UICircuit),
    fromCircuit,
    run

) where

    -- Prelude
    import Prelude hiding ((.))

    -- Control
    import           Control.Category                 as Category hiding (id)
    import qualified Control.Category                 as Category
    import           Control.Arrow                    as Arrow
    import           Control.Arrow.Transformer.Reader as ReaderArrow
    import           Control.Concurrent.MVar          as MVar

    -- FRP.Grapefruit
    import FRP.Grapefruit.Circuit         as Circuit
    import FRP.Grapefruit.Signal          as Signal
    import FRP.Grapefruit.Signal.Discrete as DSignal

    -- Internal
    import {-# SOURCE #-} Internal.UIItem as UIItem

    -- Graphics.UI.Grapefruit
    import Graphics.UI.Grapefruit.Backend as UIBackend
    import Graphics.UI.Grapefruit.Comp    as UIComp

    {-|
        The type of user interface circuits.

        The @item@ parameter is a phantom parameter which says which kind of items the circuit
        contains. It should be an instance of 'Item'.
    -}
    newtype UICircuit item uiBackend era i o = UICircuit (ReaderArrow (Placement item uiBackend)
                                                                      (Circuit era)
                                                                      i
                                                                      o)

    -- manual deriving because of GHC bug #1133
    instance Category (UICircuit item uiBackend era) where

        id = UICircuit Category.id

        UICircuit arrow1 . UICircuit arrow2 = UICircuit (arrow1 . arrow2)

    instance Arrow (UICircuit item uiBackend era) where

        arr fun = UICircuit (arr fun)

        first (UICircuit arrow) = UICircuit (first arrow)

        second (UICircuit arrow) = UICircuit (second arrow)

        UICircuit arrow1 *** UICircuit arrow2 = UICircuit (arrow1 *** arrow2)

        UICircuit arrow1 &&& UICircuit arrow2 = UICircuit (arrow1 &&& arrow2)

    instance ArrowLoop (UICircuit item uiBackend era) where

        loop (UICircuit arrow) = UICircuit (Arrow.loop arrow)

    -- “really manual” instance (not just because of manual deriving)
    instance UIComp UICircuit where

        circuit |>> uiCircuit = fromCircuit circuit >>> uiCircuit

        uiCircuit >>| circuit = uiCircuit >>> fromCircuit circuit

        loop = Arrow.loop

        toUICircuit = id

        fromUIItem (UIItem uiCircuit) = uiCircuit

    -- |Converts an ordinary circuit into a user interface circuit that contains no items.
    --fromCircuit :: Circuit era i o -> UICircuit item uiBackend era i o
    fromCircuit = liftReader >>> UICircuit

    {-|
        Runs a user interface circuit.

        @run@ quits when the output signal of the circuit has a first occurence. The universal
        quantification of the circuit&#x2019;s era parameter ensures that the circuit does not use
        signals which are produced outside the circuit and therefore avoids era mismatches.
    -}
    run :: (UIBackend uiBackend)
        => uiBackend
           -- ^the user interface backend to use
        -> (forall era. UICircuit Window uiBackend era i (DSignal era o))
           -- ^the circuit to run
        -> i
           -- ^the input of the ciruit
        -> IO o
           {-^
               an action running the circuit and returning the value of the output signal&#x2019;s
               first occurence
           -}
    run uiBackend uiCircuit i = run where

        run = do
                  oMVar <- newEmptyMVar
                  UIBackend.initialize uiBackend
                  let

                      circuitCreation = Circuit.create
                                            (proc _ -> do
                                                 quittingReq <- runReader $
                                                                case uiCircuit of
                                                                    UICircuit arrow -> arrow
                                                             -< (i,topLevel uiBackend)
                                                 consume $ DSignal.consumer qReqHdlr -< quittingReq)
                                            ()

                      qReqHdlr o      = do
                                            requestQuitting uiBackend
                                            putMVar oMVar o

                  (_,finalizeCircuit) <- circuitCreation
                  UIBackend.handleEvents uiBackend
                  finalizeCircuit
                  UIBackend.finalize uiBackend
                  takeMVar oMVar
                  -- Where are the top level windows removed?
