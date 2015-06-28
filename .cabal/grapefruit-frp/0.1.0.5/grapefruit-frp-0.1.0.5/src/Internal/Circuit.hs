module Internal.Circuit (

    Circuit (Circuit),
    CircuitArrow,
    getECFinalizerAdd,
    getECFinalization,
    getStartTimeID

) where

    -- Control
    import Control.Category                 as Category
    import Control.Arrow                    as Arrow
    import Control.Arrow.Operations         as ArrowOperations
    import Control.Arrow.Transformer        as ArrowTransformer
    import Control.Arrow.Transformer.Reader as ReaderArrow
    import Control.Arrow.Transformer.Writer as WriterArrow
    import Control.Concurrent.MVar          as MVar

    -- Data
    import Data.Unique as Unique

    -- FRP.Grapefruit
    import FRP.Grapefruit.Setup as Setup

    {-|
        A circuit describes a reactive system.

        The @era@ parameter denotes the time interval during which the circuit is in existence. It
        is completely analogous to the era parameters of signal types which are described in the
        documentation of "FRP.Grapefruit.Signal".

        Input and output of a circuit are typically signals, tuples of signals (with @()@ as the
        corner case) or records of signals as provided by the package grapefruit-records. The era
        parameters of these signals usually match the @era@ parameter of the circuit.

        A circuit consumes only one input value and produces only one output value. This happens
        when the circuit is constructed. So the temporal behavior does not come from turning
        multiple inputs into multiple outputs but from using signals as inputs and outputs.

        A circuit has the ability to interact with the outside world (that is, perform I/O).

        The 'ArrowApply' instance of @Circuit era@ is currently needed for implementing other parts
        of Grapefruit. However, it should not be taken for granted that it will remain in future
        versions. So it is better to not use it outside Grapefruit.
    -}
    newtype Circuit era i o = Circuit (CircuitArrow i o)
                            deriving (
                                         Category,
                                         Arrow,
                                         ArrowLoop,
                                         ArrowApply
                                     )

    type CircuitArrow = ReaderArrow Unique ECFinVarReaderArrow

    type ECFinVarReaderArrow = ReaderArrow (MVar (IO ())) SetupWriterArrow

    type SetupWriterArrow = WriterArrow Setup IOArrow

    type IOArrow = Kleisli IO

    -- “EC” stands for “event cycle”
    getECFinalizerAdd :: Circuit era () (IO () -> IO ())
    getECFinalizerAdd = Circuit $
                        lift $
                        readState >>> arr addFinalizer where

        addFinalizer finalizerVar finalizer = modifyMVar_ finalizerVar ((>> finalizer) >>> return)

    getECFinalization :: Circuit era () (IO ())
    getECFinalization = Circuit $
                        lift $
                        readState >>> arr (\finalizerVar -> do
                                                                finalizer <- takeMVar finalizerVar
                                                                putMVar finalizerVar (return ())
                                                                finalizer)

    getStartTimeID :: Circuit era () Unique
    getStartTimeID = Circuit $ readState
