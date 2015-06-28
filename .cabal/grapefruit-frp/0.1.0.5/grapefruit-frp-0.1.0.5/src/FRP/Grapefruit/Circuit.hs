-- |This module provides circuits which are descriptions of reactive systems.
module FRP.Grapefruit.Circuit (

    Circuit,
    act,
    putSetup,
    create

) where

    -- Prelude
    import           Prelude (($), (>>), IO, flip, return)
    import qualified Prelude

    -- Control
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

    -- Internal
    import Internal.Circuit as Circuit

    {-|
        This circuit takes an I/O action when it is constructed, performs this action immediately
        and outputs its result.
    -}
    act :: Circuit era (IO output) output
    act = Circuit $ (lift >>> lift >>> lift) (Kleisli Prelude.id)

    {-|
        A circuit which triggers initialization and finalization according to a given setup.
    -}
    putSetup :: Circuit era Setup ()
    putSetup = Circuit $ (lift >>> lift) write

    {-|
        Creates a circuit.

        The second argument of @create@ is fed into the circuit as its input and the circuit is
        constructed then. After that, the initialization actions of all setups inserted by
        'putSetup' are run. The finalization actions of the setups are chained and returned by
        @create@ together with the output of the circuit.

        Note that initialization is done completely after circuit creation. This allows outputs of
        circuits to be generated before they are used for forming circuit inputs. This is important
        to avoid circular dependencies when 'loop' is used.
    -}
    create :: (forall era. Circuit era i o) -> i -> IO (o,IO ())
    create circuit input = do
                               startTimeID <- newUnique
                               ecFinalizerVar <- newMVar (return ())
                               (output,setup) <- runCircuitArrow (polyCircuitArrow circuit)
                                                                 startTimeID
                                                                 ecFinalizerVar
                                                                 input
                               finalize <- Setup.run setup
                               return (output,finalize)
    {-
        When creating subcircuits because of dynamicity create neither a new EC finalizer variable,
        nor a new time ID (take the one from the event triggering the subcircuit creation instead).
    -}

    polyCircuitArrow :: (forall era. Circuit era input output) -> CircuitArrow input output
    polyCircuitArrow plainCircuit = circuitArrow plainCircuit

    circuitArrow :: Circuit era input output -> CircuitArrow input output
    circuitArrow (Circuit circuitArrow) = circuitArrow

    runCircuitArrow :: CircuitArrow input output
                    -> Unique
                    -> MVar (IO ())
                    -> input
                    -> IO (output,Setup)
    runCircuitArrow circuitArrow startTimeID ecFinalizerVar input = run where

        run                 = runKleisli ioArrow input

        ioArrow             = runWriter setupWriterArrow

        setupWriterArrow    = arr (flip (,) ecFinalizerVar) >>> runReader ecFinVarReaderArrow

        ecFinVarReaderArrow = arr (flip (,) startTimeID) >>> runReader circuitArrow
