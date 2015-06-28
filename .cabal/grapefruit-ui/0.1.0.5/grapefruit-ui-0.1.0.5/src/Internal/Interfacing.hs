module Internal.Interfacing (

    -- * Interfacing
    Interfacing (Interfacing),
    basic,
    with,

    -- * Inner components
    With (With)

) where

    -- Control
    import Control.Arrow                    as Arrow
    import Control.Arrow.Operations         as ArrowOperations
    import Control.Arrow.Transformer.Reader as ReaderArrow

    -- Data
    import Data.Record                as Record
    import Data.Record.Signal         as SignalRecord
    import Data.Record.Signal.Context as ContextSignalRecord

    -- FRP.Grapefruit
    import FRP.Grapefruit.Circuit as Circuit

    -- Internal
    import {-# SOURCE #-} Internal.UIItem    as UIItem
    import                Internal.UICircuit as UICircuit

    -- Graphics.UI.Grapefruit
    import Graphics.UI.Grapefruit.Comp as UIComp

    -- Fixities
    infixr 1 `With`

    -- * Interfacing
    {-|
        Describes communication of an item with its environment.

        An interfacing is a mapping from native items to circuits. These circuits handle input
        consumption and output production.
    -}
    newtype Interfacing nativeItem era i o = Interfacing (ReaderArrow nativeItem (Circuit era) i o)

    {-|
        Creates an interfacing based on signal connectors (consumers and producers).

        The input and output are signal records. The only restriction to these records is that their
        fields must have corresponding fields in the connector records. The order of fields in the
        signal records may differ from the order of connector fields and there may be connector
        fields without a corresponding signal field. Connectors, for which no signal field exists,
        are not performed.
    -}
    basic :: (Record SignalKind extIShape, Record SignalKind extOShape,
              Subrecord extIShape iShape, Subrecord extOShape oShape)
          => ContextConsumerRecord nativeItem iShape
          -> ContextProducerRecord nativeItem oShape
          -> Interfacing nativeItem era (SignalRecord era extIShape) (SignalRecord era extOShape)
    basic consumerRecord producerRecord = Interfacing $
                                          ContextSignalRecord.consume (narrow consumerRecord) >>>
                                          ContextSignalRecord.produce (narrow producerRecord)

    inner :: (UIComp uiComp)
          => (nativeItem -> Placement innerItem uiBackend)
          -> uiComp innerItem uiBackend era innerI innerO
          -> Interfacing nativeItem era innerI innerO
    inner placement innerComp = Interfacing $
                                arr id &&& (readState >>> arr placement) >>>
                                liftReader (runReader innerArrow) where

        UICircuit innerArrow = toUICircuit innerComp

    {-|
        Extends an interfacing so that the resulting interfacing also adds an inner component to the
        UI item in question and extends the input and output to contain the input and output of the
        inner component.
    -}
    with :: (UIComp uiComp)
         => (nativeItem -> Placement innerItem uiBackend)
            -- ^conversion from a native item into the placement for its inner items
         -> uiComp innerItem uiBackend era innerI innerO
            -- ^an inner user interface component
         -> Interfacing nativeItem era baseI baseO
            -- ^an interfacing to which the inner component interfacing shall be added
         -> Interfacing nativeItem era (baseI `With` innerI) (baseO `With` innerO)
    with placement innerComp (Interfacing baseInterfacingImpl) = interfacing' where

        interfacing'                     = Interfacing $
                                           arr fromWith                                 >>>
                                           baseInterfacingImpl *** innerInterfacingImpl >>>
                                           arr toWith

        Interfacing innerInterfacingImpl = inner placement innerComp


        fromWith (base `With` inner)     = (base,inner)

        toWith (base,inner)              = base `With` inner

    -- * Inner components
    -- |An input or output, extended with the input or output of an inner component.
    data base `With` inner = base `With` inner
