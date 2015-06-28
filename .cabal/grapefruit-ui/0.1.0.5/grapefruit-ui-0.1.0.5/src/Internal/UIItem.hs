module Internal.UIItem (

    -- * User interface items in general
    UIItem (UIItem),
    item,

    -- * Bricks
    Brick,
    brick,
    just,

    -- * Boxes
    Box,
    box,
    with,
    With (With),

    -- * Kinds of items
    Item (type CommonInputOptRecord, type CommonOutputRecord),
    Placement,
    Widget,
    Window,

    -- * Field names
    IsEnabled (IsEnabled)

) where

    -- Controls
    import Control.Arrow                    as Arrow
    import Control.Arrow.Operations         as ArrowOperations
    import Control.Arrow.Transformer.Reader as ReaderArrow

    -- Data
    import Data.Record                as Record
    import Data.Record.Optionality    as OptionalityRecord
    import Data.Record.Signal         as SignalRecord
    import Data.Record.Signal.Context as ContextSignalRecord

    -- FRP.Grapefruit
    import FRP.Grapefruit.Setup            as Setup
    import FRP.Grapefruit.Circuit          as Circuit
    import FRP.Grapefruit.Signal           as Signal
    import FRP.Grapefruit.Signal.Segmented as SSignal

    -- Graphics.UI.Grapefruit
    import Graphics.UI.Grapefruit.Backend as UIBackend

    -- Internal
    import           Internal.UICircuit   as UICircuit
    import           Internal.Interfacing as Interfacing hiding (with)
    import qualified Internal.Interfacing as Interfacing

    -- Graphics.UI.Grapefruit
    import Graphics.UI.Grapefruit.Comp as UIComp

    -- Fixities
    infixr 6 `with`

    -- * User interface items in general
    {-|
        The type of user interface items.

        The @item@ parameter is a phantom parameter which denotes the kind of the item. It should
        be an instance of 'Item'.
    -}
    newtype UIItem item uiBackend era i o = UIItem (UICircuit item uiBackend era i o)

    -- manual deriving because of GHC bug #1133
    instance UIComp UIItem where

        circuit |>> UIItem uiCircuit = UIItem (circuit |>> uiCircuit)

        UIItem uiCircuit >>| circuit = UIItem (uiCircuit >>| circuit)

        loop (UIItem uiCircuit) = UIItem (Arrow.loop uiCircuit)

        toUICircuit (UIItem uiCircuit) = uiCircuit

        fromUIItem = id

    {-|
        Constructs an item using functionality of an underlying imperative library.

        The @nativeItem@ type variable represents an item type of the underlying library.
    -}
    {- item :: (nativeItem -> IO ())
            -- ^an action which makes a native item visible
         -> (Placement item uiBackend -> IO nativeItem)
            -- ^an action which creates a native item depending on a placement for the item
         -> Interfacing nativeItem era i o
            -- ^the interface specification for the item
         -> UIItem item uiBackend era i o -}
    item showItem
         newItem
         (Interfacing interfacingImpl) = UIItem $
                                         UICircuit $ arr id &&& readState >>> liftReader arrow where

        arrow = proc (i,placement) -> do
                    nativeItem <- act                       -< newItem placement
                    o          <- runReader interfacingImpl -< (i,nativeItem)
                    _          <- putSetup                  -< Setup.fromIO $ showItem nativeItem
                    returnA -< o

    -- * Bricks
    {-|
        A brick is an era-independent item with a comfortable record-based interface.

        The parameter @iOptRecord@ is an optionality record and therefore specifies a set of
        required and a set of optional fields. When the brick is used, the input fields can be given
        in any order and optional fields can be left out. Similarily, @oRecord@ specifies a set of
        fields (without optionalities) of whom not all have to be utilized by the user of the brick.

        A brick has additional input and output fields which are not explicitely mentioned in its
        type. For an item kind @/item/@, they are given by @'CommonInputOptRecord' /item/@ and
        @'CommonOutputRecord' /item/@. Therefore, it is possible to have inputs and outputs common
        for all bricks of a certain item kind.
    -}
    newtype Brick item uiBackend iOptRecord oRecord
        = Brick (forall era extIRecord extORecord.
                 (Record SignalKind extIRecord,
                  Record SignalKind extORecord,
                  Subrecord extIRecord
                            (All iOptRecord `Cat` All (CommonInputOptRecord item)),
                  Subrecord (Required iOptRecord `Cat` Required (CommonInputOptRecord item))
                            extIRecord,
                  Subrecord extORecord
                            (oRecord `Cat` CommonOutputRecord item)) =>
                 UIItem item uiBackend era (SignalRecord era extIRecord)
                                           (SignalRecord era extORecord))

    -- |Constructs a brick.
    brick :: (Item item,
              OptRecord iOptRecord,
              Record SignalKind (All iOptRecord),
              Record SignalKind oRecord)
          => ContextConsumerRecord nativeItem (All (CommonInputOptRecord item))
             {-^
                 consumers of those inputs which are common to all bricks of the respective item
                 kind
             -}
          -> ContextProducerRecord nativeItem (CommonOutputRecord item)
             {-^
                 producers of those outputs which are common to all bricks of the respective item
                 kind
             -}
          -> (nativeItem -> IO ())
             -- ^an action which makes a native item visible
          -> (Placement item uiBackend -> IO nativeItem)
             -- ^an action which creates a native item depending on a placement for the item
          -> ContextConsumerRecord nativeItem (All iOptRecord)
             -- ^consumers of those inputs which are specific to this brick
          -> ContextProducerRecord nativeItem oRecord
             -- ^producers of those outputs which are specific to this brick
          -> Brick item uiBackend iOptRecord oRecord
    brick commonConsumerRecord commonProducerRecord showItem newItem consumerRecord producerRecord
        = Brick $ item showItem
                       newItem
                       (Interfacing.basic (cat consumerRecord commonConsumerRecord)
                                          (cat producerRecord commonProducerRecord))

    {-|
        Converts a brick into an ordinary user interface component.

        The brick is first converted into a UI item which is then converted into the resulting
        component by applying 'fromUIItem'.

        The type of @just@ states the following properties of the resulting component:

            * The input record covers only fields which are input fields according to the type of
              the brick or are common input fields of all items of the respective kind.

            * The input record covers all input fields which are marked as required.

            * The output record covers only fields which are output fields according to the type of
              the brick or are common output fields of all items of the respective kind.

            * The order of fields is arbitrary.

            * The component is not tied to a specific era.

            * All input and output signals use the same era as the component.

        Dropping certain input or output fields results in the corresponding connectors not being
        executed.

        To make the type variables @extIRecord@ and @extORecord@ non-ambiguous, the lists of input
        and output field names have to be known at the call site. For the output field names, this
        is usually done via pattern matching.
    -}
    just :: (Record SignalKind extIRecord,
             Record SignalKind extORecord,
             Subrecord extIRecord
                       (All iOptRecord `Cat` All (CommonInputOptRecord item)),
             Subrecord (Required iOptRecord `Cat` Required (CommonInputOptRecord item))
                       extIRecord,
             Subrecord extORecord
                       (oRecord `Cat` CommonOutputRecord item),
             UIComp uiComp)
         => Brick item uiBackend iOptRecord oRecord
         -> uiComp item uiBackend era (SignalRecord era extIRecord) (SignalRecord era extORecord)
    just (Brick item) = fromUIItem item

    -- * Boxes
    {-|
        A box is a container which can be transformed into an item by putting a user interface
        component into it.

        The component which is put into a box is called the inner component of that box. The
        parameters @innerItem@ and @item@ tell the type of the inner component and the item kind, it
        is based on.

        The interface of an item made from a box is similar to that of an item made from a brick.
        The only difference is that the interface of the box item covers also the input and the
        output of the inner component.
    -}
    newtype Box innerUIComp innerItem item uiBackend iOptRecord oRecord
        = Box (forall era extIRecord extORecord innerI innerO.
               (Record SignalKind extIRecord,
                Record SignalKind extORecord,
                Subrecord extIRecord
                          (All iOptRecord `Cat` All (CommonInputOptRecord item)),
                Subrecord (Required iOptRecord `Cat` Required (CommonInputOptRecord item))
                          extIRecord,
                Subrecord extORecord
                          (oRecord `Cat` CommonOutputRecord item)) =>
               innerUIComp innerItem uiBackend era innerI innerO ->
               UIItem item uiBackend era (SignalRecord era extIRecord `With` innerI)
                                         (SignalRecord era extORecord `With` innerO))

    -- |Constructs a box.
    box :: (UIComp innerUIComp,
            Item item,
            OptRecord iOptRecord,
            Record SignalKind (All iOptRecord),
            Record SignalKind oRecord)
        => ContextConsumerRecord nativeItem (All (CommonInputOptRecord item))
             {-^
                 consumers of those inputs which are common to all bricks of the respective item
                 kind
             -}
        -> ContextProducerRecord nativeItem (CommonOutputRecord item)
             {-^
                 producers of those outputs which are common to all bricks of the respective item
                 kind
             -}
        -> (nativeItem -> IO ())
           -- ^an action which makes a native item visible
        -> (Placement item uiBackend -> IO nativeItem)
           -- ^an action which creates a native item depending on a placement for the item
        -> (nativeItem -> Placement innerItem uiBackend)
           -- ^conversion from a native item into the placement for its inner items
        -> ContextConsumerRecord nativeItem (All iOptRecord)
           -- ^consumers of those inputs which are specific to this box
        -> ContextProducerRecord nativeItem oRecord
           -- ^producers of those outputs which are specific to this box
        -> Box innerUIComp innerItem item uiBackend iOptRecord oRecord
    box commonConsumerRecord commonProducerRecord
        showItem
        newItem
        placement
        consumerRecord producerRecord
        = Box $ \innerComp -> item showItem
                                   newItem
                                   (Interfacing.with placement innerComp $
                                    Interfacing.basic (cat consumerRecord commonConsumerRecord)
                                                      (cat producerRecord commonProducerRecord))

    {-|
        Puts an inner component into a box and converts the result into an ordinary user interface
        component.

        This function is very similar to 'just'. In contrast to 'just', it takes the inner component
        as an additional argument and extends the input and output of the resulting component with
        the input and output of the inner component. Note that the era of the inner component equals
        the era of the resulting component.

        Applications of @with@ are usually written infix.
    -}
    with :: (Record SignalKind extIRecord,
             Record SignalKind extORecord,
             Subrecord extIRecord
                       (All iOptRecord `Cat` All (CommonInputOptRecord item)),
             Subrecord (Required iOptRecord `Cat` Required (CommonInputOptRecord item))
                       extIRecord,
             Subrecord extORecord
                       (oRecord `Cat` CommonOutputRecord item),
             UIComp uiComp)
         => Box innerUIComp innerItem item uiBackend iOptRecord oRecord
         -> innerUIComp innerItem uiBackend era innerI innerO
         -> uiComp item uiBackend era (SignalRecord era extIRecord `With` innerI)
                                      (SignalRecord era extORecord `With` innerO)
    with (Box item) = fromUIItem . item

    -- * Kinds of items
    {-|
        The class of all kinds of items.

        Instances of this class serve as phantom parameters of 'UIItem', 'UICircuit' and others.
    -}
    class (OptRecord (CommonInputOptRecord item),
           Record SignalKind (All (CommonInputOptRecord item)),
           Record SignalKind (CommonOutputRecord item)) =>
          Item item where

        -- |Inputs which are shared by all items of the respective kind.
        type CommonInputOptRecord item :: * -> *

        -- |Outputs which are shared by all items of the respective kind.
        type CommonOutputRecord item :: * -> *

    {-|
        The family of item placement types.

        A placement says where to place a user interface item, for example, to place a widget in a
        certain box or a window at the top level. @Graphics.UI.Grapefruit.Item@ declares two
        instances of @Placement@. @Placement 'Widget' /uiBackend/@ is equivalent to
        @'WidgetPlacement' /uiBackend/@ and @Placement 'Window' /uiBackend/@ is equivalent to
        @'WindowPlacement' /uiBackend/@.
    -}
    type family Placement item uiBackend :: *

    {-|
        The widget item kind.

        A widget is an item which resides inside a window. Examples of widgets are push buttons,
        labels and boxes (which contain other widgets themselves).
    -}
    data Widget

    instance Item Widget where

        type CommonInputOptRecord Widget = X :& Opt IsEnabled ::: SSignal `Of` Bool

        type CommonOutputRecord Widget = X

    type instance Placement Widget uiBackend = WidgetPlacement uiBackend

    {-|
        The window item kind.

        A window is an item which resides directly on the desktop and typically has a frame with a
        title and some control buttons. Examples of windows are application windows and dialogs.
    -}
    data Window

    instance Item Window where

        type CommonOutputRecord Window = X

        type CommonInputOptRecord Window = X

    type instance Placement Window uiBackend = WindowPlacement uiBackend

    -- * Field names
    {-|
        A field name.

        Typical properties:

        [kind]
            input (optional)

        [type]
            @'SSignal' &#x60;'Of'&#x60; Bool@

        [meaning]
            whether a widget is enabled or not

        Disabled widgets cannot receive user events and are typically displayed in a different
        style
    -}
    data IsEnabled = IsEnabled
