-- |This module provides a GTK+-based UI backend.
module Graphics.UI.Grapefruit.GTK (

    GTK (GTK)

) where

    {-FIXME:
        Originally, this module only contained the declaration of GTK. Unfortunately, GHC 7 did not
        (re-)export the instances from the backend modules, possibly because it has a bug regarding
        the interplay of orphan modules and mutually dependent modules. As a result, we copied the
        contents of all other modules into this module. :-(
    -}

    -- Prelude
    import Prelude hiding (sequence_, mapM_)

    -- Control
    import Control.Monad as Monad hiding (sequence_, mapM_)
    import Control.Arrow as Arrow

    -- Data
    import Data.Foldable              as Foldable
    import Data.Sequence              as Seq      hiding (reverse, zipWith)
    import Data.Set                   as Set
    import Data.Fraction              as Fraction
    import Data.Colour.RGBSpace       as RGBSpace
    import Data.IORef                 as IORef
    import Data.Record                as Record
    import Data.Record.Optionality    as OptRecord
    import Data.Record.Signal         as SignalRecord
    import Data.Record.Signal.Context as ContextSignalRecord

    -- FRP.Grapefruit
    import FRP.Grapefruit.Setup                       as Setup
    import FRP.Grapefruit.Circuit                     as Circuit
    import FRP.Grapefruit.Signal                      as Signal
    import FRP.Grapefruit.Signal.Discrete             as DSignal
    import FRP.Grapefruit.Signal.Segmented            as SSignal
    import FRP.Grapefruit.Signal.Incremental          as ISignal    hiding (const)
    import FRP.Grapefruit.Signal.Incremental.Sequence as SeqISignal hiding (reverse)
    import FRP.Grapefruit.Signal.Incremental.Set      as SetISignal hiding (Diff)

    -- Graphics.UI.Grapefruit
    import Graphics.UI.Grapefruit.Comp              as UIComp
    import Graphics.UI.Grapefruit.Item              as UIItem
    import Graphics.UI.Grapefruit.Backend           as UIBackend
    import Graphics.UI.Grapefruit.Backend.Basic     as BasicUIBackend
    import Graphics.UI.Grapefruit.Backend.Container as ContainerUIBackend

    -- System.Glib
    import qualified System.Glib.GObject    as Glib
    import qualified System.Glib.Signals    as Glib
    import qualified System.Glib.Attributes as Glib

    -- Graphics.UI.Gtk
    import qualified Graphics.UI.Gtk as Gtk

    {-|
        Denotes the GTK+-based UI backend.

        See the documentation of "Graphics.UI.Grapefruit.Backend" for an introduction to UI
        backends.
    -}
    data GTK = GTK

    instance UIBackend GTK where

        type WidgetPlacement GTK = Gtk.Widget -> IO ()

        type WindowPlacement GTK = Gtk.Window -> IO ()

        initialize GTK = Gtk.unsafeInitGUIForThreadedRTS >> return ()

        handleEvents GTK = Gtk.mainGUI

        requestQuitting GTK = Gtk.mainQuit

        finalize GTK = return ()

        topLevel GTK = const (return ())

    instance BasicUIBackend GTK where

        label = widgetBrick (Gtk.labelNew (Nothing :: Maybe String))
                            Gtk.toWidget
                            (X :& Text := attrConsumer Gtk.labelLabel)
                            X

        pushButton = widgetBrick Gtk.buttonNew
                                 Gtk.toWidget
                                 (X :& Text := attrConsumer Gtk.buttonLabel)
                                 (X :& Push := eventProducer Gtk.onPressed)

        lineEditor = widgetBrick Gtk.entryNew
                                 Gtk.toWidget
                                 X
                                 (X :& Content := attrEventProducer Gtk.entryText
                                                                      Gtk.onEditableChanged)

        box orientation = widgetBox (case orientation of
                                         Horizontal -> newGtkBox Gtk.hBoxNew
                                         Vertical   -> newGtkBox Gtk.vBoxNew)
                                    Gtk.toWidget
                                    Gtk.containerAdd
                                    X
                                    X

        window = windowBox Gtk.windowNew
                           Gtk.toWindow
                           Gtk.containerAdd
                           (X :& Title   := attrConsumer Gtk.windowTitle)
                           (X :& Closure := eventProducer Gtk.onDestroy)

    newGtkBox :: (Gtk.BoxClass gtkBox) => (Bool -> Int -> IO gtkBox) -> IO Gtk.Box
    newGtkBox rawNewGtkBox = fmap Gtk.toBox (rawNewGtkBox False 0)

    instance ContainerUIBackend GTK where

        listView = listViewBrick id id

        setView = listViewBrick SetISignal.toSeqs (Set.fromList . Foldable.toList)

        data Cell GTK display = forall gtkCellRenderer. (Gtk.CellRendererClass gtkCellRenderer) =>
                                Cell (IO gtkCellRenderer) [CellValAttr gtkCellRenderer display]

        textCell = Cell Gtk.cellRendererTextNew [textAttr,backgroundColorAttr] where

            textAttr                                      = CellValAttr toText Gtk.cellText

            toText (TextCellDisplay text _)               = text

            backgroundColorAttr                           = CellValAttr toBackgroundColor
                                                                        Gtk.cellTextBackgroundColor

            toBackgroundColor (TextCellDisplay _ bgColor) = toGtkColor bgColor

        progressCell = Cell Gtk.cellRendererProgressNew [valueAttr,textAttr] where

            valueAttr                                = CellValAttr toValue Gtk.cellProgressValue

            toValue (ProgressCellDisplay progress _) = round (Fraction.toPercentage progress)

            textAttr                                 = CellValAttr toText Gtk.cellProgressText

            toText (ProgressCellDisplay _ maybeText) = maybeText

    data CellValAttr gtkCellRenderer display = forall gtkReadVal gtkWriteVal.
                                               CellValAttr (display -> gtkWriteVal)
                                                           (Gtk.ReadWriteAttr gtkCellRenderer
                                                                              gtkReadVal
                                                                              gtkWriteVal)

    listViewBrick :: (forall era. ISignal era container -> ISignal era (Seq el))
                  -> (Seq el -> container)
                  -> Brick Widget
                           GTK
                           (X :& Req Elements      ::: ISignal `Of` container
                              :& Req Columns       ::: ISignal `Of` Seq (Column GTK el)
                              :& Opt HasScrollbars ::: SSignal `Of` (Orientation -> Availability))
                           (X :&     Selection ::: SSignal `Of` container)
    listViewBrick fromContainerSignal toContainer = brick where

        brick = widgetBrick (do
                                 gtkScrolledWindow <- Gtk.scrolledWindowNew Nothing Nothing
                                 gtkTreeView <- Gtk.treeViewNew
                                 Gtk.containerAdd gtkScrolledWindow gtkTreeView
                                 gtkTreeSelection <- Gtk.treeViewGetSelection gtkTreeView
                                 Gtk.treeSelectionSetMode gtkTreeSelection Gtk.SelectionMultiple
                                 gtkListStore <- Gtk.listStoreNew []
                                 seqRef <- newIORef Seq.empty
                                 Gtk.treeViewSetModel gtkTreeView gtkListStore
                                 return (gtkScrolledWindow,gtkTreeView,gtkListStore,seqRef))
                            (\(gtkScrolledWindow,_,_,_) -> Gtk.toWidget gtkScrolledWindow)
                            (X :& Elements      := consumerComap fromContainerSignal .
                                                   seqSignalConsumer
                                                       (withModelAndRef insertListViewElement)
                                                       (withModelAndRef deleteListViewElement)
                                                       (withModelAndRef shiftListViewElement)
                               :& Columns       := seqSignalConsumer
                                                       (withViewAndModel insertListViewColumn)
                                                       (withView         deleteListViewColumn)
                                                       (withView         shiftListViewColumn)
                               :& HasScrollbars := withScrolledWindow hasScrollbarsConsumer)
                            (X :& Selection := producerMap (fmap toContainer) .
                                               withViewAndModel selectionProducer) where

        withScrolledWindow fun (gtkScrolledWindow,_,_,_) = fun gtkScrolledWindow

        withViewAndModel :: (Gtk.TreeView -> Gtk.ListStore el -> result)
                         -> (Gtk.ScrolledWindow,Gtk.TreeView,Gtk.ListStore el,IORef (Seq el))
                         -> result
        withViewAndModel fun (_,gtkTreeView,gtkListStore,_) = fun gtkTreeView gtkListStore

        withView :: (Gtk.TreeView -> result)
                 -> (Gtk.ScrolledWindow,Gtk.TreeView,Gtk.ListStore el,IORef (Seq el))
                 -> result
        withView fun = withViewAndModel (const . fun)

        withModelAndRef :: (Gtk.ListStore el -> IORef (Seq el) -> result)
                        -> (Gtk.ScrolledWindow,Gtk.TreeView,Gtk.ListStore el,IORef (Seq el))
                        -> result
        withModelAndRef fun (_,_,gtkListStore,seqRef) = fun gtkListStore seqRef

    consumerComap :: (forall era. signal era val -> signal' era val')
                  -> (Consumer signal' val' -> Consumer signal val)
    consumerComap signalFun consumer' = Consumer $ arr signalFun >>> Signal.consume consumer'

    producerMap :: (forall era. signal era val -> signal' era val')
                -> (Producer signal val -> Producer signal' val')
    producerMap signalFun producer = Producer $ Signal.produce producer >>> arr signalFun

    insertListViewElement :: Gtk.ListStore el -> IORef (Seq el) -> Int -> el -> IO ()
    insertListViewElement gtkListStore seqRef idx el = Gtk.listStoreInsert gtkListStore idx el >>
                                                       modifyIORef seqRef (flip patch diff) where

        diff = SeqISignal.elementInsertion idx el

    deleteListViewElement :: Gtk.ListStore el -> IORef (Seq el) -> Int -> IO ()
    deleteListViewElement gtkListStore seqRef idx = Gtk.listStoreRemove gtkListStore idx >>
                                                    modifyIORef seqRef (flip patch diff) where

        diff = SeqISignal.elementDeletion idx

    shiftListViewElement :: Gtk.ListStore el -> IORef (Seq el) -> Int -> Int -> IO ()
    shiftListViewElement gtkListStore seqRef from to = shift where

        shift = do
                    seq <- readIORef seqRef
                    Gtk.listStoreRemove gtkListStore from
                    Gtk.listStoreInsert gtkListStore to (Seq.index seq from)
                    modifyIORef seqRef (flip patch diff)

        diff  = elementShift from to

    insertListViewColumn :: Gtk.TreeView -> Gtk.ListStore el -> Int -> Column GTK el -> IO ()
    insertListViewColumn gtkTreeView
                         gtkListStore
                         idx
                         (Column title toDisplay (Cell newCellRenderer cellValAttrs)) = insert where

        insert                    = do
                                        gtkTreeViewColumn <- Gtk.treeViewColumnNew
                                        Gtk.treeViewColumnSetTitle gtkTreeViewColumn title
                                        gtkCellRenderer <- newCellRenderer
                                        Gtk.cellLayoutPackStart gtkTreeViewColumn
                                                                gtkCellRenderer
                                                                True
                                        Gtk.cellLayoutSetAttributes gtkTreeViewColumn
                                                                    gtkCellRenderer
                                                                    gtkListStore
                                                                    (mapM cellValAss cellValAttrs)
                                        Gtk.treeViewInsertColumn gtkTreeView gtkTreeViewColumn idx
                                        return ()

        cellValAss cellValAttr el = case cellValAttr of
                                        CellValAttr toWriteVal
                                                    gtkWriteAttr -> gtkWriteAttr Gtk.:=
                                                                    toWriteVal (toDisplay el)

    deleteListViewColumn :: Gtk.TreeView -> Int -> IO ()
    deleteListViewColumn gtkTreeView idx = delete where

        delete = do
                     Just gtkTreeViewColumn <- Gtk.treeViewGetColumn gtkTreeView idx
                     Gtk.treeViewRemoveColumn gtkTreeView gtkTreeViewColumn
                     return ()

    shiftListViewColumn :: Gtk.TreeView -> Int -> Int -> IO ()
    shiftListViewColumn gtkTreeView from to = shift where

        shift                    = do
                                       Just gtkFromColumn <- Gtk.treeViewGetColumn gtkTreeView from
                                       if to == 0 then Gtk.treeViewMoveColumnFirst gtkTreeView
                                                                                   gtkFromColumn
                                                  else if from <= to
                                                           then shiftAfter gtkFromColumn to
                                                           else shiftAfter gtkFromColumn (pred to)

        shiftAfter gtkColumn idx = do
                                       Just gtkBeforeColumn <- Gtk.treeViewGetColumn gtkTreeView idx
                                       Gtk.treeViewMoveColumnAfter gtkTreeView
                                                                   gtkColumn
                                                                   gtkBeforeColumn

    seqSignalConsumer :: (gtkObject -> Int -> el -> IO ())
                      -> (gtkObject -> Int -> IO ())
                      -> (gtkObject -> Int -> Int -> IO ())
                      -> gtkObject
                      -> Consumer ISignal (Seq el)
    seqSignalConsumer insertOne deleteOne shiftOne gtkObject = consumer where

        consumer                              = ISignal.consumer (insert 0) patchObject

        patchObject (Diff atomicDiffs)        = mapM_ atomicPatchObject atomicDiffs

        atomicPatchObject (Insertion idx seq) = insert idx seq
        atomicPatchObject (Deletion idx cnt)  = delete idx cnt
        atomicPatchObject (Shift from cnt to) = let

                                                    oneShifts = zipWith (shiftOne gtkObject)
                                                                        [from..pred (from + cnt)]
                                                                        [to..pred (to + cnt)]

                                                in sequence_ (if from <= to then reverse oneShifts
                                                                            else oneShifts)
        atomicPatchObject (Update idx seq)    = delete idx (Seq.length seq) >> insert idx seq

        insert idx seq                        = zipWithM_ (insertOne gtkObject)
                                                          [idx..]
                                                          (Foldable.toList seq)

        delete idx cnt                        = replicateM_ cnt (deleteOne gtkObject idx)

    hasScrollbarsConsumer :: Gtk.ScrolledWindow -> Consumer SSignal (Orientation -> Availability)
    hasScrollbarsConsumer gtkScrolledWindow = SSignal.consumer hdlr where

        hdlr avails          = Gtk.scrolledWindowSetPolicy gtkScrolledWindow
                                                           (policy avails Horizontal)
                                                           (policy avails Vertical)

        policy avails orient = case avails orient of
                                   Never       -> Gtk.PolicyNever
                                   AsNecessary -> Gtk.PolicyAutomatic
                                   Always      -> Gtk.PolicyAlways

    selectionProducer :: Gtk.TreeView -> Gtk.ListStore el -> Producer SSignal (Seq el)
    selectionProducer gtkTreeView gtkListStore = Producer $ proc _ -> do
        gtkTreeSelection <- act -< Gtk.treeViewGetSelection gtkTreeView
        let

            actualProducer = readEventProducer (readSelection gtkListStore)
                                               Gtk.onSelectionChanged
                                               gtkTreeSelection

        Signal.produce $ actualProducer -<< ()

    readSelection :: Gtk.ListStore el -> Gtk.TreeSelection -> IO (Seq el)
    readSelection gtkListStore gtkTreeSelection = read where

        read = do
                   gtkSelPaths <- Gtk.treeSelectionGetSelectedRows gtkTreeSelection
                   els <- mapM (Gtk.listStoreGetValue gtkListStore)
                               (Prelude.map treePathToIndex gtkSelPaths)
                   return (Seq.fromList els)

    treePathToIndex :: Gtk.TreePath -> Int
    treePathToIndex [idx] = idx
    treePathToIndex _     = error "grapefruit-ui-gtk: tree path has not length 1"

    -- FIXME: This should maybe go to another place.
    toGtkColor :: RGB Fraction -> Gtk.Color
    toGtkColor (RGB redFrac greenFrac blueFrac) = Gtk.Color (toWord16 redFrac)
                                                            (toWord16 greenFrac)
                                                            (toWord16 blueFrac) where

        toWord16 = round . toNumber (0,0xFFFF)

    -- |Constructs a GTK+-based widget brick.
    widgetBrick :: (OptRecord iOptRecord,
                    Record SignalKind (All iOptRecord),
                    Record SignalKind oRecord)
                => IO nativeWidget
                   -- ^an action which creates a native widget
                -> (nativeWidget -> Gtk.Widget)
                   -- ^converts a native widget into a Gtk2Hs widget
                -> ContextConsumerRecord nativeWidget (All iOptRecord)
                   -- ^consumers of those inputs which are specific to this brick
                -> ContextProducerRecord nativeWidget oRecord
                   -- ^producers of those outputs which are specific to this brick
                -> Brick Widget GTK iOptRecord oRecord
    widgetBrick = brickOrBox brick commonWidgetConsumerRecord commonWidgetProducerRecord

    -- |Constructs a GTK+-based window brick.
    windowBrick :: (OptRecord iOptRecord,
                    Record SignalKind (All iOptRecord),
                    Record SignalKind oRecord)
                => IO nativeWindow
                   -- ^an action which creates a native window
                -> (nativeWindow -> Gtk.Window)
                   -- ^converts a native window into a Gtk2Hs window
                -> ContextConsumerRecord nativeWindow (All iOptRecord)
                   -- ^consumers of those inputs which are specific to this brick
                -> ContextProducerRecord nativeWindow oRecord
                   -- ^producers of those outputs which are specific to this brick
                -> Brick Window GTK iOptRecord oRecord
    windowBrick = brickOrBox brick commonWindowConsumerRecord commonWindowProducerRecord

    -- |Constructs a GTK+-based widget box.
    widgetBox :: (UIComp innerUIComp,
                  OptRecord iOptRecord,
                  Record SignalKind (All iOptRecord),
                  Record SignalKind oRecord)
              => IO nativeWidget
                 -- ^an action which creates a native widget
              -> (nativeWidget -> Gtk.Widget)
                  -- ^converts a native widget into a Gtk2Hs widget
              -> (nativeWidget -> Placement innerItem GTK)
                 -- ^conversion from a native widget into the placement for its inner items
              -> ContextConsumerRecord nativeWidget (All iOptRecord)
                 -- ^consumers of those inputs which are specific to this box
              -> ContextProducerRecord nativeWidget oRecord
                 -- ^producers of those outputs which are specific to this box
              -> Box innerUIComp innerItem Widget GTK iOptRecord oRecord
    widgetBox = brickOrBox UIItem.box commonWidgetConsumerRecord commonWidgetProducerRecord

    -- |Constructs a GTK+-based window box.
    windowBox :: (UIComp innerUIComp,
                  OptRecord iOptRecord,
                  Record SignalKind (All iOptRecord),
                  Record SignalKind oRecord)
              => IO nativeWindow
                 -- ^an action which creates a native window
              -> (nativeWindow -> Gtk.Window)
                 -- ^converts a native window into a Gtk2Hs window
              -> (nativeWindow -> Placement innerItem GTK)
                 -- ^conversion from a native window into the placement for its inner items
              -> ContextConsumerRecord nativeWindow (All iOptRecord)
                 -- ^consumers of those inputs which are specific to this box
              -> ContextProducerRecord nativeWindow oRecord
                 -- ^producers of those outputs which are specific to this box
              -> Box innerUIComp innerItem Window GTK iOptRecord oRecord
    windowBox = brickOrBox UIItem.box commonWindowConsumerRecord commonWindowProducerRecord

    brickOrBox :: (Gtk.WidgetClass gtkItem, Record SignalKind iRecord, Record SignalKind oRecord)
               => (ContextConsumerRecord nativeItem iRecord ->
                   ContextProducerRecord nativeItem oRecord ->
                   (nativeItem -> IO ())                    ->
                   ((gtkItem -> IO ()) -> IO nativeItem)    ->
                   result)
               -> ContextConsumerRecord gtkItem iRecord
               -> ContextProducerRecord gtkItem oRecord
               -> IO nativeItem
               -> (nativeItem -> gtkItem)
               -> result
    brickOrBox genericBrickOrBox
               contextConsumers
               contextProducers
               newNativeItem
               toGtkItem         = genericBrickOrBox (Record.map (brickOrBoxTransformer toGtkItem)
                                                                 contextConsumers)
                                                     (Record.map (brickOrBoxTransformer toGtkItem)
                                                                 contextProducers)
                                                     (Gtk.widgetShowAll . toGtkItem)
                                                     {-FIXME:
                                                         Using widgetShowAll instead of widgetShow
                                                         is just a temporary hack for supporting
                                                         tree views inside scrolled windows.
                                                     -}
                                                     newItem where

        newItem placement = do
                                nativeItem <- newNativeItem
                                placement (toGtkItem nativeItem)
                                return nativeItem

    brickOrBoxTransformer :: (nativeItem -> gtkItem)
                          -> Forall SignalKind
                                    (TransformerPiece (ContextConnectorStyle gtkItem connector)
                                                      (ContextConnectorStyle nativeItem connector))
    brickOrBoxTransformer toGtkItem = SignalForall (TransformerPiece (. toGtkItem))

    commonWidgetConsumerRecord :: ContextConsumerRecord Gtk.Widget
                                                        (All (CommonInputOptRecord Widget))
    commonWidgetConsumerRecord = X :& IsEnabled := SSignal.consumer . Gtk.widgetSetSensitivity

    commonWidgetProducerRecord :: ContextProducerRecord Gtk.Widget (CommonOutputRecord Window)
    commonWidgetProducerRecord = X

    commonWindowConsumerRecord :: ContextConsumerRecord Gtk.Window
                                                        (All (CommonInputOptRecord Window))
    commonWindowConsumerRecord = X

    commonWindowProducerRecord :: ContextProducerRecord Gtk.Window (CommonOutputRecord Window)
    commonWindowProducerRecord = X

    {-|
        Constructs a consumer of segmented signals which makes a Gtk2Hs attribute of a Gtk2Hs widget
        being updated on every update point of the consumed signal.
    -}
    attrConsumer :: (Glib.GObjectClass gObject)
                 => Glib.ReadWriteAttr gObject readVal writeVal
                 -> gObject
                 -> Consumer SSignal writeVal
    attrConsumer gAttr gObject = SSignal.consumer $ \val -> Glib.set gObject [gAttr Glib.:= val]

    -- |Constructs a producer of discrete signals that represent sequences of Gtk2Hs events.
    eventProducer :: (Glib.GObjectClass gObject)
                   => (gObject -> IO () -> IO (Glib.ConnectId gObject))
                      {-^
                          an @on/EventType/@ function of Gtk2Hs, representing a certain kind of
                          event
                      -}
                   -> gObject
                      -- ^a Gtk2Hs widget which provides the events
                   -> Producer DSignal ()
    eventProducer onEvent gObject = DSignal.producer (register onEvent gObject . ($ ()))

    {-|
        Constructs a producer of segmented signals whose values can be read with a certain I/O
        action and which are updated updated on certain Gtk2Hs events.
    -}
    readEventProducer :: (Glib.GObjectClass gObject)
                      => (gObject -> IO val)
                         -- ^an action which provides the current value of the produced signal
                      -> (gObject -> IO () -> IO (Glib.ConnectId gObject))
                         {-^
                             an @on/EventType/@ function of Gtk2Hs whose corresponding events mark
                             the update points of the produced signal except the update point at the
                             beginning of the era
                         -}
                      -> gObject
                         -- ^a Gtk2Hs widget
                      -> Producer SSignal val
    readEventProducer readVal onEvent gObject = SSignal.producer (readVal gObject)
                                                                 (register onEvent gObject)

    {-|
        Constructs a producer of segmented signals that reflect a Gtk2Hs attribute and are updated
        on certain Gtk2Hs events.
    -}
    attrEventProducer :: (Glib.GObjectClass gObject)
                      => Glib.ReadWriteAttr gObject readVal writeVal
                         -- ^a Gtk2Hs attribute providing the values of the produced signal
                      -> (gObject -> IO () -> IO (Glib.ConnectId gObject))
                         {-^
                             an @on/EventType/@ function of Gtk2Hs whose corresponding events mark
                             the update points of the produced signal except the update point at the
                             beginning of the era
                         -}
                      -> gObject
                         -- ^a Gtk2Hs widget
                      -> Producer SSignal readVal
    attrEventProducer gAttr = readEventProducer (flip Glib.get gAttr)

    register :: (Glib.GObjectClass gObject)
             => (gObject -> IO () -> IO (Glib.ConnectId gObject))
             -> gObject
             -> (IO () -> Setup)
    register onEvent gObject handler = setup $ do
                                                   connectID <- onEvent gObject handler
                                                   return (Glib.signalDisconnect connectID)
