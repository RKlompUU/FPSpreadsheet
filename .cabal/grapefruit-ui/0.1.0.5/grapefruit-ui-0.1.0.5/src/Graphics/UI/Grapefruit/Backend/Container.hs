module Graphics.UI.Grapefruit.Backend.Container (

    -- * Interface
    ContainerUIBackend (..),

    -- * Utilities
    Column (Column),
    TextCellDisplay (TextCellDisplay),
    ProgressCellDisplay (ProgressCellDisplay),
    Availability (Never, AsNecessary, Always),

    -- * Field names
    Elements (Elements),
    Columns (Columns),
    HasScrollbars (HasScrollbars),
    Selection (Selection)

) where

    -- Data
    import Data.Sequence           as Seq
    import Data.Set                as Set
    import Data.Fraction           as Fraction
    import Data.Colour.RGBSpace    as RGBSpace
    import Data.Record             as Record
    import Data.Record.Optionality as OptionalityRecord

    -- FRP.Grapefruit
    import FRP.Grapefruit.Signal             as Signal
    import FRP.Grapefruit.Signal.Segmented   as SSignal
    import FRP.Grapefruit.Signal.Incremental as ISignal

    -- Graphics.UI.Grapefruit
    import Graphics.UI.Grapefruit.Item          as UIItem
    import Graphics.UI.Grapefruit.Backend.Basic as BasicUIBackend

    -- * Interface
    class (BasicUIBackend uiBackend) => ContainerUIBackend uiBackend where

        listView :: Brick Widget
                          uiBackend
                          (X :& Req Elements      ::: ISignal `Of` Seq el
                             :& Req Columns       ::: ISignal `Of` Seq (Column uiBackend el)
                             :& Opt HasScrollbars ::: SSignal `Of` (Orientation -> Availability))
                          (X :&     Selection     ::: SSignal `Of` Seq el)

        setView :: (Ord el) =>
                   Brick Widget
                         uiBackend
                         (X :& Req Elements      ::: ISignal `Of` Set el
                            :& Req Columns       ::: ISignal `Of` Seq (Column uiBackend el)
                            :& Opt HasScrollbars ::: SSignal `Of` (Orientation -> Availability))
                         (X :&     Selection     ::: SSignal `Of` Set el)

        data Cell uiBackend :: * -> *

        textCell :: Cell uiBackend TextCellDisplay

        progressCell :: Cell uiBackend ProgressCellDisplay

    -- * Utilities
    data Column uiBackend el where

        Column :: String -> (el -> display) -> Cell uiBackend display -> Column uiBackend el

    data TextCellDisplay = TextCellDisplay String (RGB Fraction)

    data ProgressCellDisplay = ProgressCellDisplay Fraction (Maybe String)

    -- FIXME: This should probably go into some more general module.
    data Availability = Never | AsNecessary | Always

    -- * Field names
    data Elements = Elements

    data Columns = Columns

    data HasScrollbars = HasScrollbars

    data Selection = Selection
