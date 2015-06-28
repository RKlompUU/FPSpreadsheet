{-|
    This module declares a subclass of 'UIBackend' with methods that every reasonable UI backend
    should implement.
-}
module Graphics.UI.Grapefruit.Backend.Basic (

    -- * Interface
    BasicUIBackend (..),

    -- * Utilities
    Orientation (Horizontal, Vertical),
    Caption (ColdCaption, HotCaption),

    -- * Field names
    Closure (Closure),
    Content (Content),
    Push (Push),
    Text (Text),
    Title (Title)

) where

    -- Data
    import Data.Record             as Record
    import Data.Record.Optionality as OptionalityRecord

    -- FRP.Grapefruit
    import FRP.Grapefruit.Signal             as Signal
    import FRP.Grapefruit.Signal.Discrete    as DSignal
    import FRP.Grapefruit.Signal.Segmented   as SSignal

    -- Graphics.UI.Grapefruit
    import Graphics.UI.Grapefruit.Item    as UIItem
    import Graphics.UI.Grapefruit.Circuit as UICircuit
    import Graphics.UI.Grapefruit.Backend as UIBackend

    -- * Interface
    -- |A subclass of 'UIBackend' which declares basic bricks and boxes.
    class (UIBackend uiBackend) => BasicUIBackend uiBackend where

        -- |A widget showing one line of text.
        label :: Brick Widget uiBackend (X :& Req Text ::: SSignal `Of` String) X

        -- |A push button.
        pushButton :: Brick Widget uiBackend (X :& Req Text ::: SSignal `Of` String)
                                             (X :&     Push ::: DSignal `Of` ())

        -- |An editor for a single line of text.
        lineEditor :: Brick Widget uiBackend X
                                             (X :& Content ::: SSignal `Of` String)

        -- |A widget which aggregates and arbitrary number of other widgets.
        box :: Orientation -> Box UICircuit Widget Widget uiBackend X X

        -- |An ordinary window.
        window :: Box UIItem Widget Window uiBackend (X :& Req Title   ::: SSignal `Of` String)
                                                     (X :&     Closure ::: DSignal `Of` ())

    -- * Utilities
    -- |An orientation of widgets in a box.
    data Orientation = Horizontal | Vertical

    {-|
        A caption of a widget with an optional hotkey marker.

        Currently, this type is not used.
    -}
    data Caption = ColdCaption String
                   -- ^a caption without a hotkey
                 | HotCaption String Char String
                   -- ^a caption consisting of a prefix string, a hotkey and a suffix string

    -- * Field names
    {-|
        A field name.

        Typical properties:

        [kind]
            output

        [type]
            @'DSignal' &#x60;'Of'&#x60; ()@

        [meaning]
            a &#x201C;window was closed&#x201D; event
    -}
    data Closure = Closure

    {-|
        A field name.

        Typical properties:

        [kind]
            output

        [type]
            @'SSignal' &#x60;'Of'&#x60; /val/@ for some type @/val/@

        [meaning]
            the content of an editor widget
    -}
    data Content = Content

    {-|
        A field name.

        Typical properties:

        [kind]
            output

        [type]
            @'DSignal' &#x60;'Of'&#x60; ()@

        [meaning]
            a stream of button push events
    -}
    data Push = Push

    {-|
        A field name.

        Typical properties:

        [kind]
            input (required)

        [type]
            @'SSignal' &#x60;'Of'&#x60; String@

        [meaning]
            the caption of a widget
    -}
    data Text = Text

    {-|
        A field name.

        Typical properties:

        [kind]
            input (required)

        [type]
            @'SSignal' &#x60;'Of'&#x60; String@

        [meaning]
            the title of a window
    -}
    data Title = Title
