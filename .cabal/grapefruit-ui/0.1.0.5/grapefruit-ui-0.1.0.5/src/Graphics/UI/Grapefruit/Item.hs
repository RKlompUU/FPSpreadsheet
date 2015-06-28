{-|
    This module provides support for user interface items.

    UI items are the building blocks of user interfaces. Typical items are widgets and windows. A UI
    item may contain another item or a UI circuit which is a system of UI items. An example of the
    former case is a window which contains a single widget. An example of the latter case is a box
    widget which contains an arbitrary number of other widgets.
-}
module Graphics.UI.Grapefruit.Item (

    -- * User interface items in general
    UIItem,
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

    -- Internal
    import Internal.UIItem as UIItem
