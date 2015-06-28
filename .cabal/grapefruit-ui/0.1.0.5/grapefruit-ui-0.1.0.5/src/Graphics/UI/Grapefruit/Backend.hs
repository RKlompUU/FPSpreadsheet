-- |This module defines the basic interface to all user interface backends.
module Graphics.UI.Grapefruit.Backend (

    UIBackend (..)

) where

    -- Graphics.UI.Grapefruit
    import {-# SOURCE #-} Internal.UIItem as UIItem

    {-|
        The class of all user interface backends.

        A backend is represented by a type. This technique allows the class system to be used to
        manage different implementations of the same interface. @UIBackend@ declares an interface to
        basic functionality and is implemented by all user interface backends. Subclasses of
        @UIBackend@ extend the basic interface. A backend can be an instance of only some of these
        subclasses when some functionality is not yet implemented or cannot be provided by the
        backend. Backend types are typically used as phantom parameters. However, in some cases, an
        explicit value of a backend type is needed as a function argument. Therefore, a backend is
        usually a single-value type whose only value is named like the type.

        All associated types and methods of @UIBackend@ are used internally by Grapefruit and should
        not be used directly by the user.
    -}
    class UIBackend uiBackend where

        -- |@WidgetPlacement /uiBackend/@ provides the result of @'Placement' 'Widget' /uiBackend/@.
        type WidgetPlacement uiBackend :: *

        -- |@WindowPlacement /uiBackend/@ provides the result of @'Placement' 'Window' /uiBackend/@.
        type WindowPlacement uiBackend :: *

        -- |Initializes the backend.
        initialize :: uiBackend -> IO ()

        -- |Executes the event handling loop.
        handleEvents :: uiBackend -> IO ()

        -- |Asks the event handling loop to quit.
        requestQuitting :: uiBackend -> IO ()

        -- |Finalizes the backend.
        finalize :: uiBackend -> IO ()

        -- |Yields the placement of top-level windows.
        topLevel :: uiBackend -> Placement Window uiBackend
