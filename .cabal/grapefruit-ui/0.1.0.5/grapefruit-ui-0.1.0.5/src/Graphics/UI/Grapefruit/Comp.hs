{-|
    This module provides general support for user interface components.

    A user interface component is either a user interface item or a user interface circuit. UI items
    are introduced by "Graphics.UI.Grapefruit.Item" and UI circuits by
    "Graphics.UI.Grapefruit.Circuit".
-}
module Graphics.UI.Grapefruit.Comp (

    UIComp ((|>>), (>>|), loop, toUICircuit, fromUIItem),
    (<<|),
    (|<<)

) where

    -- Control
    import Control.Arrow as Arrow -- for documentation only

    -- FRP.Grapefruit
    import FRP.Grapefruit.Circuit as Circuit

    -- Internal
    import {-# SOURCE #-} Internal.UICircuit as UICircuit
    import {-# SOURCE #-} Internal.UIItem    as UIItem

    {-|
        The class of all user interface components.

        A user interface component is a part of a user interface which communicates with the
        remainder of the user interface through signals.
    -}
    class UIComp uiComp where

        infixr 1 |>>
        {-|
            Adds a circuit before a user interface component.

            This does not add any items to the user interface but may add data manipulation and
            control functionality.
        -}
        (|>>) :: Circuit era i tmp
              -> uiComp item uiBackend era tmp o
              -> uiComp item uiBackend era i o

        infixr 1 >>|
        {-|
            Adds a circuit after a user interface component.

            This does not add any items to the user interface but may add data manipulation and
            control functionality.
        -}
        (>>|) :: uiComp item uiBackend era i tmp
              -> Circuit era tmp o
              -> uiComp item uiBackend era i o

        {-|
            Adds a feedback loop to a user interface component.

            This method is completely analogous to the 'Arrow.loop' method of 'ArrowLoop'. It is
            provided because not every instance of @UIComp@ is an arrow.
        -}
        loop :: uiComp item uiBackend era (i,feedback) (o,feedback) -> uiComp item uiBackend era i o

        -- |Converts a user interface component into a user interface circuit.
        toUICircuit :: uiComp item uiBackend era i o -> UICircuit item uiBackend era i o

        -- |Converts a user interface item into a user interface component.
        fromUIItem :: UIItem item uiBackend era i o -> uiComp item uiBackend era i o

    {-|
        Puts a circuit before a user interface component.

        This does not add any items to the user interface but may add data manipulation and control
        functionality. @(&#x3C;&#x3C;|)@ is equivalent to @flip ('|>>')@.
    -}
    (<<|) :: (UIComp uiComp)
          => uiComp item uiBackend era tmp o
          -> Circuit era i tmp
          -> uiComp item uiBackend era i o
    (<<|) = flip (|>>)

    {-|
        Puts a circuit after a user interface component.

        This does not add any items to the user interface but may add data manipulation and control
        functionality. @(|&#x3C;&#x3C;)@ is equivalent to @flip ('>>|')@.
    -}
    (|<<) :: (UIComp uiComp)
          => Circuit era tmp o
          -> uiComp item uiBackend era i tmp
          -> uiComp item uiBackend era i o
    (|<<) = flip (>>|)
