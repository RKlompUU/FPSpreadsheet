{-FIXME:
    Once Haddock supports cross-package links to modules, make FRP.Grapefruit.Circuit below a
    hyperlink.
-}
{-|
    This module provides support for user interface circuits.

    UI circuits are systems of UI items (for example, widgets). They are similar to ordinary
    circuits as provided by FRP.Grapefruit.Circuit but have the additional feature of providing
    parts of user interfaces.
-}
module Graphics.UI.Grapefruit.Circuit (

    UICircuit,
    fromCircuit,
    run

) where

    -- Internal.Circuit
    import Internal.UICircuit as UICircuit
