module Internal.UIItem (

    UIItem (UIItem),
    Placement,
    Widget,
    Window

) where

    import {-# SOURCE #-} Internal.UICircuit as UICircuit

#if MIN_VERSION_base(4,7,0)
    -- GHC >= 7.8
    type role UIItem nominal nominal phantom nominal nominal
#endif
    newtype UIItem item uiBackend era i o = UIItem (UICircuit item uiBackend era i o)

    type family Placement item uiBackend :: *

    data Widget

    data Window
