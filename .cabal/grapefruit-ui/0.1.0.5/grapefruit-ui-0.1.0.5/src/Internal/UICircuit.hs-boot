module Internal.UICircuit (

    UICircuit

) where

#if MIN_VERSION_base(4,7,0)
    -- GHC >= 7.8
    type role UICircuit nominal nominal phantom nominal nominal
#endif
    data UICircuit item uiBackend era i o
