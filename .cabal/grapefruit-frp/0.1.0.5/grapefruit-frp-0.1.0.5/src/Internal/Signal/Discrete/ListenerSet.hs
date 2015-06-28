module Internal.Signal.Discrete.ListenerSet (

    ListenerSet,
    empty,
    add,
    notify

) where

    -- Data
    import           Data.Map (Map)
    import qualified Data.Map       as Map
    import           Data.IORef     as IORef

    newtype ListenerSet = ListenerSet (Map Int (IO ()))

    empty :: ListenerSet
    empty = ListenerSet Map.empty

    add :: IORef ListenerSet -> IO () -> IO (IO ())
    add setRef listener = do
                              ListenerSet currentMap <- readIORef setRef
                              let

                                  newKey | Map.null currentMap = minBound
                                         | otherwise           = succ (fst (Map.findMax currentMap))

                              writeIORef setRef
                                         (ListenerSet $ Map.insert newKey listener currentMap)
                              return $ modifyIORef setRef
                                                   (\(ListenerSet map) -> ListenerSet $
                                                                          Map.delete newKey map)

    notify :: ListenerSet -> IO ()
    notify (ListenerSet map) = sequence_ (Map.elems map)
