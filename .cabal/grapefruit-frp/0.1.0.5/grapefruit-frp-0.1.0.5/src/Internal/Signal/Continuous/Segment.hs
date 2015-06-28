{-# OPTIONS_GHC -fno-cse #-}
module Internal.Signal.Continuous.Segment (

    CSeg,
    currentValCapsule,
    producer

) where

    -- Control
    import Control.Applicative     as Applicative
    import Control.Arrow           as Arrow
    import Control.Compose         as Compose
    import Control.Concurrent.MVar as MVar

    -- Data
    import Data.Unique as Unique

    -- System
    import System.IO.Unsafe as UnsafeIO

    -- Internal
    import Internal.Signal.Discrete.Capsule as Capsule
    import Internal.Circuit                 as Circuit

    -- FRP.Grapefruit
    import FRP.Grapefruit.Circuit as Circuit

    newtype CSeg val = CSeg (((->) Unique :. Capsule) val) deriving (Functor, Applicative)

    currentValCapsule :: Unique -> CSeg val -> Capsule val
    currentValCapsule currentTimeID (CSeg capsuleGen) = unO capsuleGen currentTimeID

    producer :: IO val -> Circuit era () (CSeg val)
    producer readVal = proc _ -> do
                           maybeValVar <- act -< newMVar Nothing
                           addECFinalizer <- getECFinalizerAdd -< ()
                           returnA -< CSeg $
                                      O (unsafeCurrentValCapsule readVal maybeValVar addECFinalizer)

    {-# NOINLINE unsafeCurrentValCapsule #-}
    unsafeCurrentValCapsule :: IO val
                            -> MVar (Maybe val)
                            -> (IO () -> IO ())
                            -> Unique
                            -> Capsule val
    unsafeCurrentValCapsule readVal maybeValVar addECFinalizer timeID = unsafePerformIO $
                                                                        seq timeID $
                                                                        getCurrentValCapsule where

        getCurrentValCapsule = do
                                   maybeVal <- takeMVar maybeValVar
                                   case maybeVal of
                                       Nothing            -> do
                                                                 val <- readVal
                                                                 putMVar maybeValVar (Just val)
                                                                 addECFinalizer resetMaybeValVar
                                                                 return (Applicative.pure val)
                                       justVal@(Just val) -> do
                                                                 putMVar maybeValVar justVal
                                                                 return (Applicative.pure val)

        resetMaybeValVar     = do
                                   takeMVar maybeValVar
                                   putMVar maybeValVar Nothing
