-- |A setup describes how to initialize and finalize a reactive system.
module FRP.Grapefruit.Setup (

    Setup,
    setup,
    fromIO,
    run

) where

    -- Control
    import Control.Arrow   as Arrow
    import Control.Compose as Compose

    -- Data
    import Data.Monoid as Monoid

    {-|
        A setup describes the initialization and finalization of a reactive system. It is equivalent
        to an action of type @IO (IO ())@ which initializes the system and returns a finalization
        action.

        The 'mempty' method of the 'Monoid' instance denotes a setup which does no initialization
        and no finalization.  The 'mappend' method sequences initialization and finalization
        actions.
    -}
    newtype Setup = Setup ((IO :. IO) :$ ()) deriving (Monoid)

    -- |Converts an I/O action into a setup.
    setup :: IO (IO ()) -> Setup
    setup = Setup . App . O

    -- |Forms an initialization-only setup from an I/O action.
    fromIO :: IO () -> Setup
    fromIO = setup . (>> return (return ()))

    -- |Converts a setup into an I/O action.
    run :: Setup -> IO (IO ())
    run (Setup io) = unO (unApp io)
