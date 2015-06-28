module Internal.Signal.Discrete.Capsule (

    Capsule (Capsule)

) where

    -- Control
    import Control.Applicative as Applicative

    -- Don’t use newtype since this would defeat the purpose of Capsule.
    data Capsule val = Capsule val

    instance Functor Capsule where

        fmap fun (Capsule val) = Capsule (fun val)

    instance Applicative Capsule where

        pure = Capsule

        Capsule fun <*> Capsule arg = Capsule (fun arg)
