{-# LANGUAGE GADTs, KindSignatures, RankNTypes, FlexibleContexts, TypeOperators, NoMonomorphismRestriction, TypeFamilies #-}
module Src.Lambda.Lambda where

data HFix f a = HIn { hout :: f (HFix f) a }
type ExpT' = HFix ExpTF
data ExpTF :: (* -> *) -> * -> * where
  TI      :: Int    -> ExpTF a Int
  TB      :: Bool   -> ExpTF a Bool
  TGT     :: a Int  -> a Int        -> ExpTF a Bool
  TEQ     :: a Int  -> a Int        -> ExpTF a Bool
  TSucc   :: a Int  -> ExpTF a Int
  TAdd    :: a Int  -> a Int        -> ExpTF a Int
  TIf     :: a Bool -> a b          -> a b          -> ExpTF a b

class HFunctor f where
  hfmap :: (forall b. g b -> h b) -> f g a -> f h a

hfold :: HFunctor f => (forall b. f r b -> r b) -> HFix f a -> r a
hfold f = f.hfmap (hfold f) . hout

newtype Id a = Id { unId :: a }

evalT' :: ExpT' a -> a
evalT' = unId . hfold evalAlgT

evalAlgT :: ExpTF Id a -> Id a
evalAlgT (TI i) = Id $ i
evalAlgT (TB b) = Id $ b
evalAlgT (TGT e1 e2) | unId e1 > unId e2 = Id $ True
                     | otherwise         = Id $ False
evalAlgT (TIsZero e) | unId e == 0 = Id $ True
                     | otherwise   = Id $ False
evalAlgT (TSucc e) = Id $ unId e + 1
evalAlgT (TAdd e1 e2) = Id $ unId e1 + unId e2
evalAlgT (TIf e1 e2 e3) | unId e1   = e2
                        | otherwise = e3

instance HFunctor ExpTF where
  hfmap eval (TI i) = TI i
  hfmap eval (TB b) = TB b
  hfmap eval (TGT e1 e2) = TGT (eval e1) (eval e2)
  hfmap eval (TIsZero e) = TIsZero (eval e)
  hfmap eval (TSucc e) = TSucc (eval e)
  hfmap eval (TAdd e1 e2) = TAdd (eval e1) (eval e2)
  hfmap eval (TIf e1 e2 e3) = TIf (eval e1) (eval e2) (eval e3)
