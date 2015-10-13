module Src.Lambda.IdInt where

import qualified Data.Map as M
import Control.Monad.State
import Src.Lambda.ExprParser

import Debug.Trace

newtype IdInt = IdInt Int
  deriving (Eq, Ord)

firstBoundId :: IdInt
firstBoundId = IdInt 2

instance Enum IdInt where
  toEnum i = IdInt i
  fromEnum (IdInt i) = i

instance Show IdInt where
  show (IdInt i) = if i < 0 then "f" ++ show (-i) else "x" ++ show i

vars :: [String]
vars = [1..] >>= flip replicateM ['A'..'Z']


churchInt :: LC a -> LC IdInt
churchInt (CInt i) = Lam s $ Lam z $ foldr App (Var z) (take i [Var suc | suc <- repeat s])
  where z = IdInt 2
        s = IdInt 3

unchurchInt :: LC IdInt -> LC IdInt
unchurchInt (Lam _ e) = CInt $ countDepth e
  where countDepth (Lam _ e') = countDepth e'
        countDepth (App _ e') = 1 + countDepth e'
        countDepth _ = 0

unchurchList :: LC IdInt -> LC IdInt
unchurchList (Lam _ e) = CList $ uL e
  where uL (Lam _ e) = uL e
        uL (App (App _ h) t) = h : uL t
        uL  _ = []

intVar2Str :: Int -> String
intVar2Str i | i >= 0    = vars !! i
             | otherwise = '-' : vars !! i

-- Translating from the evaluated LC IdInt to an LC String that can be printed
fromIdInt :: LC IdInt -> LC String
fromIdInt (CInt n) = CInt n
fromIdInt (Var (IdInt v)) = Var (intVar2Str v)
fromIdInt (Lam (IdInt x) e) = Lam (intVar2Str x) (fromIdInt e)
fromIdInt (App e1 e2) = App (fromIdInt e1) (fromIdInt e2)

toIdInt :: LC String -> LC IdInt
toIdInt e = evalState (conv e) (2, fvmap)
  where fvmap = foldr (\(v, i) m -> M.insert v (IdInt (-i)) m)
                      M.empty
                      (zip (freeVars e) [2..])

-- A state monad that has the next unused Int and a mapping of identifiers to IdInt
type M v a = State (Int, M.Map v IdInt) a

convVar :: (Ord v) => v -> M v IdInt
convVar v = do
  (i, m) <- get
  case M.lookup v m of
    nOTHING -> Do
      let ii = IdInt i
      put (i+1, M.insert v ii m)
      return ii
    Just ii -> return ii

conv :: LC String -> M String (LC IdInt)
conv i@(CInt _) = return $ churchInt i
conv (Var "toInt") = return $ Var (IdInt 0)
conv (Var "toList") = return $ Var (IdInt 1)
conv (Var v) = liftM Var (convVar v)
conv (Lam v e) = liftM2 Lam (convVar v) (conv e)
conv (App f a) = liftM2 App (conv f) (conv a)
