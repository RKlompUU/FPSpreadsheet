{-|
Module      : Lambda.IdInt
Description : LC String -> LC IdInt and vice versa
Stability   : experimental

This module supplies functions for translating an 'LC String' to an 'LC IdInt' and vice versa.
Currently, 'LC String' expressions are parsed from user written code, but the function
that reduces an expression to its normal form can only reduce 'LC IdInt' expressions.
-}
module Lambda.IdInt where

import qualified Data.Map as M
import Control.Monad.State
import Lambda.ExprParser

import Debug.Trace

newtype IdInt = IdInt Int
  deriving (Eq, Ord)

-- | The first 'IdInt' that may be used for fresh variables.
-- Variables 0 and 1 are reserved for the internally defined
-- \'toInt\' and \'toList\' LC functions respectively.
firstBoundId :: IdInt
firstBoundId = IdInt 2

instance Enum IdInt where
  toEnum i = IdInt i
  fromEnum (IdInt i) = i

instance Show IdInt where
  show (IdInt i) = if i < 0 then "f" ++ show (-i) else "x" ++ show i

-- | 'vars' builds an infinite list of unique strings:
-- [\"A\",\"B\", .. , \"Z\", \"AA\", \"AB\", ..]
vars :: [String]
vars = [1..] >>= flip replicateM ['A'..'Z']

-- | 'churchInt' translates an integer to its
-- lambda expression equivalent church representation.
churchInt :: LC a -> LC IdInt
churchInt (CInt i) = Lam s $ Lam z $ foldr App (Var z) (take i [Var suc | suc <- repeat s])
  where z = IdInt 2
        s = IdInt 3

-- | 'unchurchInt' translates a lambda expression that represents a church
-- numeral to an integer. Note that this function is
-- not safe, if an expression is given that does not represent a church
-- numeral, then haskell will crash due to an uncatched pattern match.
unchurchInt :: LC IdInt -> LC IdInt
unchurchInt (Lam _ e) = CInt $ countDepth e
  where countDepth (Lam _ e') = countDepth e'
        countDepth (App _ e') = 1 + countDepth e'
        countDepth _ = 0

-- | 'unchurchList' translates a lambda expression that represents a church
-- list to a list. Note that this function is not safe, if an expression is
-- given that does not represent a church list, then haskell will crash due
-- to an uncatched pattern match.
unchurchList :: LC IdInt -> LC IdInt
unchurchList (Lam _ e) = CList $ uL e
  where uL (Lam _ e) = uL e
        uL (App (App _ h) t) = h : uL t
        uL  _ = []

-- | 'intVar2Str' maps integers to Strings in a unique way.
--
-- @
-- intVar2Str i = r
-- @
-- , for every r there is only 1 i (and vice versa but that is obvious
-- since this function is pure)
intVar2Str :: Int -> String
intVar2Str i | i >= 0    = vars !! i
             | otherwise = '-' : vars !! i

-- | 'fromIdInt' translates an 'LC IdInt' to an 'LC String'.
-- This step doesn't have to take showing of variables in regard since
-- a normal form of a no-shadowed-variables-'LC IdInt' will not contain any
-- shadowed variables either.
fromIdInt :: LC IdInt -> LC String
fromIdInt (CInt n) = CInt n
fromIdInt (Var (IdInt v)) = Var (intVar2Str v)
fromIdInt (Lam (IdInt x) e) = Lam (intVar2Str x) (fromIdInt e)
fromIdInt (App e1 e2) = App (fromIdInt e1) (fromIdInt e2)

-- | 'fromIdInt' translates an 'LC String' to an 'LC IdInt'.
-- This step fixes shadowing of variables.
toIdInt :: LC String -> LC IdInt
toIdInt e = evalState (conv e) (2, fvmap)
  where fvmap = foldr (\(v, i) m -> M.insert v (IdInt (-i)) m)
                      M.empty
                      (zip (freeVars e) [2..])

-- | A state monad that has the next unused 'Int' and a mapping of identifiers to 'IdInt'
type M v a = State (Int, M.Map v IdInt) a

-- | 'convVar' translates (possibly shadowed) 'String' variable names to
-- unshadowed 'IdInt' variable names.
convVar :: (Ord v) => v -> M v IdInt
convVar v = do
  (i, m) <- get
  case M.lookup v m of
    Nothing -> do
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
