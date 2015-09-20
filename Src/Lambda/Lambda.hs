module Src.Lambda.Lambda
  ( module Src.Lambda.Lambda
  , toIdInt ) where

import Data.List
import Src.Lambda.ExprParser
import Src.Lambda.ExprLexer
import Src.Lambda.IdInt
import Debug.Trace

-- Heavily inspired by Lennar Augustsson's paper "Lamda Calculus Cooked Four Ways"

nf :: LC IdInt -> LC IdInt
nf e = nf' e
{-  = case nf' e of
      e'@(Var _) -> e'
      e'@(Lam _ _) -> e'
      (App (Var (IdInt 0)) a) -> unchurchInt a
      e'@(App _ _) -> e'
-}
nf' :: LC IdInt -> LC IdInt
nf' e@(CInt _) = e
nf' e@(Var _) = e
nf' (Lam x e) = Lam x (nf' e)
nf' (App (Var (IdInt 0)) a) = unchurchInt (nf' a)
nf' (App (Var (IdInt 1)) a) = unchurchList (nf' a)
nf' (App f a) =
  case whnf f of
    Lam x b -> nf' (subst x a b)
    f' -> App (nf' f') (nf' a)

whnf :: LC IdInt -> LC IdInt
whnf e@(CInt _) = e
whnf e@(Var _) = e
whnf e@(Lam _ _) = e
--whnf (App (Var (IdInt 0)) a) = trace "1!" unchurchInt a
whnf (App f a) =
  case whnf f of
    Lam x b -> whnf (subst x a b)
    f' -> App f' a


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

subst :: IdInt -> LC IdInt -> LC IdInt -> LC IdInt
subst x s b = sub b
  where sub e@(CInt _) = e
        sub e@(Var v) | v == x = s
                      | otherwise = e
        sub e@(Lam v e') | v == x = e
                         | v `elem` fvs = Lam v' (sub e'')
                         | otherwise = Lam v (sub e')
                            where v' = newId vs
                                  e'' = subst v (Var v') e'
        sub (App f a) = App (sub f) (sub a)

        fvs = freeVars s
        vs = fvs `union` allVars b

newId :: [IdInt] -> IdInt
newId vs = head ([firstBoundId ..] \\ vs)

parseExpr :: String -> LC String
parseExpr str = parseLambdaExpression
              $ alexScanTokens str
