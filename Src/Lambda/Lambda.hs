module Src.Lambda.Lambda where

import Data.List
import Src.Lambda.ExprParser
import Src.Lambda.IdInt

-- Heavily inspired by Lennar Augustsson's paper "Lamda Calculus Cooked Four Ways"

nf :: LC IdInt -> LC IdInt
nf e@(CInt _) = e
nf e@(Var _) = e
nf (Lam x e) = Lam x (nf e)
nf (App f a) =
  case whnf f of
    Lam x b -> nf (subst x a b)
    f' -> App (nf f') (nf a)

whnf :: LC IdInt -> LC IdInt
whnf e@(CInt _) = e
whnf e@(Var _) = e
whnf e@(Lam _ _) = e
whnf (App f a) =
  case whnf f of
    Lam x b -> whnf (subst x a b)
    f' -> App f' a

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
