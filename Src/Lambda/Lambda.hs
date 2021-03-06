{-|
Module      : Lambda.Lambda
Description : Experimental application of the API
Stability   : experimental

-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module Lambda.Lambda
  ( module Lambda.Lambda
  , toIdInt, fromIdInt
  , LC(..) ) where

import Data.List
import Lambda.ExprParser
import Lambda.ExprLexer
import Lambda.IdInt
import Debug.Trace

import Control.Monad.Reader

import API.SheetAbstr

import qualified Data.Map as Map

instance Var String where

instance Expr (LC String) String (Reader (Env String (LC String))) where
  evalExpr e =
    do
      env <- ask
      return (fromIdInt $ nf $ toIdInt $ addCellRefs (Map.assocs env) e)
-- lambda {lExpr_ = fromIdInt $ nf $ toIdInt $ addCellRefs (map (\(v,l) -> (v,lExpr_ l)) $ Map.assocs vars) e}

-- | Translates a 'Pos' reference to a 'String' reference representation.
cRefPos2Var :: Pos -> String
cRefPos2Var (r,c) = trace ("Getting col: " ++ show c) show r ++ colRefs !! c

-- | 'parseExpr' combines the lexer and parser stages. If both succeed it
-- returns an 'LC String'.
parseExpr :: String -> Maybe (LC String)
parseExpr str = scanExpr str >>= parseLambdaExpression

-- | 'addCellRefs' adds global variables to an expression.
addCellRefs :: [(String, LC String)] -> LC String -> LC String
addCellRefs rs = cVar2Vars . transformLet rs

-- | 'cVar2Vars' translates all 'CVar' encoded references to 'Var String'
-- encoded references in an expression.
cVar2Vars :: LC String -> LC String
cVar2Vars (CVar p) = Var $ cRefPos2Var p
cVar2Vars (Lam x e)    = Lam x (cVar2Vars e)
cVar2Vars (App e1 e2)  = App (cVar2Vars e1) (cVar2Vars e2)
cVar2Vars e = e

-- Heavily inspired by Lennar Augustsson's paper "Lamda Calculus Cooked Four Ways"

-- | Evaluate an 'LC' expression to its normal form
nf :: LC IdInt -> LC IdInt
nf e@(CInt _) = e
nf e@(Var _) = e
nf (Lam x e) = Lam x (nf e)
nf (App (Var (IdInt 0)) a) = unchurchInt (nf a)
nf (App (Var (IdInt 1)) a) = unchurchList (nf a)
nf (App f a) =
  case whnf f of
    Lam x b -> nf (subst x a b)
    f' -> App (nf f') (nf a)

-- | Evaluates an 'LC' expression to its weak head normal form
whnf :: LC IdInt -> LC IdInt
whnf e@(CInt _) = e
whnf e@(Var _) = e
whnf e@(Lam _ _) = e
whnf (App f a) =
  case whnf f of
    Lam x b -> whnf (subst x a b)
    f' -> App f' a

-- | Substitute a variable 'x' with an expression 's'.
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

-- | Get a fresh variable
newId :: [IdInt] -> IdInt
newId vs = head ([firstBoundId ..] \\ vs)
