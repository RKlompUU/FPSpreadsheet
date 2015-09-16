{
module Src.Lambda.ExprParser where

import Src.Lambda.ExprLexer

import Data.List
import Data.Char
import Text.PrettyPrint.HughesPJ (Doc, renderStyle, style, text, (<>), (<+>), parens)
}

%name parseLambdaExpression
%tokentype { Token }
%error { parseError }

%token
  '.'     { TPeriod }
  '\\'    { TLambda }
  '('     { TParenOpen }
  ')'     { TParenClose }
  ident   { TVar $$ }
  digit   { TInt $$ }
  "let"   { TLet }
  "="     { TIs }
  "in"    { TIn }
  ';'     { TSep }

%%

lc : con        { $1 }
   | var        { $1 }
   | lam        { $1 }
   | apps       { $1 }
   | let        { $1 }
   | '(' lc ')' { $2 }

lcRec : con         { $1 }
      | var         { $1 }
      | '(' lc ')'  { $2 }

con : digit { CInt (read $1) }

var : ident { Var $1 }

lam : '\\' idents ident '.' lc { foldFuncArgs2Lams ($3 : $2) $5 }

idents : {-empty-}    { [] }
       | idents ident { $2 : $1 }

apps : app      { $1 }
     | apps lcRec { App $1 $2 }

app : lcRec lcRec { App $1 $2 }

let : "let" letEntrys "in" lc { transformLet $2 $4 }

letEntrys : letEntry               { [$1] }
          | letEntrys ';' letEntry { $3 : $1 }

letEntry : ident "=" lc { ($1, $3) }

{
type IdentTy = String


foldFuncArgs2Lams :: [IdentTy] -> LC IdentTy -> LC IdentTy
foldFuncArgs2Lams args expr = foldl arg2Lam expr args
  where arg2Lam expr arg = Lam arg expr

transformLet :: [(IdentTy, LC IdentTy)] -> LC IdentTy -> LC IdentTy
transformLet vars inExpr = foldl let2LamApp inExpr vars
  where let2LamApp inExpr (var, expr) = App (Lam var inExpr) expr

data LC v = CInt Int
          | Var v
          | Lam v (LC v)
          | App (LC v) (LC v)
          deriving (Eq)

parseError :: [Token] -> a
parseError ts = error "Parse error, [Token]: " (show ts)


freeVars :: Eq v => LC v -> [v]
freeVars (CInt _) = []
freeVars (Var v) = [v]
freeVars (Lam v e) = freeVars e \\ [v]
freeVars (App f a) = freeVars f `union` freeVars a

allVars :: Eq v => LC v -> [v]
allVars (CInt _) = []
allVars (Var v) = [v]
allVars (Lam _ e) = allVars e
allVars (App f a) = allVars f `union` allVars a


instance (Show v) => Show (LC v) where
  show = renderStyle style . ppLC 0


quotelessShow :: Show a => a -> String
quotelessShow = filter (/= '\"') . show

ppLC :: (Show v) => Int -> LC v -> Doc
ppLC _ (CInt i) = text $ show i
ppLC _ (Var v) = text $ quotelessShow v
ppLC p (Lam v e) = pparens (p>0) $ text ("\\" ++ quotelessShow v ++ ".") <> ppLC 0 e
ppLC p (App f a) = pparens (p>1) $ ppLC 1 f <+> ppLC 2 a

pparens :: Bool -> Doc -> Doc
pparens True d = parens d
pparens False d =  d

newtype Id = Id String
  deriving (Eq, Ord)

instance Show Id where
  show (Id i) = i

instance Read Id where
  readsPrec _ s =
    case span isAlphaNum s of
      ("", _) -> []
      (i, s') -> [(Id i, s')]

}
