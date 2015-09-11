{
module Src.Lambda.ExprParser where

import Src.Lambda.ExprLexer

import Data.List
import Data.Char
import Text.PrettyPrint.HughesPJ (Doc, renderStyle, style, text, (<>), (<+>), parens)
}

%name parseLambdaExpressions
%tokentype { Token }
%error { parseError }

%token
  '.'     { TPeriod }
  '\\'    { TLambda }
  '('     { TParenOpen }
  ')'     { TParenClose }
  ident   { TVar $$ }

%%

lc : var        { $1 }
   | lam        { $1 }
   | app        { $1 }
   | '(' lc ')' { $2 }

lcRec : var         { $1 }
      | '(' lc ')'  { $2 }

var : ident { Var $1 }

lam : '\\' ident '.' lc { Lam $2 $4 }

app : lcRec lcRec { App $1 $2 }


{
type Ident = String

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

ppLC :: (Show v) => Int -> LC v -> Doc
ppLC _ (CInt i) = text $ show i
ppLC _ (Var v) = text $ show v
ppLC p (Lam v e) = pparens (p>0) $ text ("\\" ++ show v ++ ".") <> ppLC 0 e
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
