module Src.Lambda.LambdaType where

-- Heavily inspired by Lennar Augustsson's paper "Lamda Calculus Cooked Four Ways"


import Data.List
import Data.Char
import Text.PrettyPrint.HughesPJ (Doc, renderStyle, style, text, (<>), (<+>), parens)
import Text.ParserCombinators.ReadP

data LC v = CInt Int
          | Var v
          | Lam v (LC v)
          | App (LC v) (LC v)
          deriving Eq

freeVars :: (Eq v) => LC v -> [v]
freeVars (Var v) = [v]
freeVars (Lam v e) = freeVars e \\ [v]
freeVars (App f a) = freeVars f `union` freeVars a

allVars :: (Eq v) => LC v -> [v]
allVars (Var v) = [v]
allVars (Lam _ e) = allVars e
allVars (App f a) = allVars f `union` allVars a

instance (Read v) => Read (LC v) where
  readsPrec _ = readP_to_S pLC

pLC, pLCAtom, pLCVar, pLCLam, pLCApp :: (Read v) => ReadP (LC v)
pLC = pLCLam +++ pLCApp +++ pLCLet

pLCVar = do
  v <- pVar
  return $ Var v

pLCLam = do
  schar '\\'
  v <- pVar
  schar '.'
  e <- pLC
  return $ Lam v e

pLCApp = do
  es <- many1 pLCAtom
  return $ foldl1 App es

pLCAtom = pLCVar +++ (do schar '('; e <- pLC; schar ')'; return e)

pLCLet :: (Read v) => ReadP (LC v)
pLCLet = do
  let lcLet (x,e) b = App (Lam x b) e
      pDef = do
        v <- pVar
        schar '='
        e <- pLC
        return (v, e)
  sstring "let"
  bs <- sepBy pDef (schar ';')
  sstring "in"
  e <- pLC
  return $ foldr lcLet e bs

schar :: Char -> ReadP Char
schar c = do skipSpaces; char c

sstring :: String -> ReadP String
sstring c = do skipSpaces; string c

pVar :: (Read v) => ReadP v
pVar = do skipSpaces; readS_to_P (readsPrec 9)

instance (Show v) => Show (LC v) where
  show = renderStyle style . ppLC 0

ppLC :: (Show v) => Int -> LC v -> Doc
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
