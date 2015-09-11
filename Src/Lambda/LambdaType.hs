module Src.Lambda.LambdaType where

-- Heavily inspired by Lennar Augustsson's paper "Lamda Calculus Cooked Four Ways"


import Data.List
import Data.Char
import Text.PrettyPrint.HughesPJ (Doc, renderStyle, style, text, (<>), (<+>), parens)

import ParseLib.Abstract

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

--instance (Read v) => Read (LC v) where
--  readsPrec _ = readP_to_S pLC

pAnyLetter :: Parser Char Char
pAnyLetter = choice $ map symbol (['a'..'z'] ++ ['A'..'Z'])

pWSpace :: Parser Char String
pWSpace = greedy $ choice $ map symbol [' ', '\n', '\r', '\t']

parseLC :: String -> LC Char
parseLC = fst . head . parse (pLC <* eof)

pLC, pVar, pLam, pApp :: Parser Char (LC Char)
pLC =  pVar
   <|> bracketed pLam
   <|> braced pApp
   <|> parenthesised pLC

pVar = Var <$> (pWSpace *> pAnyLetter)

pLam = Lam <$> (symbol '\\' *> pWSpace *> pAnyLetter) <*> (symbol '.' *> pLC)

pApp = App <$> pLC <*> pLC


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
