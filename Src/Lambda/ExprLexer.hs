{-|
Module      : Lambda.ExprLexer
Description : Lambda calculus lexer
Stability   : experimental

Convert raw text (:: String) to Lambda calculus tokens, that can subsequently be parsed by the Lambda.ExprParser module.
-}
module Lambda.ExprLexer where

import Prelude hiding ((*>), (<*), (<$))

import ParseLib.Abstract
import Data.Char
import Data.Maybe

import Control.Monad
import Data.List

type TIdent = String

data Token =
    TPeriod
  | TLambda
  | TParenOpen
  | TParenClose
  | TVar TIdent
  | TInt Int
  | TLet
  | TIs
  | TIn
  | TSep
  | TCellRef (Int, Int)
  deriving (Eq, Show)

-- | Lexes a lambda calculus expression
scanExpr :: String -> Maybe [Token]
scanExpr str = listToMaybe [p | (p, []) <- parse lTokens str]

lTokens :: Parser Char [Token]
lTokens = many (lexWhiteSpace *> lToken <* lexWhiteSpace)

-- | Mappings of tokens to their corresponding @String@ counterparts
terminals :: [(Token, String)]
terminals =
    [ ( TParenOpen     , "("      )
    , ( TParenClose    , ")"      )
    , ( TLambda        , "\\"     )
    , ( TPeriod        , "."      )
    , ( TIn            , "in"      )
    , ( TLet           , "let"      )
    , ( TIs            , "="      )
    , ( TSep           , ";"      )
    ]

lToken :: Parser Char Token
lToken =  lTerminal
      <|> lVar
      <|> lCellRef
      <|> lInt

lTerminal :: Parser Char Token
lTerminal = choice [t <$ token s | (t,s) <- terminals]

lVar :: Parser Char Token
lVar = TVar <$> lexLowerId

lInt :: Parser Char Token
lInt = TInt <$> natural

lexWhiteSpace :: Parser Char String
lexWhiteSpace = greedy (satisfy isSpace)

lexLowerId :: Parser Char String
lexLowerId = (:) <$> satisfy isAlpha <*> greedy (satisfy isAlphaNum)

lCellRef :: Parser Char Token
lCellRef = (\a b -> TCellRef (a,b)) <$> integer <*> (colRef2Int <$> some (satisfy isUpper))

colRefs :: [String]
colRefs = [1..] >>= flip replicateM ['A'..'Z']

colRef2Int :: String -> Int
colRef2Int r = fromJust $ findIndex (==r) colRefs
