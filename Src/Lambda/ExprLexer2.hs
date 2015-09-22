module Src.Lambda.ExprLexer2 where

import ParseLib.Abstract
import Data.Char
import Data.Maybe

type TIdent = String

data Token =
    TPeriod
  | TLambda
  | TParenOpen
  | TParenClose
  | TVar TIdent
  | TInt TIdent
  | TLet
  | TIs
  | TIn
  | TSep
  deriving (Eq, Show)

scanExpr :: String -> Maybe [Token]
scanExpr str = listToMaybe [p | (p, []) <- parse lTokens str]

lTokens :: Parser Char [Token]
lTokens = many (lexWhiteSpace *> lToken <* lexWhiteSpace)

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

lTerminal :: Parser Char Token
lTerminal = choice [t <$ token s | (t,s) <- terminals]

lVar :: Parser Char Token
lVar = TVar <$> lexLowerId

lexWhiteSpace :: Parser Char String
lexWhiteSpace = greedy (satisfy isSpace)

lexLowerId :: Parser Char String
lexLowerId = (:) <$> satisfy isAlpha <*> greedy (satisfy isAlphaNum)
