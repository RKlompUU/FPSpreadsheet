{
module Src.Lambda.ExprLexer where
}

%wrapper "monad"

$digit = [0-9]
$ident = [a-zA-Z\+\-]

tokens :-
  $white+                  ;
  "."                      { \s -> TPeriod }
  \\                       { \s -> TLambda }
  "("                      { \s -> TParenOpen }
  ")"                      { \s -> TParenClose }
  "let"                    { \s -> TLet }
  "in"                     { \s -> TIn }
  $ident($ident | $digit)* { \s -> TVar s }
  $digit+                  { \s -> TInt s }
  "="                      { \s -> TIs }
  ";"                      { \s -> TSep }
{

-- Identifier
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

alexEOF = undefined
}
