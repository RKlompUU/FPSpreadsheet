{
module Src.Lambda.ExprLexer where
}

%wrapper "basic"

$digit = [0-9]
$ident = [a-zA-Z\+\-]

tokens :-
  $white+                  ;
  "."                      { \s -> TPeriod }
  \\                       { \s -> TLambda }
  "("                      { \s -> TParenOpen }
  ")"                      { \s -> TParenClose }
  $ident($ident | $digit)* { \s -> TVar s }
  $digit+                  { \s -> TInt s }
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
  deriving (Eq, Show)
}
