{
module Lexer(
  scanTokens,
  Token(..),
) where

import Data.Char(chr)
}

%wrapper "basic"

$digit       = [0-9]   
$lower       = [a-z]       
$upper       = [A-Z]    
$ln          = [\n\r]
$special     = [\\'ntrf]
$graphic     = $printable # $white
@escape      = \\ $special
@charlit     = \' (($graphic # $special) | @escape) \'

tokens :-
  "--"\-*.*                    ;
  $white                       ;
  fun                          {const TokenFun}
  data                         {const TokenData}
  class                        {const TokenClass}
  instance                     {const TokenInstance}
  let                          {const TokenLet}
  in                           {const TokenIn}
  if                           {const TokenIf}
  then                         {const TokenThen}
  else                         {const TokenElse}
  case                         {const TokenCase}
  of                           {const TokenOf}
  forall                       {const TokenForall}
  $digit+                      {\s -> TokenNum (read s)}
  \?                           {const TokenDyn}
  \->                          {const TokenArrow}
  \=>                          {const TokenDoubleArrow}
  \=                           {const TokenBind}
  \+                           {const TokenAdd}
  \-                           {const TokenSub}
  \*                           {const TokenMul}
  \/                           {const TokenDiv}
  \++                           {const TokenAppend}
  \<                           {const TokenLt}
  \>                           {const TokenGt}
  \<=                          {const TokenLe}
  \>=                          {const TokenGe}
  \==                          {const TokenEqu}
  \/=                          {const TokenNeq}
  \&&                          {const TokenAnd}
  \|\|                         {const TokenOr}
  \|                           {const TokenAlt}
  \!                           {const TokenBang}
  \(                           {const TokenLParen}
  \)                           {const TokenRParen}
  \{                           {const TokenLCurly}
  \}                           {const TokenRCurly}
  \[                           {const TokenLBrack}
  \]                           {const TokenRBrack}
  \,                           {const TokenComma }
  \;                           {const TokenSemi}
  \.                           {const TokenDot}
  \::                          {const TokenAscribe}
  \:                           {const TokenColon}
  \@    { const TokenAt}
  \_    { const TokenWildcard}
  $upper [$lower $upper $digit \_]*   {\s -> TokenCon s}
  $lower [$lower $upper $digit \_]*   {\s -> TokenVar s}
  
{
-- Each action has type :: String -> Token

-- The token type
data Token = TokenFun
           | TokenData
           | TokenClass
           | TokenInstance
           | TokenLet
           | TokenIn
           | TokenIf
           | TokenThen
           | TokenElse
           | TokenComma
           | TokenLParen
           | TokenRParen
           | TokenTrue
           | TokenFalse
           | TokenVar String
           | TokenCon String
           | TokenBind
           | TokenAdd
           | TokenSub
           | TokenMul
           | TokenDiv
           | TokenLt
           | TokenGt
           | TokenLe
           | TokenGe
           | TokenEqu
           | TokenNeq
           | TokenAnd
           | TokenOr
           | TokenBang
           | TokenAssign
           | TokenArrow
           | TokenNum Integer
           | TokenDot
           | TokenForall
           | TokenCase | TokenOf | TokenDoubleArrow | TokenAlt | TokenLCurly | TokenRCurly
           | TokenLBrack | TokenRBrack | TokenAscribe | TokenColon | TokenAppend
           | TokenSemi | TokenAt | TokenWildcard | TokenString String
           | TokenDyn
           deriving (Show, Eq)

scanTokens = alexScanTokens

 
}
