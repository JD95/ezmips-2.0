{
module Lexer.Token where
}

%wrapper "posn"

$digit = 0-9
$alpha = [a-zA-Z]

tokens :-

  $white+                       ;
  "--".*                        ;
  let                           { \_ s -> TokenLet }
  in                            { \_ s -> TokenIn }
  $digit+                       { \_ s -> TokenInt (read s) }
  \=                            { \_ s -> TokenEq }
  \+                            { \_ s -> TokenPlus }
  \-                            { \_ s -> TokenMinus }
  \*                            { \_ s -> TokenTimes }
  \/                            { \_ s -> TokenDiv }
  \(                            { \_ s -> TokenLParen }
  \)                            { \_ s -> TokenRParen }
  $alpha [$alpha $digit \_ \']* { \_ s -> TokenSym s }
{
-- The token type:
data Token = TokenLet
           | TokenIn
           | TokenInt Int
           | TokenSym String
           | TokenEq
           | TokenPlus
           | TokenMinus
           | TokenTimes
           | TokenDiv
           | TokenLParen
           | TokenRParen
           deriving (Eq,Show)

}
