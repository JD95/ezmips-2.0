{
module Lexer.Token where
import Data.Monoid
import Data.String.Conv
}

%wrapper "monad-bytestring"

$digit = 0-9
$alpha = [a-zA-Z]

tokens :-

  $white+                       ;
  "--".*                        ;
  $digit+                       { usingInput TokenInt (read . toS) }
  \=                            { ignoreInput TokenEq }
  \+                            { ignoreInput TokenPlus }
  \-                            { ignoreInput TokenMinus }
  \*                            { ignoreInput TokenTimes }
  \/                            { ignoreInput TokenDiv }
  \(                            { ignoreInput TokenLParen }
  \)                            { ignoreInput TokenRParen }
  $alpha [$alpha $digit \_ \']* { usingInput' TokenSym }
{

data Lexeme = L AlexPosn Token | EOF deriving (Show)

usingInput f g (p,_,s,_) _ = pure $ L p (f . g $ s)
usingInput' f (p,_,s,_) _ = pure $ L p (f s)
ignoreInput f (p,_,s,_) _ = pure $ L p f

alexEOF = pure EOF
lexwrap = (alexMonadScan >>=)

parseError :: Token -> Alex a
parseError _ = alexError "Why is using happy and alex so hard"

-- The token type:
data Token = TokenLet
           | TokenIn
           | TokenInt Int
           | TokenSym ByteString.ByteString
           | TokenEq
           | TokenPlus
           | TokenMinus
           | TokenTimes
           | TokenDiv
           | TokenLParen
           | TokenRParen
           deriving (Eq,Show)

}
