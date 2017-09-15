{
module Lexer.Token where
import Data.Monoid
import Data.String.Conv
import qualified Text.Read as R
}

%wrapper "monad-bytestring"

$digit = 0-9
$alpha = [a-zA-Z]

tokens :-

  $white+ ;
  "--".* ;
  
  $digit+ { usingInput TokenInt }
  if                            { ignoreInput TokenIf }
  while                         { ignoreInput TokenWhile }
  else                          { ignoreInput TokenElse }
  \=                            { ignoreInput TokenAssign }
  \=\=                          { ignoreInput TokenEq }
  \&\&                          { ignoreInput TokenAnd }
  \|\|                          { ignoreInput TokenOr }
  \<                            { ignoreInput TokenLT }
  \>                            { ignoreInput TokenGT }
  \!                            { ignoreInput TokenNot }
  \+                            { ignoreInput TokenPlus }
  \-                            { ignoreInput TokenMinus }
  \*                            { ignoreInput TokenTimes }
  \/                            { ignoreInput TokenDiv }
  \(                            { ignoreInput TokenLParen }
  \)                            { ignoreInput TokenRParen }
  \{                            { ignoreInput TokenLCurly }
  \}                            { ignoreInput TokenRCurly }
  $alpha [$alpha $digit \_ \']* { usingInput' TokenSym }
  \;	 	 	   	{ ignoreInput TokenSemiColon }
{

usingInput f (p,_,s,_) l = do
  case (R.readMaybe . toS . ByteString.take l $ s) of 
      Just i -> pure $ L p (f i)
      Nothing -> alexError $ "Couldn't parse from " ++ toS s
  
usingInput' f (p,_,s,_) l = pure $ L p (f (ByteString.take l s))
ignoreInput f (p,_,s,_) l = pure $ L p f

alexEOF = pure EOF
lexwrap = (alexMonadScan >>=)

parseError :: Lexeme -> Alex a	
parseError _ = alexError "Why is using happy and alex so hard"

scanner str = runAlex

data Lexeme = L AlexPosn Token | EOF deriving (Show)

-- The token type:
data Token = TokenIf
           | TokenWhile
     	   | TokenElse
           | TokenIn
           | TokenInt Int
           | TokenSym ByteString.ByteString
           | TokenAssign
           | TokenPlus
           | TokenMinus
           | TokenTimes
           | TokenDiv
	   | TokenAnd
	   | TokenOr
	   | TokenLT
	   | TokenGT
	   | TokenEq
	   | TokenNot
           | TokenLParen
           | TokenRParen
	   | TokenLCurly
	   | TokenRCurly
	   | TokenSemiColon
           deriving (Eq,Show)

}
