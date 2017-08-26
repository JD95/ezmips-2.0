{

module Parser.Grammar (Exp_ (..), parseCalc, Exp) where
  
import Lexer.Token
import Data.Functor.Foldable
import Parser.AST    
  
  
}

%name parseGrammar
%lexer {lexwrap} {EOF}
%monad {Alex}
%error { parseError }
%tokentype { Lexeme }

%token
    let { TokenLet }
    in  { TokenIn }
    int { TokenInt $$ }
    var { TokenSym $$ }
    '=' { TokenEq }
    '+' { TokenPlus }
    '-' { TokenMinus }
    '*' { TokenTimes }
    '/' { TokenDiv }
    '(' { TokenLParen }
    ')' { TokenRParen }

%right in
%nonassoc '>' '<'
%left '+' '-'
%left '*' '/'
%left NEG

%%

Exp : Exp '+' Exp            { Fix (Plus $1 $3) }
    | Exp '-' Exp            { Fix (Minus $1 $3) }
    | Exp '*' Exp            { Fix (Times $1 $3) }
    | Exp '/' Exp            { Fix (Div $1 $3) }
    | '(' Exp ')'            { $2 }
    | '-' Exp %prec NEG      { Fix (Negate $2) }
    | int                    { Fix (Int $1) }

{

parseError :: [Token] -> a
parseError _ = error "Parse error"
  
}
