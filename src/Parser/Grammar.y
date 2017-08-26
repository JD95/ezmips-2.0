{

module Parser.Grammar (Exp_ (..), parseExp, Exp) where
  
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
int { L _ (TokenInt $$) }
var { L _ (TokenSym $$) }
'=' { L _ TokenEq }
'+' { L _ TokenPlus }
'-' { L _ TokenMinus }
'*' { L _ TokenTimes }
'/' { L _ TokenDiv }
'(' { L _ TokenLParen }
')' { L _ TokenRParen }
';' { L _ TokenSemiColon }

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
    | Exp ';'                { Fix (Stmt $1 Nothing) }
    | Exp ';' Exp            { Fix (Stmt $1 (Just $3)) }
    | '(' Exp ')'            { $2 }
    | '-' Exp %prec NEG      { Fix (Negate $2) }
    | int                    { Fix (Int $1) }

{
  parseExp str = runAlex str parseGrammar
}
