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
sym { L _ (TokenSym $$) }
'+' { L _ TokenPlus }
'-' { L _ TokenMinus }
'*' { L _ TokenTimes }
'/' { L _ TokenDiv }
'(' { L _ TokenLParen }
')' { L _ TokenRParen }
'{' { L _ TokenLCurly }
'}' { L _ TokenRCurly }
';' { L _ TokenSemiColon }
'=' { L _ TokenEq }
if  { L _ TokenIf }
else { L _ TokenElse }
while { L _ TokenWhile }

%right in
%nonassoc '>' '<'
%left '+' '-'
%left '*' '/'
%left NEG

%%

Exp : if '(' Exp ')' '{' Exp '}' { Fix (If $3 $6 Nothing) }
    | if '(' Exp ')' '{' Exp '}' else '{' Exp '}' { Fix (If $3 $6 (Just $10)) }
    | while '(' Exp ')' '{' Exp '}' { Fix (While $3 $6) }
| sym sym                { Fix (Decl (Sym_ $1) (Sym_ $2)) }
| Exp '=' Exp            { Fix (Assign $1 $3) }
    | Exp '+' Exp            { Fix (Plus $1 $3) }
    | Exp '-' Exp            { Fix (Minus $1 $3) }
    | Exp '*' Exp            { Fix (Times $1 $3) }
    | Exp '/' Exp            { Fix (Div $1 $3) }
    | Exp ';'                { Fix (Stmt $1 Nothing) }
    | Exp ';' Exp            { Fix (Stmt $1 (Just $3)) }
    | '(' Exp ')'            { $2 }
    | '-' Exp %prec NEG      { Fix (Negate $2) }
    | int                    { Fix (Int $1) }
    | sym                    { Fix (Sym $1) }

{
  parseExp str = runAlex str parseGrammar
}
