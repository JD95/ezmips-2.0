{

module Parser.Grammar (Exp_ (..), parseExp, Exp) where
  
import Prelude hiding (GT, LT, EQ)
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
'=' { L _ TokenAssign }
"==" { L _ TokenEq }
'<' { L _ TokenLT }
'>' { L _ TokenGT }
'!' { L _ TokenNot }
"&&" { L _ TokenAnd }
"||" { L _ TokenOr }
if  { L _ TokenIf }
else { L _ TokenElse }
while { L _ TokenWhile }

%right in
%nonassoc '>' '<'
%left '+' '-'
%left '*' '/'
%left NEG

%%

Stmts : Stmts S         { $2 : $1 }
      | Stmts ';'           { $1 }
      | S                   { [$1] }
      | {- empty -}         { [] }

S : if '(' Exp ')' '{' Stmts '}' else S { Fix (If $3 $6 (Just $9)) }
  | if '(' Exp ')' '{' Stmts '}'          { Fix (If $3 $6 Nothing) }
  | while '(' Exp ')' '{' Stmts '}'       { Fix (While $3 $6) }
  | Exp ';'                             { Fix (Stmt $1) }

Exp : Exp '=' Exp            { Fix (Assign $1 $3) }
    | Exp "==" Exp           { Fix (EQ $1 $3) }
    | Exp "&&" Exp           { Fix (And $1 $3) }
    | Exp "||" Exp           { Fix (Or $1 $3) }
    | Exp '<' Exp            { Fix (LT $1 $3) }
    | Exp '>' Exp            { Fix (GT $1 $3) }
    | '!' Exp                { Fix (Not $2) }
    | Exp '+' Exp            { Fix (Plus $1 $3) }
    | Exp '-' Exp            { Fix (Minus $1 $3) }
    | Exp '*' Exp            { Fix (Times $1 $3) }
    | Exp '/' Exp            { Fix (Div $1 $3) }
    | '(' Exp ')'            { $2 }
    | '-' Exp %prec NEG      { Fix (Negate $2) }
    | int                    { Fix (Int $1) }
    | sym sym                { Fix (Decl (Sym_ $1) (Sym_ $2)) }
    | sym                    { Fix (Sym $1) }
{
  parseExp str = runAlex str parseGrammar
}
