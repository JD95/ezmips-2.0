module TypeCheck.Infer where

import           Control.Comonad.Cofree
import           Data.Functor.Foldable
import           Prelude                ()
import           Protolude

import           Parser.AST

data IType
  = IInt
  | IVoid
    deriving (Show, Eq)

data IError
  = WrongType IType IType
    deriving (Show)

type IResult = Either IError IType

inferType :: Exp -> IResult
inferType = histo f
  where f :: Exp_ (Cofree Exp_ IResult) -> IResult
        f (Int _)                   = Right IInt
        f (Plus (l :< _) (r :< _))  = mathCheck l r
        f (Minus (l :< _) (r :< _)) = mathCheck l r
        f (Times (l :< _) (r :< _)) = mathCheck l r
        f (Div (l :< _) (r :< _))   = mathCheck l r
        f (Negate (t :< _))         = IInt ==. t

mathCheck = binaryCheck IInt IInt IInt

binaryCheck :: IType -> IType -> IType -> IResult -> IResult -> IResult
binaryCheck a b c l r = a ==. l *> b ==. r *> pure c

(==.) :: IType -> IResult -> IResult
(==.) t (Right c)
  | t == c = Right c
  | otherwise = Left $ WrongType t c
(==.) t r = r

test = plus (int 5) (int 6)
