{-# LANGUAGE OverloadedStrings #-}

module TypeCheck.Infer where

import Control.Comonad.Cofree
import Data.Functor.Foldable
import qualified Data.Map as Map
import Control.Monad.Reader
import Prelude ()
import Protolude hiding (sym)

import Parser.Grammar
import Parser.AST

data IType
  = IInt
  | IVoid
    deriving (Show, Eq)

data IError
  = WrongType IType IType
  | InvalidFuncArgs [IType] [IType]
  | UndefinedSymbol Name
  | Errors [IError]
    deriving (Show)

instance Semigroup IError where
  (Errors xs) <> (Errors ys) = Errors (xs <> ys)
  (Errors xs) <> e = Errors (e:xs)
  e <> (Errors xs) = Errors (e:xs)
  x <> y = Errors [x,y]
  
type IResult = Either IError IType
type TypedTree = Cofree Exp_ IResult
type TypeInference = Reader (Map.Map Name IType) TypedTree 

infer :: Exp -> TypeInference
infer = cata f
  where f :: Exp_ TypeInference -> TypeInference
        f (Int x) = pure (Right IInt :< Int x)
        f (Sym s) = do
          t <- Map.lookup s <$> ask 
          let t' = maybe (Left . UndefinedSymbol $ s) Right t
          pure (t' :< Sym s)
        f (Plus l r) = do
          x@(lType :< lExp) <- l
          y@(rType :< rExp) <- r
          pure ((lType <*$> rType) binaryMathOp :< Plus x y)

(<*$>) :: Semigroup m => Either m a -> Either m a -> (a -> a -> Either m a) -> Either m a 
(Left a) <*$> (Left b) = const $ Left (a <> b)
x <*$> y = \f -> join $ f <$> x <*> y 

binaryMathOp :: IType -> IType -> IResult 
binaryMathOp IInt IInt = Right IInt
binaryMathOp a b = Left (InvalidFuncArgs [IInt, IInt] [a, b])

test = flip runReader table . infer . freeToFix $ do
  plus (sym "x") (sym "y")
  where table = Map.fromList 
          [ ("x", IVoid)
          , ("y", IInt)
          ]
