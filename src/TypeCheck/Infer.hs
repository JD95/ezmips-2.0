{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

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
  | IBool
  | IFunc IType [IType]
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

data IEither e a = ILeft e | IRight a deriving (Show, Functor)

instance Semigroup e => Applicative (IEither e) where
  pure = IRight
  (ILeft e) <*> (ILeft d) = ILeft (e <> d)
  (ILeft e) <*> _ = ILeft e
  _ <*> (ILeft e) = ILeft e
  (IRight f) <*> (IRight x) = IRight (f x)

instance Semigroup e => Monad (IEither e) where
  (ILeft e) >>= f = ILeft e
  (IRight e) >>= f = f e
  
type IResult = IEither IError IType
type TypedTree = Cofree Exp_ IResult
type TypeInference = Reader (Map.Map Name IType) TypedTree 

infer :: Exp -> TypeInference
infer = cata f
  where f :: Exp_ TypeInference -> TypeInference
        f (Int x) = pure (IRight IInt :< Int x)
        f (Sym s) = do
          t <- Map.lookup s <$> ask 
          let t' = maybe (ILeft . UndefinedSymbol $ s) IRight t
          pure (t' :< Sym s)

        -- Math Operations
        f (Plus l r) = binaryFunc l r mathOp Plus
        f (Minus l r) = binaryFunc l r mathOp Minus
        f (Times l r) = binaryFunc l r mathOp Times
        f (Negate e) = unaryFunc e (unaryCheck IInt IInt) Negate

        -- Logic Operations
        f (Div l r) = binaryFunc l r logicOp Div
        f (And l r) = binaryFunc l r logicOp And 
        f (Or l r) = binaryFunc l r logicOp Or 
        f (Not e) = unaryFunc e (unaryCheck IBool IBool) Not 

unaryFunc :: TypeInference
          -> (IResult -> IResult)
          -> (forall a. a -> Exp_ a)
          -> TypeInference
unaryFunc e f h = do
  x@ (t :< exp) <- e
  pure (f t :< h x)

unaryCheck e pass = \x -> matchFuncArgs [e] [x] pass

binaryFunc :: TypeInference
                -> TypeInference
                -> (IResult -> IResult -> IResult)
                -> (forall a. a -> a -> Exp_ a)
                -> TypeInference
binaryFunc l r f h = do
  x@(lType :< lExp) <- l
  y@(rType :< rExp) <- r
  pure ((f lType rType) :< h x y)

binaryCheck l r pass = \x y -> matchFuncArgs [l, r] [x, y] pass
mathOp = binaryCheck IInt IInt IInt
logicOp = binaryCheck IBool IBool IBool

matchFuncArgs :: [IType]    -- ^ Expected Inputs
              -> [IResult]  -- ^ Given Inputs
              -> IType      -- ^ Success Type
              -> IResult
matchFuncArgs es gs r = do
  givens <- sequence gs
  if and (zipWith (==) es givens) 
    then pure r
    else ILeft $ InvalidFuncArgs es givens
    
test = flip runReader table . infer . freeToFix $ do
  plus (sym "x") (sym "y")
  where table = Map.fromList 
          [ ("x", IVoid)
          , ("y", IInt)
          ]
