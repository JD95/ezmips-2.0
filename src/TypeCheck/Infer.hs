{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module TypeCheck.Infer where

import Control.Comonad.Cofree
import Data.Functor.Foldable
import qualified Data.Map as Map
import Control.Monad.Reader
import qualified Prelude 
import Protolude hiding (sym, LT, GT, EQ)

import Parser.Grammar
import Parser.AST

newtype VarDecl = VarDecl (TypeInference -> TypeInference)

instance Prelude.Show VarDecl where
  show _ = "..."

instance Prelude.Eq VarDecl where
  _  == _ = True 

data IType
  = IInt
  | IVoid
  | ISym Name
  | IDecl IType VarDecl  
  | IBool
  | ITypeTag IType
  | IFunc IType [IType]
    deriving (Prelude.Show, Prelude.Eq)

data IError
  = WrongType IType IType
  | InvalidFuncArgs [IType] [IType]
  | UndefinedSymbol Name
  | RedeclaredVar Name
  | InvalidType Name
  | BadVariableDecl
  | Errors [IError]
    deriving (Prelude.Show)

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

        f (LT l r) = binaryFunc l r intComp LT
        f (EQ l r) = binaryFunc l r intComp EQ
        f (GT l r) = binaryFunc l r intComp GT

        f (Decl t n) = binaryFunc t n declCheck Decl  

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
intComp = binaryCheck IInt IInt IBool

declCheck :: IResult -> IResult -> IResult
declCheck (IRight (ITypeTag t)) (ILeft (UndefinedSymbol n)) =
  IRight . IDecl t . VarDecl $ \next ->
    runReader next . Map.insert n t <$> ask 
declCheck _ (IRight (ISym n)) = ILeft (RedeclaredVar n)
declCheck (IRight (ISym t)) _ = ILeft (InvalidType t) 
declCheck _ _ = ILeft BadVariableDecl 

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
  decl "int" "x"
  where table = Map.fromList 
          [ ("y", IInt)
          , ("int", ITypeTag IInt)
          , ("bool", ITypeTag IBool)
          ]
