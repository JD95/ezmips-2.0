{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
module Parser.AST where

import           Control.Monad.Free
import qualified Control.Monad.Trans.Free as F
import           Data.Functor.Foldable
import qualified Prelude
import           Protolude

data Exp_ a
  = Plus a a
  | Minus a a
  | Times a a
  | Div a a
  | Negate a
  | Int Int
    deriving (Show, Functor, Foldable, Traversable)

type Exp = Fix Exp_

newtype PrintExp = PrintExp Exp

showExp :: Exp -> Text
showExp = cata f
  where f :: Exp_ Text -> Text
        f (Int i)     = show i
        f (Plus l r)  = l <> " + " <> r
        f (Minus l r) = l <> " - " <> r
        f (Times l r) = l <> " * " <> r
        f (Div l r)   = l <> " / " <> r
        f (Negate e)  = "-" <> e

instance Prelude.Show PrintExp where
  show (PrintExp e) = toS $ showExp e

pattern Plus_ l r = Fix (Plus l r)
pattern Minus_ l r = Fix (Minus l r)
pattern Times_ l r = Fix (Times l r)
pattern Div_ l r = Fix (Div l r)
pattern Negate_ e  = Fix (Negate e)
pattern Int_ n = Fix (Int n)

type FExp = Free Exp_ Void

newtype PrintFExp = PrintFExp FExp

instance Prelude.Show PrintFExp  where
  show (PrintFExp e) = show . PrintExp . freeToFix $ e

plus :: FExp -> FExp -> FExp
plus l r = Free (Plus l r)

minus :: FExp -> FExp -> FExp
minus l r = Free (Minus l r)

times :: FExp -> FExp -> FExp
times l r = Free (Times l r)

div :: FExp -> FExp -> FExp
div l r = Free (Div l r)

neg :: FExp -> FExp
neg e = Free (Negate e)

int :: Int -> FExp
int e = Free (Int e)

freeToFix :: (Functor f) => Free f Void -> Fix f
freeToFix = cata f
  where f :: F.FreeF f Void (Fix f) -> Fix f
        f (F.Free r) = Fix r

test = plus (int 1) (int 2)
