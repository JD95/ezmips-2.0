{-# LANGUAGE DeriveFunctor, PatternSynonyms #-}
module Parser.AST where

import Data.Functor.Foldable

data Exp_ a
  = Plus a a
  | Minus a a
  | Times a a
  | Div a a
  | Negate a
  | Int Int
    deriving (Show, Functor)

type Exp = Fix Exp_

pattern Plus_ l r = Fix (Plus l r)
pattern Minus_ l r = Fix (Minus l r)
pattern Times_ l r = Fix (Times l r)
pattern Div_ l r = Fix (Div l r)
pattern Negate_ e  = Fix (Negate e)
pattern Int_ n = Fix (Int n)
