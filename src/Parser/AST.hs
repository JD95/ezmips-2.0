{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
module Parser.AST where

import           Control.Monad.Free
import qualified Control.Monad.Trans.Free as F
import           Data.Functor.Foldable
import qualified Prelude
import           Protolude hiding (GT, LT, EQ)
import qualified Data.ByteString.Lazy as LBS

data Exp_ a
  = Stmt a (Maybe a)
  | If a a (Maybe a)
  | While a a
  | Decl a a
  | Assign a a
  | Plus a a
  | Minus a a
  | Times a a
  | Div a a
  | And a a
  | Or a a
  | LT a a
  | EQ a a
  | GT a a
  | Not a
  | Negate a
  | Sym LBS.ByteString
  | Int Int
    deriving (Show, Functor, Foldable, Traversable)

type Exp = Fix Exp_

newtype PrintExp = PrintExp Exp

showExp :: Exp -> Text
showExp = cata f
  where f :: Exp_ Text -> Text        
        f (Int i)     = show i
        f (Sym s)     = show s
        f (Plus l r)  = l <> " + " <> r
        f (Minus l r) = l <> " - " <> r
        f (Times l r) = l <> " * " <> r
        f (Div l r)   = l <> " / " <> r
        f (And l r)  = l <> " && " <> r
        f (Or l r) = l <> " || " <> r
        f (LT l r) = l <> " < " <> r
        f (EQ l r)   = l <> " == " <> r
        f (GT l r) = l <> " > " <> r
        f (Not e)  = "!" <> e
        f (Negate e)  = "-" <> e
        f (Stmt e Nothing) = e <> ";"
        f (Stmt e (Just n)) = e <> ";\n" <> n
        f (If c a (Just e)) = "if(" <> c <> ") {\n"
                           <> "\t" <> a
                           <> "}\n else {\n" <> e <> "}\n"
        f (If c a Nothing) = "if(" <> c <> ") {\n" <> a <> "\n}\n"
        f (While c a) = "while(" <> c <> ") {\n" <> a <> "\n}\n"
        f (Decl t a) = t <> " " <> a
        f (Assign v e) = v <> " = " <> e

instance Prelude.Show PrintExp where
  show (PrintExp e) = toS $ showExp e

pattern Plus_ l r = Fix (Plus l r)
pattern Minus_ l r = Fix (Minus l r)
pattern Times_ l r = Fix (Times l r)
pattern Div_ l r = Fix (Div l r)
pattern Negate_ e  = Fix (Negate e)
pattern Int_ n = Fix (Int n)
pattern Sym_ s = Fix (Sym s)

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


