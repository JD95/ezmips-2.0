{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude ()
import Protolude

import Data.Semigroup ((<>))
import Options.Applicative
import Lens.Micro.Platform

import qualified Lexer.Token as T
import qualified Parser.Grammar as G

data Options = Options { message :: Text }

opts :: Parser Options
opts = Options <$> option auto
     ( long "message"
    <> short 'm'
    <> metavar "MESSAGE"
    <> help "The message to print"
    <> showDefault
    <> value "Hello, World!"
      )

optsInfo :: ParserInfo Options
optsInfo = info (opts <**> helper)
  ( fullDesc
  <> progDesc "Print a simple message"
  <> header "ezmips - a minimal application"
  )

main :: IO ()
main = do
  options <- execParser optsInfo
  putStrLn (message options)

