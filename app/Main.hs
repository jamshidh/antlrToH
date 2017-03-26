module Main where

import Text.Parsec

import GrammarParser
import Lib

main :: IO ()
main = do
  contents <- readFile "arithmetic.g4"
  let x = parse parseGrammar "<hardcoded>" contents
  case x of
   Left e -> putStrLn $ show e
   Right grammar -> putStrLn $ formatGrammar grammar
