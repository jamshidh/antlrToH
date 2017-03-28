module Main where

import Text.Parsec

import GrammarParser
import ParserCreator

main :: IO ()
main = do
  contents <- readFile "arithmetic.g4"
  let x = parse parseGrammar "<hardcoded>" contents
  case x of
   Left e -> putStrLn $ show e
   Right grammar -> putStrLn $ createHaskellParser grammar
