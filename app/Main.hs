module Main where

import Data.List
import System.Environment
import Text.Parsec

import GrammarParser
import ParserCreator

usage::String
usage = "antlrToH [filename]"
  

main :: IO ()
main = do
  args <- getArgs
  contents <-
    case filter (not . ("-" `isPrefixOf`)) args of
     [] -> getContents
     [filename] -> readFile filename
     _ -> error $ unlines ["Incorrect usage:", usage]
     
  let x = parse parseGrammar "<hardcoded>" contents
  case x of
   Left e -> putStrLn $ show e
   Right grammar -> putStrLn $ createHaskellParser grammar
