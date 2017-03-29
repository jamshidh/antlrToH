{-#
  LANGUAGE
    RecordWildCards
#-}

module ParserCreator where

import Data.Char
import Data.List

import GrammarParser

createHaskellParser::Grammar->String
createHaskellParser g =
  "module Parser where\n\nimport Text.Parsec\nimport Data.Ix\n\n"
  ++ "wrap x y = \"<\" ++ x ++ \">\" ++ y ++ \"</\" ++ x ++ \">\"\n"
  ++ intercalate "\n\n" (map createRuleParser $ rules g)
  ++ "\n"

createRuleParser r =
  "parse" ++ toUpper (head (ruleName r)):tail (ruleName r) ++ "::Parsec String () String\n"
  ++ "parse" ++ toUpper (head (ruleName r)):tail (ruleName r) ++ " =\n    "
  ++ createOrParser (options r)


createOrParser (Or [tl]) = createTermListParser tl
createOrParser options =
  "choice [" ++ intercalate ", " (map createTermListParser $ orTerm options) ++ "]"

createTermListParser (TermList [mt]) = createModifiedParser mt
createTermListParser termList =
  "fmap concat (sequence [" ++ intercalate ", " (map createModifiedParser $ listTerms termList) ++ "])"

createModifiedParser::ModifiedTerm->String
createModifiedParser (ModifiedTerm negated t (Many greedy)) = "fmap concat (many (" ++ createTermParser t ++ "))"
createModifiedParser (ModifiedTerm negated t Option) = "option \"\" (" ++ createTermParser t ++ ")"
createModifiedParser (ModifiedTerm negated t Plus) = "fmap concat (many1 (" ++ createTermParser t ++ "))"
createModifiedParser (ModifiedTerm negated t None) = createTermParser t

createTermParser AnyChar = "anyChar"
createTermParser RuleTerm{..} = "fmap (wrap \"" ++ termName ++ "\") parse" ++ toUpper (head termName):tail termName
createTermParser ParenTerm{..} = "choice [" ++ intercalate ", " (map createTermListParser $ orTerm parenTermOptions) ++ "]"
createTermParser CharTerm{..} = "fmap show (char " ++ show theChar ++ ")"
createTermParser CharRangeTerm{range=CharRange from to} =
  "fmap (:[]) (satisfy (inRange (" ++ show from ++ ", " ++ show to ++ ")))"
createTermParser CharSetTerm{charset=CharSet cs} = "fmap show (oneOf " ++ show cs ++ ")"

