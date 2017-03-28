{-#
  LANGUAGE
    RecordWildCards
#-}

module GrammarParser where

import Data.List
import Text.Parsec

class Format a where
  format::a->String

data Grammar =
  Grammar {
    grammarName::String,
    rules::[Rule]
    } deriving (Show)

formatGrammar Grammar{..} =
  grammarName ++ "\n" ++ replicate (length grammarName) '=' ++ "\n"
  ++ unlines (map formatRule rules)

parseGrammar::Parsec String () Grammar
parseGrammar = do
  whiteSpace
  string "grammar"
  whiteSpace
  name <- parseWord
  char ';'
  whiteSpace
  rules <- sepBy parseRule whiteSpace
  eof
  return Grammar{
    grammarName=name,
    rules=rules
    }


data Rule =
  Rule {
    ruleName::String,
    options::Or
    } deriving (Show)

formatRule Rule{..} =
  ruleName ++ "\n   : "
  ++ formatOr options
  ++ "\n   ;\n"

--parseRule::Parsec String
parseRule = do
  name <- parseWord
  whiteSpace
  char ':'
  whiteSpace
  options <- parseOr
  whiteSpace
  optional $ do
    string "->"
    whiteSpace
    parseLexerCommand
    whiteSpace
  char ';'
  return Rule {
    ruleName=name,
    options=options
    }

parseLexerCommand = do
  string "skip"
  <|> string "more"
  <|> string "popMode"
  <|> string "mode()"
  <|> string "pushMode()"
  <|> string "type()"
  <|> string "channel (HIDDEN)"

parseParen = do
  char '('
  options <- parseOr
  char ')'
  return options

data Or =
  Or {
    orTerm::[TermList]
    } deriving (Show)

formatOr Or{..} =
  intercalate "\n   | " $ map formatTermList orTerm

parseOr = do
  terms <-
    sepBy parseTerms $ do
      char '|' 
      whiteSpace

  whiteSpace

  return Or{
    orTerm=terms
    }

data TermList =
  TermList{
    listTerms::[ModifiedTerm]
    } deriving (Show)

formatTermList TermList{..} =
  unwords $ map formatModifiedTerm listTerms

parseTerms = do
  terms <- many $ do
    t <- parseModifiedTerm
    whiteSpace
    return t
  return TermList{
    listTerms=terms
    }

data Term =
  RuleTerm {
    termName::String,
    modifier::Modifier
    }
  | ParenTerm {
    parenTermOptions::Or,
    modifier::Modifier
    }
  | CharTerm {
    theChar::Char,
    modifier::Modifier
    }
  | CharRangeTerm {
    range::CharRange,
    modifier::Modifier
    }
  | CharSetTerm {
    charset::CharSet,
    modifier::Modifier
    } deriving (Show)

data Modifier = Many | Option | Plus | None deriving (Show)

data ModifiedTerm = ModifiedTerm Term Modifier deriving (Show)

formatModifier Many = "*"
formatModifier Option = "?"
formatModifier Plus = "+"
formatModifier None = ""

formatModifiedTerm (ModifiedTerm t m) = formatTerm t ++ formatModifier m

formatTerm::Term->String
formatTerm RuleTerm{..} = termName
formatTerm ParenTerm{..} = "(" ++ intercalate " | " (map formatTermList (orTerm parenTermOptions)) ++ ")"
formatTerm CharTerm{..} = show theChar
formatTerm CharRangeTerm{..} = format range
formatTerm CharSetTerm{..} = format charset

parseModifiedTerm::Parsec String () ModifiedTerm
parseModifiedTerm = do
  term <- parseTerm
  whiteSpace
  maybeModifierChar <- optionMaybe $ oneOf "*?+"

  let modifier = 
        case maybeModifierChar of
         Nothing -> None
         Just '*' -> Many
         Just '?' -> Option
         Just '+' -> Plus
         
  return $ ModifiedTerm term modifier



parseTerm = do
  parseRuleTerm <|> parseParenTerm <|> parseCharOrCharRangeTerm <|> parseCharSetTerm



parseRuleTerm = do
  word <- parseWord
  return $ RuleTerm word None

parseParenTerm = do
  paren <- parseParen
  return $ ParenTerm paren None

parseCharOrCharRangeTerm = do
  c <- parseChar
  whiteSpace
  maybeChar <-
    optionMaybe $ do
      string ".."
      whiteSpace
      parseChar

  case maybeChar of
   Nothing -> return $ CharTerm c None
   Just toC -> return $ CharRangeTerm (CharRange c toC) None

data CharRange = CharRange Char Char deriving (Show)

instance Format CharRange where
  format (CharRange from to) = show from ++ ".." ++ show to

data CharSet = CharSet [Char] deriving (Show)

instance Format CharSet where
  format (CharSet cs) = "[" ++ concat (map format cs) ++ "]"

instance Format Char where
  format '\n' = "\\n"
  format '\t' = "\\t"
  format '\r' = "\\r"
  format c = [c]

parseCharSetTerm = do
  charset <- parseCharSet
  return CharSetTerm{charset=charset, modifier=None}

parseCharRange = do
  parseChar
  whiteSpace
  string ".."
  whiteSpace
  parseChar
  return ()

parseCharSet = do
  char '['
  charset <- many parseCharSetChar
  char ']'
  return $ CharSet charset

parseCharSetChar = do
  parseEscapedChar <|> (oneOf (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " "))

parseEscapedChar = do
  char '\\'
  c <- oneOf "nrt"
  return $ read ['\'', '\\', c, '\'']
  
parseChar = do
  char '\''
  c <- anyChar
  char '\''
  return c

{-
parseRuleName = undefined
-}

whiteSpace = do
  many $ (parseComment <|> (space >> return ()))

parseComment = do
  string "/*"
  manyTill (anyChar) $ try $ string "*/"
  return ()

parseWord = do
  many1 alphaNum
