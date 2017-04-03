{-#
  LANGUAGE
    RecordWildCards
#-}

module GrammarParser where

import Data.List
import Data.Maybe
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
    fragment::Bool,
    ruleName::String,
    options::Or
    } deriving (Show)

formatRule Rule{..} =
  ruleName ++ "\n   : "
  ++ formatOr options
  ++ "\n   ;\n"

--parseRule::Parsec String
parseRule = do
  fragment <- fmap isJust $ optionMaybe $ try $ string "fragment"
  whiteSpace
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
    fragment=fragment,
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
  <|> do
    string "channel"
    whiteSpace
    string "(HIDDEN)"

parseParen = do
  char '('
  whiteSpace
  options <- parseOr
  whiteSpace
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
  AnyChar
  | RuleTerm {
    termName::String
    }
  | ParenTerm {
    parenTermOptions::Or
    }
  | CharTerm {
    theChar::String
    }
  | CharRangeTerm {
    range::CharRange
    }
  | CharSetTerm {
    charset::CharSet
    } deriving (Show)

data Modifier = Many Bool | Option | Plus | None deriving (Show)

data ModifiedTerm = ModifiedTerm Bool Term Modifier deriving (Show)

formatModifier (Many greedy) = "*" ++ if greedy then "?" else ""
formatModifier Option = "?"
formatModifier Plus = "+"
formatModifier None = ""

formatModifiedTerm (ModifiedTerm negated t m) = if negated then "~" else "" ++ formatTerm t ++ formatModifier m

formatTerm::Term->String
formatTerm AnyChar = "."
formatTerm RuleTerm{..} = termName
formatTerm ParenTerm{..} = "(" ++ intercalate " | " (map formatTermList (orTerm parenTermOptions)) ++ ")"
formatTerm CharTerm{..} = show theChar
formatTerm CharRangeTerm{..} = format range
formatTerm CharSetTerm{..} = format charset

parseModifiedTerm::Parsec String () ModifiedTerm
parseModifiedTerm = do
  negated <- fmap isJust $ optionMaybe $ char '~'
  term <- parseTerm
  whiteSpace
  maybeModifierChar <- optionMaybe $ oneOf "*?+"
  greedyFlag <- fmap isJust $ optionMaybe $ char '?'

  let modifier = 
        case maybeModifierChar of
         Nothing -> None
         Just '*' -> Many greedyFlag
         Just '?' -> Option
         Just '+' -> Plus
         
  return $ ModifiedTerm negated term modifier



parseTerm = do
  parseAnyChar <|> parseRuleTerm <|> parseParenTerm <|> parseCharOrCharRangeTerm <|> parseCharSetTerm



parseAnyChar = do
  char '.'
  return AnyChar

parseRuleTerm = do
  word <- parseWord
  return $ RuleTerm word

parseParenTerm = do
  paren <- parseParen
  return $ ParenTerm paren

parseCharOrCharRangeTerm = do
  c <- parseString
  whiteSpace
  maybeChar <-
    optionMaybe $ do
      try $ string ".."
      whiteSpace
      parseString

  case maybeChar of
   Nothing -> return $ CharTerm c
   Just toC -> return $ CharRangeTerm (CharRange c toC)

data CharRange = CharRange String String deriving (Show)

instance Format CharRange where
  format (CharRange from to) = show from ++ ".." ++ show to

data CharSet = CharSet [CharOrSetRange] deriving (Show)

instance Format CharSet where
  format (CharSet cs) = "[" ++ concat (map format cs) ++ "]"

instance Format CharOrSetRange where
  format (AChar c) = format c
  format (ARange c1 c2) = format c1 ++ "-" ++ format c2
  
instance Format Char where
  format '\n' = "\\n"
  format '\t' = "\\t"
  format '\r' = "\\r"
  format c = [c]

parseCharSetTerm = do
  charset <- parseCharSet
  return CharSetTerm{charset=charset}

parseCharRange = do
  parseChar
  whiteSpace
  try $ string ".."
  whiteSpace
  parseChar
  return ()

parseCharSet = do
  char '['
  charset <- many parseCharSetCharOrSetRange
  char ']'
  return $ CharSet charset

data CharOrSetRange = AChar Char | ARange Char Char deriving (Show)

parseCharSetCharOrSetRange = do
  c <- parseEscapedChar <|> (oneOf (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "'\" $_"))
  maybeChar <-
    optionMaybe $ do
      char '-'
      parseEscapedChar <|> (oneOf (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " "))

  case maybeChar of
   Nothing -> return $ AChar c
   Just toC -> return $ ARange c toC



parseEscapedChar = do
  char '\\'
  c <- do
    c1 <- oneOf "nrt\\"
    return ['\\', c1]
    <|> do
    char 'u'
    c1 <- anyChar
    c2 <- anyChar
    c3 <- anyChar
    c4 <- anyChar
    return ['\\', 'x', c1, c2, c3, c4]
  return $ read $ "'" ++ c ++ "'"
  
parseChar = do
  char '\''
  c <- anyChar
  char '\''
  return c


parseEChar = do
  noneOf "\\'" <|> do
    char '\\'
    c <- anyChar
    return $
      case c of
        '\'' -> '\''
        '\\' -> '\\'

parseString::Parsec String () String
parseString = do
  char '\''
  st <- many $ parseEChar
  char '\''
  return st

{-
parseRuleName = undefined
-}

whiteSpace = do
  many $ (try parseComment <|> try parseLineComment <|> (space >> return ()))

parseComment = do
  string "/*"
  manyTill (anyChar) $ try $ string "*/"
  return ()

parseLineComment = do
  string "//"
  many $ noneOf "\r\n"
  return ()
  
parseWord = do
  many1 $ alphaNum <|> char '_'
