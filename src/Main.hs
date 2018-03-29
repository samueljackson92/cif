{-# LANGUAGE CPP, FlexibleContexts, Safe #-}
module Main where

import System.Environment
import System.IO
import Control.Monad
import Text.Parsec.Char
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token
import Control.Applicative hiding ((<|>), many)

-- Parsec Language Definitions
languageDef =
  emptyDef { Token.commentLine     = "#"
           , Token.reservedNames   = [ "DATA_"
                                     , "LOOP_"
                                     , "GLOBAL_"
                                     , "SAVE_"
                                     , "STOP_"
                                     ]
            , Token.identStart = char '_' <|> alphaNum
            , Token.identLetter = char '_' <|> alphaNum
            , Token.caseSensitive  = False
           }

lexer = Token.makeTokenParser languageDef
identifier = Token.identifier lexer
reserved   = Token.reserved lexer
whiteSpace = Token.whiteSpace lexer

type DataBlockHeading = String
type Tag = String

-- Character set definitions
doubleQuote = '\"'
singleQuote = '\''

ordinaryCharset = ['!' , '%' , '&' , '(' , ')' , '*' , '+' , ',' , '-' , '.' , '/' , '0' , '1' , '2' , '3' , '4' , '5' , '6' , '7' , '8' , '9' , ':' , '<' , '=' , '>' , '?' , '@' , 'A' , 'B' , 'C' , 'D' , 'E' , 'F' , 'G' , 'H' , 'I' , 'J' , 'K' , 'L' , 'M' , 'N' , 'O' , 'P' , 'Q' , 'R' , 'S' , 'T' , 'U' , 'V' , 'W' , 'X' , 'Y' , 'Z' , '\\' , '^' , '`' , 'a' , 'b' , 'c' , 'd' , 'e' , 'f' , 'g' , 'h' , 'i' , 'j' , 'k' , 'l' , 'm' , 'n' , 'o' , 'p' , 'q' , 'r' , 's' , 't' , 'u' , 'v' , 'w' , 'x' , 'y' , 'z' , '{' , ',' , '}' , '~']

nonBlankCharset = ordinaryCharset ++ ['#' , '$' , '_' , ';' , '[' , ']']
textLeadCharset = ordinaryCharset ++ ['#' , '$' , '_' , ' ' , '\t' ,'[' , ']']
anyPrintCharset = ordinaryCharset ++ ['#' , '$' , '_' , ' ' , '\t' , ';' , '[' , ']']

isOrdinaryChar :: Char -> Bool
isOrdinaryChar c = elem c ordinaryCharset

data Numeric = FloatValue Float | IntValue Int deriving (Show)
data Value = StringValue String | NumericValue Numeric deriving (Show)
data DataItem = Item Tag Value | Items [DataItem] deriving (Show)
data DataBlock = DataBlock DataBlockHeading [DataItem] deriving (Show)
data Cif = Cif [DataBlock] deriving (Show)

escape :: Parser Char
escape = do
    d <- char '\\'
    c <- oneOf "\\\"\'0nrvtbf" -- all the characters which can be escaped
    return c

ordinaryString :: Parser String
ordinaryString = many (oneOf ordinaryCharset) <?> "ordinary string"

nonBlankString :: Parser String
nonBlankString = many (oneOf nonBlankCharset) <?> "non-blank string"

anyPrintString :: Parser String
anyPrintString = many (try escape <|> oneOf anyPrintCharset) <?> "any print string"

parseQuotedString :: Char -> Parser String
parseQuotedString c =
    do
       char c
       value <- anyPrintString
       char c
       return $ value

singleQuotedString :: Parser String
singleQuotedString = parseQuotedString singleQuote

doubleQuotedString :: Parser String
doubleQuotedString = parseQuotedString doubleQuote

unQuotedString :: Parser String
unQuotedString = do
    many endOfLine
    first <- oneOf (ordinaryCharset ++ [doubleQuote, singleQuote])
    rest <- many (oneOf anyPrintCharset)
    return $ first : rest

textField :: Parser String
textField =
    do
       whiteSpace
       lines <- between (string ";\n") (char ';') (many (oneOf (textLeadCharset ++ "\n")))
       return $ lines

charString :: Parser String
charString = singleQuotedString <|> doubleQuotedString <|> textField <|> unQuotedString

makeItem :: (String, Value) -> DataItem
makeItem (header, value) = Item header value

parseLoop :: Parser [DataItem]
parseLoop =
    do string "loop_"
       endOfLine
       headers <- parseHeader
       values <- parseLine `manyTill` try (lookAhead (string "_" <|> string "loop_" <|> (eof >> string "")))
       return $ [Items (map makeItem (zip headers vs)) | vs <- values]
    where
        parseHeader :: Parser [Tag]
        parseHeader = do tags <- getTag `manyTill` try (lookAhead (notFollowedBy (string "_")))
                         return tags

        parseLine :: Parser [Value]
        parseLine =
            do values <- (try getNumber <|> getString) `sepBy1` char ' '
               endOfLine
               return values

        getTag = tag >>= \t -> endOfLine >> return t
        getString = StringValue <$> (singleQuotedString <|> doubleQuotedString <|> nonBlankString)
        getNumber = NumericValue <$> parseNumeric >>= \num -> lookAhead (char ' ') >> return num

(<++>) a b = (++) <$> a <*> b
(<:>) a b = (:) <$> a <*> b

number = try (many1 digit)

plus = char '+' *> number

minus = char '-' <:> number

int = plus <|> minus <|> number

integer = fmap rd $ int
    where rd = read :: String -> Int

float = fmap rd $ int <++> decimal <++> exponent <++> brackets
    where rd       = read :: String -> Float
          decimal  = char '.' <:> many digit
          exponent = option "" $ oneOf "eE" <:> int
          brackets = option "" $ (char '(') >> int >>= \num -> (char ')') >> return num

parseNumeric :: Parser Numeric
parseNumeric = FloatValue <$> (try float) <|> IntValue <$> (try integer)

parseValue :: Parser Value
parseValue = try (getNumeric) <|> getString
    where
        getNumeric  = NumericValue <$> parseNumeric >>= \x -> lookAhead endOfLine >> return  x
        getString   = StringValue <$> charString

tag = char '_' *> nonBlankString <?> "tag"

dataItem :: Parser DataItem
dataItem = tag >>= \t -> whiteSpace
    >> parseValue >>= \value -> endOfLine
    >> return (Item t value)

dataBlock :: Parser DataBlock
dataBlock =
    do header <- string "data_" >> nonBlankString
       endOfLine
       items <- many (dataItems <|> parseLoop)
       return $ DataBlock header (concat items)
    where
        dataItems = dataItem >>= \item -> return [item]

cifBlock :: Parser Cif
cifBlock =
    do blocks <- dataBlocks
       return $ Cif blocks
    where
        dataBlocks = many dataBlock

whileParser :: Parser Cif
whileParser = whiteSpace >> cifBlock

parseFile :: String -> IO Cif
parseFile file =
  do program  <- readFile file
     case parse whileParser "" program of
       Left e  -> print e >> fail "parse error"
       Right r -> return r

main :: IO ()
main = do
  args <- getArgs
  cif <- parseFile (head args)
  putStrLn $ show cif
