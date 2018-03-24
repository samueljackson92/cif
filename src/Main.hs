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
import Control.Applicative ((*>))

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

type DataBlockHeading = String
type Tag = String
type Value = String

-- Character set definitions
doubleQuote = '\"'
singleQuote = '\''

ordinaryCharset = ['!' , '%' , '&' , '(' , ')' , '*' , '+' , ',' , '-' , '.' , '/' , '0' , '1' , '2' , '3' , '4' , '5' , '6' , '7' , '8' , '9' , ':' , '<' , '=' , '>' , '?' , '@' , 'A' , 'B' , 'C' , 'D' , 'E' , 'F' , 'G' , 'H' , 'I' , 'J' , 'K' , 'L' , 'M' , 'N' , 'O' , 'P' , 'Q' , 'R' , 'S' , 'T' , 'U' , 'V' , 'W' , 'X' , 'Y' , 'Z' , '\\' , '^' , '`' , 'a' , 'b' , 'c' , 'd' , 'e' , 'f' , 'g' , 'h' , 'i' , 'j' , 'k' , 'l' , 'm' , 'n' , 'o' , 'p' , 'q' , 'r' , 's' , 't' , 'u' , 'v' , 'w' , 'x' , 'y' , 'z' , '{' , ',' , '}' , '~']

nonBlankCharset = ordinaryCharset ++ ['#' , '$' , '_' , ';' , '[' , ']']
textLeadCharset = ordinaryCharset ++ ['#' , '$' , '_' , ' ' , '\t' ,'[' , ']']
anyPrintCharset = ordinaryCharset ++ ['#' , '$' , '_' , ' ' , '\t' , ';' , '[' , ']']

isOrdinaryChar :: Char -> Bool
isOrdinaryChar c = elem c ordinaryCharset

data DataItem = Item Tag Value deriving (Show)
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
anyPrintString = many (escape <|> oneOf anyPrintCharset) <?> "any print string"

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

charString :: Parser String
charString = unQuotedString <|> singleQuotedString <|> doubleQuotedString

tag = char '_' *> nonBlankString <?> "tag"

lexer = Token.makeTokenParser languageDef
identifier = Token.identifier lexer
reserved   = Token.reserved
whiteSpace = Token.whiteSpace lexer

dataItem :: Parser DataItem
dataItem =
    do tag <- tag
       whiteSpace
       value <- charString
       whiteSpace
       return $ Item tag value

dataBlock :: Parser DataBlock
dataBlock =
    do header <- identifier
       whiteSpace
       items <- dataItems
       return $ DataBlock header items
    where
        dataItems = sepBy dataItem whiteSpace

cifBlock :: Parser Cif
cifBlock =
    do blocks <- dataBlocks
       return $ Cif blocks
    where
        dataBlocks = sepBy dataBlock whiteSpace

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
