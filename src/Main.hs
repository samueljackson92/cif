module Main where

import System.Environment
import System.IO
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

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


data DataItem = Item Tag Value deriving (Show)
data DataBlock = DataBlock DataBlockHeading [DataItem] deriving (Show)
data Cif = Cif [DataBlock] deriving (Show)

lexer = Token.makeTokenParser languageDef
identifier = Token.identifier lexer
reserved   = Token.reserved
whiteSpace = Token.whiteSpace lexer

dataItem :: Parser DataItem
dataItem =
    do tag <- identifier
       whiteSpace
       value <- identifier
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
