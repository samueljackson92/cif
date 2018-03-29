{-# LANGUAGE CPP, FlexibleContexts, Safe #-}
module Main where

import System.Environment
import System.IO
import Control.Monad
import Text.Parsec.Char
import Text.ParserCombinators.Parsec
import Control.Applicative hiding ((<|>), many)

import Grammar
import Strings
import Types
import Utils

parseLoop :: Parser [DataItem]
parseLoop =
    do reserved "LOOP_"
       headers <- parseLoopHeader
       entries <- parseEntries `manyTill` terminator
       return $ [Items $ map makeItem $ zip headers values | values <- entries]
    where terminator = try (lookAhead (string "_" <|> string "loop_" <|> (eof >> string "")))
          makeItem (header, value) = Item header value

parseLoopHeader :: Parser [Tag]
parseLoopHeader =
    do tags <- getTag `manyTill` terminator
       return tags
    where terminator = try (lookAhead (notFollowedBy (string "_")))
          getTag     = tag >>= \t -> endOfLine >> return t

parseEntries :: Parser [Value]
parseEntries =
    do values <- getEntry `sepBy1` char ' '
       endOfLine
       return values
    where getEntry  = try getNumber <|> getString
          getString = StringValue <$> (singleQuotedString <|> doubleQuotedString <|> nonBlankString)
          getNumber = NumericValue <$> parseNumeric >>= \num -> lookAhead (char ' ') >> return num


parseNumeric :: Parser Numeric
parseNumeric = FloatValue <$> (try float) <|> IntValue <$> (try integer)

parseValue :: Parser Value
parseValue = try (getNumeric) <|> getString
    where getNumeric  = NumericValue <$> parseNumeric >>= \x -> lookAhead endOfLine >> return  x
          getString   = StringValue <$> charString

tag :: Parser Tag
tag = char '_' *> nonBlankString <?> "tag"

dataItem :: Parser DataItem
dataItem =
    do t <- tag
       whiteSpace
       value <- parseValue
       endOfLine
       return (Item t value)

dataBlock :: Parser DataBlock
dataBlock =
    do caseInsensitiveString "data_"
       header <- nonBlankString
       endOfLine
       items <- many (dataItems <|> parseLoop)
       return $ DataBlock header (concat items)
    where
        dataItems = dataItem >>= \item -> return [item]

parseCif :: Parser Cif
parseCif =
    do blocks <- dataBlocks
       return $ Cif blocks
    where dataBlocks = many dataBlock

whileParser :: Parser Cif
whileParser = whiteSpace >> parseCif

parseCIFFile :: String -> IO Cif
parseCIFFile file =
  do program  <- readFile file
     case parse whileParser "" program of
       Left e  -> print e >> fail "parse error"
       Right r -> return r

main :: IO ()
main = do
  args <- getArgs
  cif <- parseCIFFile (head args)
  putStrLn $ show cif
