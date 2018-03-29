{-# LANGUAGE CPP, FlexibleContexts, Safe #-}
module Strings where

import Data.Char
import Text.Parsec.Char
import Text.ParserCombinators.Parsec

import Utils
import Grammar

-- Match the lowercase or uppercase form of 'c'
caseInsensitiveChar c = char (toLower c) <|> char (toUpper c)

-- Match the string 's', accepting either lowercase or uppercase form of each character
caseInsensitiveString s = try (mapM caseInsensitiveChar s) <?> "\"" ++ s ++ "\""

ordinaryString :: Parser String
ordinaryString = many (oneOf ordinaryCharset) <?> "ordinary string"

nonBlankString :: Parser String
nonBlankString = many (oneOf nonBlankCharset) <?> "non-blank string"

anyPrintString :: Parser String
anyPrintString = many (try escape <|> oneOf anyPrintCharset) <?> "any print string"

parseQuotedString :: Char -> Parser String
parseQuotedString c = do
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
    first <- oneOf $ ordinaryCharset ++ [doubleQuote, singleQuote]
    rest <- many $ oneOf anyPrintCharset
    return $ first : rest

textField :: Parser String
textField =
    do
       whiteSpace
       lines <- colonSeperated multiLineString
       return $ lines
    where multiLineString = many $ oneOf $ textLeadCharset ++ "\n"
          colonSeperated = between (string ";\n") (char ';')

charString :: Parser String
charString = singleQuotedString <|> doubleQuotedString <|> textField <|> unQuotedString

