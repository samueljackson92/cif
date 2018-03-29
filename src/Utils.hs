module Utils where

import Text.ParserCombinators.Parsec

(<++>) a b = (++) <$> a <*> b
(<:>) a b = (:) <$> a <*> b

number :: Parser String
number = try (many1 digit)

plus :: Parser String
plus = char '+' *> number

minus :: Parser String
minus = char '-' <:> number

int :: Parser String
int = plus <|> minus <|> number

integer :: Parser Int
integer = fmap rd $ int
    where rd = read :: String -> Int

float :: Parser Float
float = fmap rd $ int <++> decimal <++> exponent <++> brackets
    where rd       = read :: String -> Float
          decimal  = char '.' <:> many digit
          exponent = option "" $ oneOf "eE" <:> int
          brackets = option "" $ (char '(') >> int >>= \num -> (char ')') >> return num

escape :: Parser Char
escape = do
    d <- char '\\'
    c <- oneOf "\\\"\'0nrvtbf"
    return c
