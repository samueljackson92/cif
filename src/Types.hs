module Types where

type DataBlockHeading = String
type Tag = String

data Numeric = FloatValue Float | IntValue Int deriving (Show)
data Value = StringValue String | NumericValue Numeric deriving (Show)
data DataItem = Item Tag Value | Items [DataItem] deriving (Show)
data DataBlock = DataBlock DataBlockHeading [DataItem] deriving (Show)
data Cif = Cif [DataBlock] deriving (Show)
