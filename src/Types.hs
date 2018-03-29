{-# LANGUAGE TemplateHaskell, RankNTypes #-}
module Types where

import Control.Lens

type DataBlockHeading = String
type Tag = String

data Numeric = FloatValue Float | IntValue Int deriving (Show)
data DataValue = StringValue String | NumericValue Numeric deriving (Show)

data DataItem = Item
    { _tagName :: Tag
    , _value :: DataValue
    }
    | Items
    { _items :: [DataItem]
    } deriving (Show)

data DataBlock = DataBlock
    { _blockHeading :: DataBlockHeading
    , _dataItems :: [DataItem]
    } deriving (Show)

data Cif = Cif
    { _dataBlocks :: [DataBlock]
    } deriving (Show)

makeLenses ''DataItem
makeLenses ''DataBlock
makeLenses ''Cif
