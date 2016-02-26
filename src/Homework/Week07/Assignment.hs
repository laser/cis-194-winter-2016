{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module Homework.Week07.Assignment (
  ynToBool,
  parseData,
  parseMarkets,
  loadData,
  search,
  firstFound,
  lastFound,
  allFound,
  numberFound,
  orderedNtoS,
  Market(..),
  OrdList(..),
  Searcher(..)
) where

import Data.Aeson
import Data.Monoid
import GHC.Generics
import Data.List (sort)

import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Text as T
import qualified Data.Text.IO as T

-- #1

-- Value -> Value. It's not creating Values, it's just
-- converting one Value to another

-- This data type does conversions to
-- Object !Object
-- Array !Array
-- String !Text
-- Number !Scientific
-- Bool !Bool
-- Null

ynToBool :: Value -> Value
ynToBool (String "Y")  = Bool True
ynToBool (String "N")  = Bool False
ynToBool (Object value) = Object $ fmap ynToBool value
ynToBool (Array value ) = Array  $ fmap ynToBool value
ynToBool val            = val


-- #2
parseData :: B.ByteString -> Either String Value
parseData = fmap ynToBool . eitherDecode
  -- parseData conv (eitherDecode bstr :: Either String Value) where
  --   conv :: Either String Value -> Either String Value
  --   conv (Right esv) = Right $ ynToBool esv
  --   conv (Left str)  = Left str

-- #3
data Market = Market { marketname :: T.Text
                     , x :: Double
                     , y :: Double
                     , state :: T.Text } deriving (Eq, Show, Generic)

instance FromJSON Market

parseMarkets :: B.ByteString -> Either String [Market]
--parseMarkets bstr = (eitherDecode bstr :: Either String [Market]) --where
  -- conv :: Either String [Market] -> Either String [Market]
  -- conv (Right eslm) = Right eslm
  -- conv (Left str)   = Left str

parseMarkets bst  = let
  eVals = parseData bst
  in case eVals of
    Left x       -> Left x
    Right aValue -> case fromJSON aValue of
      Success markets -> Right markets
      Error       str -> Left str



   -- case (fmap fromJSON $ parseData :: Either String (Result [Market]))


-- #4
-- readFile :: IO B.ByteString
loadData :: IO [Market]
loadData = do
  filedata <- B.readFile "/Users/harnold/haskell/cis-194-winter-2016/src/Homework/Week07/markets.json"
  let (Right [market]) = parseMarkets filedata
      (Left x)         = fail x
  return [market]

data OrdList a = OrdList { getOrdList :: [a] } deriving (Eq, Show)

instance Ord a => Monoid (OrdList a) where
    mempty = OrdList []
    mappend (OrdList xs) (OrdList ys) = OrdList (sort $ xs ++ ys)

-- #6
type Searcher m = T.Text -> [Market] -> m

search :: Monoid m => (Market -> m) -> Searcher m
search f = undefined

-- #7
firstFound :: Searcher (Maybe Market)
firstFound = undefined

-- #8
lastFound :: Searcher (Maybe Market)
lastFound = undefined

-- #9
allFound :: Searcher [Market]
allFound = undefined

-- #10
numberFound :: Searcher Int
numberFound = undefined

-- #11
orderedNtoS :: Searcher [Market]
orderedNtoS = undefined
