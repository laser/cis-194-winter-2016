{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module Homework.Week07.Assignment (
  ynToBool,
  parseData,
  parseMarkets,
  loadData,
  search,
  firstFound,
  lastFound,
  numberFound,
  orderedNtoS,
  Market(..),
  OrdList(..),
  Searcher(..)
) where

import Data.Aeson
import Data.Monoid
import GHC.Generics

import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Text as T
import qualified Data.Text.IO as T

-- #1
ynToBool :: Value -> Value
ynToBool value = case value of
    (Object hashMap) -> Object $ fmap ynToBool hashMap
    v@(String text)  -> case T.unpack text of
        "Y"       -> Bool True
        "N"       -> Bool False
        otherwise -> v
    _ -> value

-- #2
parseData :: B.ByteString -> Either String Value
parseData s = case eitherDecode s of
    (Right obj) -> Right $ ynToBool obj
    left -> left

-- #3
data Market = Market { marketname :: String
                     , x :: Double
                     , y :: Double
                     , state :: String } deriving (Show, Generic)

instance FromJSON Market

parseMarkets :: B.ByteString -> Either String [Market]
parseMarkets = undefined

-- #4
loadData :: IO [Market]
loadData = undefined

-- #5
data OrdList a = OrdList { getOrdList :: [a] } deriving (Eq, Show)

--instance Ord a => Monoid (OrdList a) where
--  compare = ???

-- #6
type Searcher m = T.Text -> [Market] -> m

search :: Monoid m => (Market -> m) -> Searcher m
search = undefined

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
