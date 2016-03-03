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
import Data.List
import Data.Monoid
import GHC.Generics

import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Text as T
import qualified Data.Text.IO as T

-- #1
ynToBool :: Value -> Value
ynToBool (String "Y") = Bool True
ynToBool (String "N") = Bool False
ynToBool (Array xs)   = Array (fmap ynToBool xs)
ynToBool (Object xs)  = Object (fmap ynToBool xs)
ynToBool x            = x

-- #2
parseData :: B.ByteString -> Either String Value
parseData x = fmap ynToBool (eitherDecode x)

-- #3
data Market = Market {
    marketname :: T.Text,
    x :: Double,
    y :: Double,
    state :: T.Text
} deriving (Eq, Show, Generic)

instance FromJSON Market

parseMarkets :: B.ByteString -> Either String [Market]
parseMarkets = eitherDecode

-- #4
loadData :: IO [Market]
loadData = do
    jsonMarkets <- B.readFile "markets.json"
    either fail return $ parseMarkets jsonMarkets

-- #5
data OrdList a = OrdList {
    getOrdList :: [a]
} deriving (Eq, Show)

instance Ord a => Monoid (OrdList a) where
    mempty = OrdList []
    mappend (OrdList lhsList) (OrdList rhsList) = OrdList (sort $ lhsList <> rhsList)

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
