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
import Data.Maybe
import Data.Monoid
import Data.List
import GHC.Generics

import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Text as T
import qualified Data.Text.IO as T

-- #1
ynToBool :: Value -> Value
ynToBool (String x)
  | (x == "Y") = toJSON True
  | (x == "N") = toJSON False
  | otherwise = toJSON x
ynToBool (Array x) = toJSON $ fmap ynToBool x
ynToBool (Object x) = toJSON $ fmap ynToBool x
ynToBool x = toJSON x

-- #2
parseData :: B.ByteString -> Either String Value
parseData = fmap ynToBool . eitherDecode

-- #3
data Market = Market { marketname :: T.Text
                     , x :: Double
                     , y :: Double
                     , state :: T.Text } deriving (Eq, Show, Generic)

instance FromJSON Market

parseMarkets :: B.ByteString -> Either String [Market]
parseMarkets = extractFromJSON . fmap fromJSON . parseData
  where extractFromJSON (Left err) = Left err
        extractFromJSON (Right (Error err)) = Left err
        extractFromJSON (Right (Success a)) = Right a

-- #4
loadData :: IO [Market]
loadData = undefined

-- #5
data OrdList a = OrdList { getOrdList :: [a] } deriving (Eq, Show)

instance Ord a => Monoid (OrdList a) where
  mempty = OrdList []
  mappend (OrdList x) (OrdList y) = OrdList $ sort $ x ++ y

-- #6
type Searcher m = T.Text -> [Market] -> m

search :: Monoid m => (Market -> m) -> Searcher m
search f text markets = mconcat $ map f $ filter (matchName text) markets where
  matchName text market = T.isInfixOf text $ marketname market

-- #7
compose2 :: (c -> d) -> (a -> b -> c) -> a -> b -> d
compose2 = (.) . (.)

firstFound :: Searcher (Maybe Market)
firstFound = compose2 listToMaybe allFound

-- #8
lastFound :: Searcher (Maybe Market)
lastFound = compose2 (listToMaybe . reverse) allFound

-- #9
allFound :: Searcher [Market]
allFound = search (:[])

-- #10
numberFound :: Searcher Int
numberFound = compose2 length allFound

-- #11
data MarketNtoS = MarketNtoS {market :: Market} deriving (Eq)

instance Ord MarketNtoS where
  (<=) (MarketNtoS m1) (MarketNtoS m2) = (y m1) <= (y m2)

orderedNtoS :: Searcher [Market]
orderedNtoS = compose2 ((map market) . sort . (map MarketNtoS)) allFound
