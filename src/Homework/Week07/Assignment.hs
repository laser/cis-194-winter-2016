{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

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

import           Data.Aeson
import           Data.List
import           Data.Monoid
import           GHC.Generics

import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Text                  as T
import qualified Data.Text.IO               as T

-- #1
ynToBool :: Value -> Value
ynToBool (String x)
  | x == "Y"  = Bool True
  | x == "N"  = Bool False
  | otherwise = String x
ynToBool (Object x) = Object $ fmap ynToBool x
ynToBool (Array x)  = Array $ fmap ynToBool x
ynToBool x          = x

-- #2
parseData :: B.ByteString -> Either String Value
-- parseData bs = ynToBool <$> eitherDecode bs
parseData = fmap ynToBool . eitherDecode

-- #3
data Market = Market { marketname :: T.Text
                     , x          :: Double
                     , y          :: Double
                     , state      :: T.Text } deriving (Eq, Show, Generic)

instance FromJSON Market

parseMarkets :: B.ByteString -> Either String [Market]
-- parseMarkets = pure <$> parseData
parseMarkets = eitherDecode

-- #4
loadData :: IO [Market]
loadData = do
  mktJSON <- B.readFile "/Users/marnold/Code/cis-194-winter-2016/src/Homework/Week07/markets.json"
  let parsed = parseMarkets mktJSON
  case parsed of
    (Right x) -> return x
    (Left x) -> fail x

-- #5
data OrdList a = OrdList { getOrdList :: [a] } deriving (Eq, Show)

instance Ord a => Monoid (OrdList a) where
  mempty = OrdList []
  mappend ol1@(OrdList xs) ol2@(OrdList ys) = OrdList $ sort $ merge xs ys

merge :: [a] -> [a] -> [a]
merge xs     []     = xs
merge []     ys     = ys
merge (x:xs) (y:ys) = x : y : merge xs ys

-- #6
type Searcher m = T.Text -> [Market] -> m

search :: Monoid m => (Market -> m) -> Searcher m
search mktToMonoid textToFind mktList = go mktList
  where go [] = mempty
        go (mkt@Market {marketname = name} : mkts)
          | T.isInfixOf textToFind name = mktToMonoid mkt <> go mkts
          | otherwise = go mkts

compose2 :: (c -> d) -> (a -> b -> c) -> a -> b -> d
compose2 = (.).(.)

-- #7
firstFound :: Searcher (Maybe Market)
firstFound textToFind mktList = case search (:[]) textToFind mktList of
  (mkt : _) -> Just mkt
  _         -> Nothing

-- #8
lastFound :: Searcher (Maybe Market)
lastFound textToFind mktList = case search (:[]) textToFind mktList of
  [mkt]       -> Just mkt
  [mkts, mkt] -> Just mkt
  _           -> Nothing

-- #9
allFound :: Searcher [Market]
allFound = search (:[])

-- #10
numberFound :: Searcher Int
numberFound textToFind mktList = length $ allFound textToFind mktList

-- #11
orderedNtoS :: Searcher [Market]
orderedNtoS textToFind mktList = sortBy nToS $ search (:[]) textToFind mktList

nToS mkt1@Market{y = y1} mkt2@Market{y = y2}
  | y1 < y2  = LT
  | y1 == y2 = EQ
  | y1 > y2  = GT
