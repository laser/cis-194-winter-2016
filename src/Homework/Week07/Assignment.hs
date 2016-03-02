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
import Data.List
import GHC.Generics

import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Text as T
import qualified Data.Text.IO as T

-- #1
ynToBool :: Value -> Value
ynToBool (String "Y") = Bool True
ynToBool (String "N") = Bool False
ynToBool (Array val) = Array (fmap ynToBool val)
ynToBool (Object val) = Object (fmap ynToBool val)
ynToBool val = val

-- #2
parseData :: B.ByteString -> Either String Value
parseData x = fmap ynToBool (eitherDecode x)

-- #3
data Market = Market { marketname :: T.Text
                     , x :: Double
                     , y :: Double
                     , state :: T.Text } deriving (Eq, Show, Generic)

instance FromJSON Market

parseMarkets :: B.ByteString -> Either String [Market]
parseMarkets x = resultToEither (fmap fromJSON (parseData x))
  where resultToEither (Right (Success markets)) = Right markets
        resultToEither (Right (Error e)) = Left e
        resultToEither (Left e) = Left e

-- #4
loadData :: IO [Market]
loadData = do
    marketFile <- B.readFile "./src/Homework/Week07/markets.json"
    case parseMarkets marketFile of
        (Right m) -> return m
        (Left e) -> fail e

-- #5
data OrdList a = OrdList { getOrdList :: [a] } deriving (Eq, Show)

instance Ord a => Monoid (OrdList a) where
  mempty = OrdList []
  mappend (OrdList a) (OrdList b) = OrdList (sort (a ++ b))

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
