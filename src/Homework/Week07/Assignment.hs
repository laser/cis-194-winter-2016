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
import Data.Maybe (listToMaybe)

import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Text as T
import qualified Data.Text.IO as T

-- #1
ynToBool :: Value -> Value
ynToBool (String "Y") = Bool True
ynToBool (String "N") = Bool False
ynToBool (Array vs) = Array (fmap ynToBool vs)
ynToBool (Object vs) = Object (fmap ynToBool vs)
ynToBool v = v

-- #2
parseData :: B.ByteString -> Either String Value
parseData = fmap ynToBool . eitherDecode

-- #3
data Market = Market {
  marketname :: T.Text,
  x :: Double,
  y :: Double,
  state :: T.Text,
  website :: Maybe T.Text
} deriving (Eq, Show, Generic)

instance FromJSON Market

parseMarkets :: B.ByteString -> Either String [Market]
parseMarkets src = do
  vals <- parseData src
  case fromJSON vals of
    Success mkts -> Right mkts
    Error err -> Left err


-- #4
loadData :: IO [Market]
loadData = do
  content <- B.readFile "markets.json"
  return $ either fail id (parseMarkets content)

-- #5
data OrdList a = OrdList { getOrdList :: [a] } deriving (Eq, Show)

instance Ord a => Monoid (OrdList a) where
  mempty = OrdList []
  mappend (OrdList a) (OrdList b) = OrdList (sort $ a ++ b)

-- #6
type Searcher m = T.Text -> [Market] -> m

search :: Monoid m => (Market -> m) -> Searcher m
search f = \match mkts -> let
  found = filter (T.isInfixOf match . marketname) mkts
  in mconcat (map f found)

-- #7
compose2 :: (c -> d) -> (a -> b -> c) -> a -> b -> d
compose2 = (.) . (.)

firstFound :: Searcher (Maybe Market)
firstFound = compose2 listToMaybe (search (:[]))

-- #8
lastFound :: Searcher (Maybe Market)
lastFound = compose2 (listToMaybe . reverse) (search (:[]))

-- #9
allFound :: Searcher [Market]
allFound = search (:[])

-- #10
numberFound :: Searcher Int
numberFound = compose2 length allFound

-- #11

newtype MarketY = MarketY { unMarketY :: Market }
  deriving (Eq)

instance Ord MarketY where
  compare a b = compare (y $ unMarketY a) (y $ unMarketY b)

orderedNtoS :: Searcher [Market]
orderedNtoS match mkts = map unMarketY $ sort (map MarketY $ allFound match mkts)
