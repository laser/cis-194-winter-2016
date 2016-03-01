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

import Data.Aeson   hiding ((.:))
import Data.Monoid
import GHC.Generics
import Data.List
import Data.Maybe

import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Text as T
import qualified Data.Text.IO as T

-- #1

ynToBool :: Value -> Value
ynToBool (String "Y") = Bool True
ynToBool (String "N") = Bool False
ynToBool (Object hashMap) = Object (fmap ynToBool hashMap)
ynToBool (Array array) = Array (fmap ynToBool array)
ynToBool otherType = otherType

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
parseMarkets src = do
  vals <- parseData src
  case fromJSON vals of
    Success markets -> Right markets
    Error err -> Left err

-- #4
loadData :: IO [Market]
loadData = do
  marketsJSON <- B.readFile "markets.json"
  return (either fail id (parseMarkets marketsJSON))

-- #5
data OrdList a = OrdList { getOrdList :: [a] } deriving (Eq, Show)

instance Ord a => Monoid (OrdList a) where
   mempty = OrdList []
   mappend (OrdList a) (OrdList b) = OrdList (sort $ a ++ b)


-- #6
type Searcher m = T.Text -> [Market] -> m

getName :: Market -> T.Text
getName mkt@(Market { marketname = name }) = name

search :: Monoid m => (Market -> m) -> Searcher m
search toMonoid text mkts = mconcat $ map toMonoid $ filter hasName mkts
  where hasName mkt = T.isInfixOf text (getName mkt)

-- #7
compose2 :: (c -> d) -> (a -> b -> c) -> a -> b -> d
compose2 = (.) . (.)

firstFound :: Searcher (Maybe Market)
firstFound = compose2 listToMaybe (search (:[]))

-- #8
lastFound :: Searcher (Maybe Market)
lastFound = compose2 (listToMaybe . reverse)  (search (:[]))

-- #9
allFound :: Searcher [Market]
allFound = search (:[])

-- #10
numberFound :: Searcher Int
numberFound = compose2 length (search (:[]))

-- #11
newtype MarketNS = MarketNS { getNtoS:: Market }
  deriving (Eq)

instance Ord MarketNS where
  compare (MarketNS Market{y = a}) (MarketNS Market{y = b}) = compare a b

orderedNtoS :: Searcher [Market]
orderedNtoS text mkt = map getNtoS $ sortBy compare $ map MarketNS $ allFound text mkt
