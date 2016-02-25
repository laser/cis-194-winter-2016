{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module Homework.Week07.Assignment (
  ynToBool,
  parseData,
  parseMarkets,
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
import Data.Either
import Data.Maybe


import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Text as T
import qualified Data.Text.IO as T

-- #1
ynToBool :: Value -> Value
ynToBool (String x)
  | ['Y'] == (T.unpack x) = (Bool True)
  | ['N'] == (T.unpack x) = (Bool False)
ynToBool (Array x) = Array (fmap ynToBool x)
ynToBool (Object x) = Object (fmap ynToBool x)
ynToBool x = x


-- convertYnToBool :: IO ()
-- convertYnToBool = do
--   marketsString <- B.readFile "markets.json"
--   marketsJsonObject = eitherDecode marketsString


-- #2

parseData :: B.ByteString -> Either String Value
parseData x = fmap ynToBool (eitherDecode x)

-- #3
data Market = Market { marketname :: T.Text
                     , x :: Double
                     , y :: Double
                     , state :: T.Text } deriving (Eq, Show, Generic)

instance FromJSON Market

getRight :: Either String Value -> Value
getRight (Right v) = v

parseMarkets :: B.ByteString -> Either String [Market]
-- parseMarkets x
--  | fromJSON  (parseData x) == Error "" = Left ""
--  | otherwise = Right fromJSON  (parseData x)
parseMarkets x = eitherDecode x

-- #4
 --loadData :: IO [Market]
 -- loadData = undefined
-- loadData = do
--   marketsByteString <- B.readFile "markets.json"
--   let marketsEitherJsonObject = eitherDecode marketsByteString
--   return lefts marketsEitherJsonObject

-- #5
data OrdList a = OrdList { getOrdList :: [a] } deriving (Eq, Show)

instance Ord a => Monoid (OrdList a) where
   mempty = OrdList ([])
   mappend (OrdList a) (OrdList b) = OrdList (sort (a ++ b))

-- #6
type Searcher m = T.Text -> [Market] -> m

filterMarkets :: T.Text -> [Market] -> [Market]
filterMarkets toBeSearched listOfMarkets = filter (\mkt@( Market {marketname = name}) -> (T.isInfixOf toBeSearched name) )  listOfMarkets

--search :: Monoid m => (Market -> m) -> Searcher m

search :: Monoid m => (Market -> m) -> T.Text -> [Market] -> m
search f marketNameToBeSearched listOfMarkets = mconcat (map f (filterMarkets marketNameToBeSearched listOfMarkets))

-- http://stackoverflow.com/questions/5821089/haskell-function-composition-operator-of-type-c%E2%86%92d-%E2%86%92-a%E2%86%92b%E2%86%92c-%E2%86%92-a%E2%86%92b%E2%86%92d/5822800#5822800
compose2 :: (c -> d) -> (a -> b -> c) -> a -> b -> d
-- compose2 = (.) . (.)
compose2 g f x y = g (f x y)


-- #7
-- http://stackoverflow.com/questions/2190773/in-haskell-is-there-a-built-in-function-that-creates-a-list-of-one-element
-- The return function creates a list of single element
-- firstFound :: Searcher (Maybe Market)
firstFound :: T.Text -> [Market] -> (Maybe Market)
firstFound marketNameToBeSearched listOfMarkets = compose2 listToMaybe (search return) marketNameToBeSearched listOfMarkets

-- #8
lastFound :: Searcher (Maybe Market)
lastFound marketNameToBeSearched listOfMarkets = compose2 (listToMaybe . reverse ) (search return) marketNameToBeSearched listOfMarkets

-- #9
allFound :: Searcher [Market]
allFound marketNameToBeSearched listOfMarkets = compose2 (id) (search return) marketNameToBeSearched listOfMarkets

-- #10
-- numberFound :: Searcher Int
numberFound :: T.Text -> [Market] -> Int
numberFound marketNameToBeSearched listOfMarkets = length $ filterMarkets marketNameToBeSearched listOfMarkets

instance Ord (Market)  where
    (Market _ x1 y1 _) <= (Market _ x2 y2 _) =  y1 <= y2

-- #11
-- orderedNtoS :: Searcher [Market]
orderedNtoS :: T.Text -> [Market] -> [Market]
orderedNtoS marketNameToBeSearched listOfMarkets = sort $ filterMarkets marketNameToBeSearched listOfMarkets
