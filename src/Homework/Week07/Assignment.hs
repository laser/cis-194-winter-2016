
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

) where

import Data.Aeson
import Data.Monoid
import GHC.Generics
import Data.List

import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Text as T
import qualified Data.Text.IO as T


-- #1
ynToBool :: Value -> Value
ynToBool (String "Y") =  Bool True
ynToBool (String "N") = Bool False
ynToBool (Array array) = Array (fmap ynToBool array)
ynToBool (Object object) = Object (fmap ynToBool object)
ynToBool other = other

-- #2
parseData :: B.ByteString -> Either String Value
parseData string = ynToBool <$> eitherDecode string

-- #3
data Market = Market { marketname :: T.Text
                     , x :: Double
                     , y :: Double
                     , state :: T.Text } deriving (Eq, Show, Generic)

instance FromJSON Market

parseMarkets :: B.ByteString -> Either String [Market]
parseMarkets marketString = (getResult . fromJSON) <$> parseData marketString

getResult :: Result[Market] -> [Market]
getResult (Success result) = result

-- #4
loadData :: IO [Market]
loadData = do
  filedata <- B.readFile "markets.json"
  return (either fail id (parseMarkets filedata))

-- #5
data OrdList a = OrdList { getOrdList :: [a] } deriving (Eq, Show)

instance Ord a => Monoid (OrdList a) where
  mempty = OrdList []
  mappend list1 list2 = OrdList (sort(getOrdList list1 `mappend` getOrdList list2))

-- #6
type Searcher m = T.Text -> [Market] -> m

search :: Monoid m => (Market -> m) -> Searcher m
-- search :: Monoid m => (Market -> m) -> T.Text -> [Market] -> m
search func text marketList = doSearch marketList
                                where doSearch [] = mempty
                                      doSearch (first @ (Market { marketname = name }) : rest)
                                        | T.isInfixOf text name = func first <> doSearch rest
                                        | otherwise = doSearch rest

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x : _ ) = Just x

-- -- #7
firstFound :: Searcher (Maybe Market)
firstFound text marketList = safeHead $ allFound text marketList

-- #8
lastFound :: Searcher (Maybe Market)
lastFound text marketList = safeHead(reverse(allFound text marketList))

-- #9
allFound :: Searcher [Market]
allFound = search (:[])

instance Monoid Int where
  mempty = 0
  mappend = (+)

-- #10
numberFound :: Searcher Int
numberFound = search (const 1)

instance Ord Market where
  (Market _ _ y_1 _ ) `compare` (Market _ _ y_2 _) = y_1 `compare` y_2

-- #11
orderedNtoS :: Searcher [Market]
orderedNtoS text marketList = sort(allFound text marketList)
