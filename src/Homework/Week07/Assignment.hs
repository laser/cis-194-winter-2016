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
  ,modifyJson
) where

import Data.Aeson
import Data.Monoid
import GHC.Generics

import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Text as T
import qualified Data.Text.IO as T
import GHC.Exts
import qualified Data.HashMap.Strict as HM
import Data.Either

-- #1
ynToBool :: Value -> Value
ynToBool (String "Y")   = Bool True
ynToBool (String "N")   = Bool False
ynToBool (Array  arr)   = Array  $ fmap ynToBool arr
ynToBool (Object obj)   = Object $ fmap ynToBool obj
ynToBool etc            = etc


-- #2
parseData :: B.ByteString -> Either String Value
parseData  = fmap ynToBool . eitherDecode

-- #3
data Market = Market { marketname :: T.Text
                     , x :: Double
                     , y :: Double
                     , state :: T.Text } deriving (Eq, Show, Generic)

instance FromJSON Market

parseMarkets :: B.ByteString -> Either String [Market]
--parseMarkets  = fmap fromJSON . parseData
parseMarkets = eitherDecode


-- #4
loadData :: IO [Market]
loadData = do
              filedata <- B.readFile "markets.json"
              return $ case parseMarkets filedata of
                     Right markets -> markets
                     Left  error   -> fail error

-- #5
data OrdList a = OrdList { getOrdList :: [a] } deriving (Eq, Show)

instance Ord a => Monoid (OrdList a) where
  mempty = OrdList []
  mappend (OrdList []  )   (OrdList [] )    = mempty
  mappend (OrdList left) (OrdList right) =
    OrdList $ go left right
              where go [] []                 = []
                    go left@(x : _) []       = left
                    go [] right@(x : _)      = right
                    go left right
                       | left < right        = head left  : go (tail left) right
                       | otherwise           = head right : go left (tail right)
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

-- extras

modifyJson :: Value -> Value
modifyJson (String x) = String $ T.reverse x
modifyJson (Array  x) = Array  $ fmap modifyJson x
modifyJson (Number x) = Number $ x * 2
modifyJson (Object x) = let revPair (k, v) = (T.reverse k, modifyJson v)
                         in Object . fromList . map revPair . HM.toList $  x
modifyJson other      = other

