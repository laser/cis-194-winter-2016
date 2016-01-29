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
  Searcher
) where

import Control.Monad (join)
import Data.Aeson    hiding ((.:))
import Data.Monoid
import GHC.Generics

import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Text as T

-- #1
ynToBool :: Value -> Value
ynToBool (String "Y") = Bool True
ynToBool (String "N") = Bool False
ynToBool (Array vs)   = Array $ fmap ynToBool vs
ynToBool (Object vs)  = Object $ fmap ynToBool vs
ynToBool other        = other

-- #2
parseData :: B.ByteString -> Either String Value
parseData = fmap ynToBool . eitherDecode

-- #3
data Market = Market { marketname :: T.Text
                     , x :: Double
                     , y :: Double
                     , state :: T.Text
                     } deriving (Eq, Show, Generic)

instance FromJSON Market

resultToEither :: Result a -> Either String a
resultToEither (Success a) = Right a
resultToEither (Error str) = Left str

parseMarkets :: B.ByteString -> Either String [Market]
parseMarkets = join . fmap (resultToEither . fromJSON) . parseData

-- #4
fromEither :: (a -> c) -> (b -> c) -> Either a b -> c
fromEither f _ (Left l)  = f l
fromEither _ f (Right r) = f r

loadData :: IO [Market]
loadData = eitherToIO . parseMarkets =<< B.readFile "markets.json"
  where eitherToIO = fromEither fail return

-- #5
data OrdList a = OrdList { getOrdList :: [a] }
  deriving (Eq, Show)

instance Ord a => Monoid (OrdList a) where
  mempty = OrdList []
  mappend xs (OrdList []) = xs
  mappend (OrdList []) ys = ys
  mappend a@(OrdList (m:ms)) b@(OrdList (n:ns))
    | m < n     = OrdList $ m : getOrdList (OrdList ms <> b)
    | otherwise = OrdList $ n : getOrdList (a <> OrdList ns)

-- #6
type Searcher m = T.Text -> [Market] -> m

infixr 8 .:
(.:) :: (a -> b) -> (c -> d -> a) -> c -> d -> b
(.:) f g a b = f (g a b)

simpleSearch :: Searcher [Market]
simpleSearch txt = filter (T.isInfixOf txt . marketname)

search :: Monoid m => (Market -> m) -> Searcher m
search f = mconcat . map f .: simpleSearch

-- #7
firstFound :: Searcher (Maybe Market)
firstFound = getFirst .: search (First . Just)

-- #8
lastFound :: Searcher (Maybe Market)
lastFound = getLast .: search (Last . Just)

-- #9
allFound :: Searcher [Market]
allFound = simpleSearch

-- #10
numberFound :: Searcher Int
numberFound = getSum .: search (Sum . const 1)

-- #11
newtype NtoS = NtoS { getNtoS :: Market }
  deriving (Eq, Show)

instance Ord NtoS where
  compare (NtoS Market{y = a}) (NtoS Market{y = b}) = compare a b

orderedNtoS :: Searcher [Market]
orderedNtoS = map getNtoS . getOrdList .: search (OrdList . (:[]) . NtoS)
