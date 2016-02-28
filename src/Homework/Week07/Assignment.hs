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

import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Text as T
import qualified Data.Text.IO as T

-- #1

-- Value -> Value. It's not creating Values, it's just
-- converting one Value to another

-- This data type does conversions to
-- Object !Object
-- Array !Array
-- String !Text
-- Number !Scientific
-- Bool !Bool
-- Null

ynToBool :: Value -> Value
ynToBool (String "Y")  = Bool True
ynToBool (String "N")  = Bool False
ynToBool (Object value) = Object $ fmap ynToBool value
ynToBool (Array value ) = Array  $ fmap ynToBool value
ynToBool val            = val


-- #2
parseData :: B.ByteString -> Either String Value
parseData = fmap ynToBool . eitherDecode
  -- parseData conv (eitherDecode bstr :: Either String Value) where
  --   conv :: Either String Value -> Either String Value
  --   conv (Right esv) = Right $ ynToBool esv
  --   conv (Left str)  = Left str

-- #3
data Market = Market { marketname :: T.Text
                     , x :: Double
                     , y :: Double
                     , state :: T.Text } deriving (Eq, Show, Generic)

instance FromJSON Market

parseMarkets :: B.ByteString -> Either String [Market]
parseMarkets bst  = let
  eVals = parseData bst
  in case eVals of
    Left x       -> Left x
    Right aValue -> case fromJSON aValue of
      Success markets -> Right markets
      Error       str -> Left str


-- #4
-- readFile :: IO B.ByteString
loadData :: IO [Market]
loadData = do
  filedata <- B.readFile "/Users/harnold/code/haskell/cis-194-winter-2016/src/Homework/Week07/markets.json"
  return $ either fail id $ parseMarkets filedata

  -- let (Right [market]) = parseMarkets filedata
  --     (Left x)         = fail x
  -- return [market]

data OrdList a = OrdList { getOrdList :: [a] } deriving (Eq, Show)

instance Ord a => Monoid (OrdList a) where
    mempty = OrdList []
    mappend (OrdList xs) (OrdList ys) = OrdList (sort $ xs ++ ys)

-- #6
-- returns a Monoid that knows how to monoidal thingies
type Searcher m = T.Text -> [Market] -> m

-- Market {}
--search :: Monoid m => (Market -> m) -> T.Text -> [Market] -> m
-- Data.Text.isInfixOf ??

-- mktToBool :: T.Text -> Market -> Bool
-- mktToBool text mkt = text `T.isInfixOf` marketname mkt

search :: Monoid m => (Market -> m) -> Searcher m
search mkf text ms = mconcat $ search' text ms
  where search' _ [] = mempty
        search' text (m : ms)
          | text `T.isInfixOf` marketname m = mkf m : search' text ms
          | otherwise = search' text ms

compose2 :: (b -> c) -> (a -> a1 -> b) -> a -> a1 -> c
compose2 = (.) . (.)

-- #7
-- type Searcher m = T.Text -> [Market] -> m
firstFound :: T.Text -> [Market] -> (Maybe Market)
--firstFound  = compose2 getFirst $ search (First . Just)
-- firstFound = compose2 getFirst (search (\m -> First (Just m)))
firstFound  = compose2 getFirst $ search (First . Just)


-- #8
lastFound :: Searcher (Maybe Market)
lastFound = compose2 getLast $ search (Last . Just)

-- #9 this is actually 'allThatMeetTheCriteria' and not
-- the all from Bool. So just build a list
allFound :: Searcher [Market]
allFound = search (:[])

-- #10
-- type Searcher m = T.Text -> [Market] -> m
-- this is a sum of ints. Since we can't use length,
-- we have to use a Sum type: Sum {setSum :: Int}
numberFound :: Searcher Int
numberFound = compose2 getSum (search (\m -> Sum 1 ))

-- 'y' is the north/south latitude, so (y market) give the latitude for
-- each market value
instance Ord Market where
    compare a b = compare (y a) (y b)

-- compiler told me that I needed an instance of Ord for Market, and it
-- can only have one function called 'compare', so..
-- -- #11
orderedNtoS :: Searcher [Market]
orderedNtoS = getOrdList `compose2` search (\m -> OrdList [m])
