module Main where

import qualified Data.HashMap.Strict as HMap
import Data.Time
import Debug.Trace
import System.Environment

import Test.Hspec.Runner (configAddFilter, defaultConfig, hspecWith, Path(..))
import Spec (spec)

data Assignment = Assignment { issued :: Day
                             , due :: Day } deriving (Eq, Show)

stuff = HMap.fromList [ ("Homework.Week1", Assignment { issued = fromGregorian 2015 12 01
                                                      , due = fromGregorian 2016 01 12 })
                      , ("Homework.Week2", Assignment { issued = fromGregorian 2016 01 13
                                                      , due = fromGregorian 2016 01 20 }) ]

main :: IO ()
main = do
  today <- getCurrentTime >>= return . utctDay
  runByTravis <- (lookupEnv "TRAVIS") >>= return . (maybe False ((==)"true"))
  let filter = (if runByTravis then (activeTestPath today) else const True)
  hspecWith (configAddFilter filter defaultConfig) Spec.spec

activeTestPath :: Day -> Path -> Bool
activeTestPath today (key:_, _) = case HMap.lookup key stuff of
  Just (Assignment issued due) -> diffDays today issued > 0
  Nothing -> True
