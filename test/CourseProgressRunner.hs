module CourseProgressRunner (
  hspecWithDateGates
) where

import Data.Time
import Test.Hspec (Spec)
import Test.Hspec.Runner (configAddFilter, hspecWith, Path(..), Config)

import qualified Data.HashMap.Strict as HMap

data Assignment = Assignment { issued :: Day
                             , due :: Day } deriving (Eq, Show)

stuff = HMap.fromList [ ("Homework.Week1", Assignment { issued = fromGregorian 2015 12 01
                                                      , due = fromGregorian 2016 01 12 })
                      , ("Homework.Week2", Assignment { issued = fromGregorian 2016 01 13
                                                      , due = fromGregorian 2016 01 20 }) ]

activeTestPath :: Day -> Path -> Bool
activeTestPath today (key:_, _) = case HMap.lookup key stuff of
  Just (Assignment issued due) -> diffDays today issued >= 0
  Nothing -> True

hspecWithDateGates :: Config -> Spec -> IO ()
hspecWithDateGates config spec = do
  today <- getCurrentTime >>= return . utctDay
  let isActiveToday = activeTestPath today
  hspecWith (configAddFilter isActiveToday config) spec