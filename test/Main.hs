module Main where

import Data.Time
import System.Environment
import Test.Hspec (Spec)
import Test.Hspec.Runner (configAddFilter, defaultConfig, hspecWith, Path(..), Config)

import Spec (spec)

data Assignment = Assignment { name :: String, issued :: Day, due :: Day }

as :: [Assignment]
as = [ Assignment { name = "Homework.Week01", issued = fromGregorian 2015 12 01, due = fromGregorian 2016 01 12 }
     , Assignment { name = "Homework.Week02", issued = fromGregorian 2016 01 13, due = fromGregorian 2016 01 19 }
     , Assignment { name = "Homework.Week03", issued = fromGregorian 2016 01 20, due = fromGregorian 2016 01 26 }
     , Assignment { name = "Homework.Week04", issued = fromGregorian 2016 01 27, due = fromGregorian 2016 02 02 }
     , Assignment { name = "Homework.Week05", issued = fromGregorian 2016 02 03, due = fromGregorian 2016 02 09 }
     , Assignment { name = "Homework.Week06", issued = fromGregorian 2016 02 10, due = fromGregorian 2016 02 16 }
     , Assignment { name = "Homework.Week08", issued = fromGregorian 2016 02 17, due = fromGregorian 2016 02 23 }
     , Assignment { name = "Homework.Week09", issued = fromGregorian 2016 02 24, due = fromGregorian 2016 03 01 }
     , Assignment { name = "Homework.Week10", issued = fromGregorian 2016 03 02, due = fromGregorian 2016 03 08 }
     , Assignment { name = "Homework.Week11", issued = fromGregorian 2016 03 09, due = fromGregorian 2016 03 15 }
     , Assignment { name = "Homework.Week12", issued = fromGregorian 2016 03 16, due = fromGregorian 2016 03 22 }]

hspecWithPathFilter :: (Path -> Bool) -> Config -> Spec -> IO ()
hspecWithPathFilter p c s = hspecWith (configAddFilter p c) spec

runTravis :: Config -> Spec -> IO ()
runTravis config spec = do
  today <- getCurrentTime >>= return . utctDay

  let
    activeTestPath :: Day -> Path -> Bool
    activeTestPath today (key:_, _) = case take 1 $ filter (((==) key) . name) as of
      ((Assignment _ issued _):_) -> diffDays today issued >= 0
      _ -> True

  hspecWithPathFilter (activeTestPath today) config spec

runDev  :: Config -> Spec -> IO ()
runDev config spec = do
  name <- lookupEnv "CIS_194_TEST_TARGET"
  let p = maybe (const True) (\name -> \path -> case (path :: Path) of (key:_,_) -> name == key) name
  hspecWithPathFilter p config spec

main :: IO ()
main = do
  isTravisCI <- (lookupEnv "TRAVIS") >>= return . (maybe False ((==)"true"))
  (if isTravisCI then runTravis else runDev) defaultConfig Spec.spec
