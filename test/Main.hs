module Main where

import Data.Time
import System.Environment
import Test.Hspec (Spec)
import Test.Hspec.Runner (configAddFilter, defaultConfig, hspecWith, Path(..), Config)

import qualified Homework.Week02Spec as W02
import qualified Homework.Week03Spec as W03
import qualified Homework.Week04Spec as W04
import qualified Homework.Week05Spec as W05
import qualified Homework.Week06Spec as W06
import qualified Homework.Week07Spec as W07
import qualified Homework.Week09Spec as W09
import qualified Homework.Week10Spec as W10
import qualified Homework.Week11Spec as W11
import qualified Homework.Week12Spec as W12
import Spec (spec)

data Assignment = Assignment { name :: String, issued :: Day, due :: Day, force :: Bool }

as :: [Assignment]
as = [ Assignment { name = "Homework.Week01", issued = fromGregorian 2015 12 01, due = fromGregorian 2016 01 12, force = False }
     , Assignment { name = "Homework.Week02", issued = fromGregorian 2016 01 13, due = fromGregorian 2016 01 19, force = W02.forceCIToRunThisTest }
     , Assignment { name = "Homework.Week03", issued = fromGregorian 2016 01 20, due = fromGregorian 2016 01 26, force = W03.forceCIToRunThisTest }
     , Assignment { name = "Homework.Week04", issued = fromGregorian 2016 01 27, due = fromGregorian 2016 02 02, force = W04.forceCIToRunThisTest }
     , Assignment { name = "Homework.Week05", issued = fromGregorian 2016 02 03, due = fromGregorian 2016 02 09, force = W05.forceCIToRunThisTest }
     , Assignment { name = "Homework.Week06", issued = fromGregorian 2016 02 10, due = fromGregorian 2016 02 16, force = W06.forceCIToRunThisTest }
     , Assignment { name = "Homework.Week07", issued = fromGregorian 2016 02 17, due = fromGregorian 2016 02 23, force = W07.forceCIToRunThisTest }
     , Assignment { name = "Homework.Week09", issued = fromGregorian 2016 02 24, due = fromGregorian 2016 03 01, force = W09.forceCIToRunThisTest }
     , Assignment { name = "Homework.Week10", issued = fromGregorian 2016 03 02, due = fromGregorian 2016 03 08, force = W10.forceCIToRunThisTest }
     , Assignment { name = "Homework.Week11", issued = fromGregorian 2016 03 09, due = fromGregorian 2016 03 15, force = W11.forceCIToRunThisTest }
     , Assignment { name = "Homework.Week12", issued = fromGregorian 2016 03 16, due = fromGregorian 2016 03 22, force = W12.forceCIToRunThisTest }]

hspecWithPathFilter :: (Path -> Bool) -> Config -> Spec -> IO ()
hspecWithPathFilter p c s = hspecWith (configAddFilter p c) spec

runTravis :: Config -> Spec -> IO ()
runTravis config spec = do
  today <- getCurrentTime >>= return . utctDay

  let
    activeTestPath :: Day -> Path -> Bool
    activeTestPath today (key:_, _) = case take 1 $ filter (((==) key) . name) as of
      ((Assignment _ issued _ force):_) -> force || diffDays today issued >= 0
      _ -> True

  hspecWithPathFilter (activeTestPath today) config spec

runDev  :: Config -> Spec -> IO ()
runDev config spec = do
  name <- lookupEnv "CIS_194_TEST_TARGET"
  let p = maybe (const True) (\name -> \path -> case (path :: Path) of (key:_,_) -> name == key) name
  hspecWithPathFilter p config spec

main :: IO ()
main = do
  runDev defaultConfig Spec.spec
