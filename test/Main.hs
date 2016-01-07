module Main where

import Data.Time
import System.Environment
import Test.Hspec (Spec)
import Test.Hspec.Runner (configAddFilter, defaultConfig, hspecWith, Path(..), Config)

import Spec (spec)

hspecWithPathFilter :: (Path -> Bool) -> Config -> Spec -> IO ()
hspecWithPathFilter p c s = hspecWith (configAddFilter p c) spec

runDev  :: Config -> Spec -> IO ()
runDev config spec = do
  name <- lookupEnv "CIS_194_TEST_TARGET"
  let p = maybe (const True) (\name -> \path -> case (path :: Path) of (key:_,_) -> name == key) name
  hspecWith (configAddFilter p config) spec

main :: IO ()
main = do
  runDev defaultConfig Spec.spec
