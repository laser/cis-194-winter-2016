module Main where

import System.Environment
import Test.Hspec.Runner (defaultConfig, hspecWith, Config)

import Spec (spec)
import CourseProgressRunner (hspecWithDateGates)

main :: IO ()
main = do
  isDev <- (lookupEnv "TRAVIS") >>= return . not . (maybe False ((==)"true"))
  (if isDev then hspecWith else hspecWithDateGates) defaultConfig Spec.spec