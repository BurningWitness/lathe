module Main where

import           Test.Lathe.Time

import           Test.Hspec



main :: IO ()
main =
  hspec $
    describe "Time" time
