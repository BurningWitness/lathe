module Main
  ( main
  ) where

import           Test.Lathe
import           Test.Lathe.Binary
import           Test.Lathe.Encoding.UTF16
import           Test.Lathe.Encoding.UTF32
import           Test.Lathe.Encoding.UTF8
import           Test.Lathe.Numeric.FixedWidth
import           Test.Lathe.Numeric.Fractional
import           Test.Lathe.Numeric.Integral
import           Test.Lathe.Radix

import           Test.Hspec



main :: IO ()
main =
  hspec $ do
    describe "_"                  core
    describe "Encoding.UTF16"     utf16
    describe "Encoding.UTF32"     utf32
    describe "Encoding.UTF8"      utf8
    describe "Numeric.Binary"     binary
    describe "Numeric.FixedWidth" fixedWidth
    describe "Numeric.Fractional" fractional
    describe "Numeric.Integral"   integral
    describe "Radix"              radix
