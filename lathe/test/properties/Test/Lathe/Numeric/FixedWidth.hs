{-# LANGUAGE OverloadedStrings
           , ScopedTypeVariables #-}

module Test.Lathe.Numeric.FixedWidth
  ( fixedWidth
  ) where

import           Parser.Lathe
import           Parser.Lathe.Numeric.FixedWidth as O

import           Data.ByteString.Builder as I
import           Data.Foldable
import           Test.Hspec



data E = Malformed
       | EoF
         deriving (Show, Eq)

pit_ :: (Eq a, Show a) => (a -> Builder) -> (E -> E -> Parser E a) -> a -> Expectation
pit_ build tear a =
  let (Scrap _ remaining more, ei) = parse (tear Malformed EoF) (toLazyByteString $ build a)
  in (remaining, more, ei) `shouldBe` ("", End, Right a)



pit :: (Eq a, Show a) => (a -> Builder) -> (E -> E -> Parser E a) -> [a] -> Spec
pit build tear as =
  for_ as $ \a ->
    it (show a) $ pit_ build tear a



fixedWidth :: Spec
fixedWidth = do
  describe "int8HexFixed"   $ pit I.int8HexFixed   O.int8HexFixed   [0x01, -0x7F, -0x45]
  describe "word8HexFixed"  $ pit I.word8HexFixed  O.word8HexFixed  [0x01,  0xFF,  0xAB]

  describe "int16HexFixed"  $ pit I.int16HexFixed  O.int16HexFixed  [0x0102, -0x7FFE, -0x4567]
  describe "word16HexFixed" $ pit I.word16HexFixed O.word16HexFixed [0x0102,  0xFFFE,  0xABCD]

  describe "int32HexFixed"  $ pit I.int32HexFixed  O.int32HexFixed  [0x01020304, -0x7FFEFDFC, -0x45678901]  
  describe "word32HexFixed" $ pit I.word32HexFixed O.word32HexFixed [0x01020304,  0xFFFEFDFC,  0xABCDEF01]
  describe "floatHexFixed"  $ pit I.floatHexFixed  O.floatHexFixed  [        pi,      exp 50,     -log 10]

  describe "int64HexFixed"  $ pit I.int64HexFixed  O.int64HexFixed  [0x0102030405060708, -0x7FFEFDFCFBFAF9F8, -0x4567890123456789]
  describe "word64HexFixed" $ pit I.word64HexFixed O.word64HexFixed [0x0102030405060708,  0xFFFEFDFCFBFAF9F8,  0xABCDEF0123456789]
  describe "doubleHexFixed" $ pit I.doubleHexFixed O.doubleHexFixed [                pi,             exp 100,             -log 10]
