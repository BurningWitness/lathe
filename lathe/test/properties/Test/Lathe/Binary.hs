{-# LANGUAGE OverloadedStrings
           , ScopedTypeVariables #-}

module Test.Lathe.Binary
  ( binary
  ) where

import           Parser.Lathe
import           Parser.Lathe.Binary as O

import           Data.ByteString.Builder as I
import           Data.Foldable (for_)
import           Test.Hspec



pit_ :: (Eq a, Show a) => (a -> Builder) -> (() -> Parser () a) -> a -> Expectation
pit_ build tear a =
  let (Scrap _ remaining more, ei) = parse (tear ()) (toLazyByteString $ build a)
  in (remaining, more, ei) `shouldBe` ("", End, Right a)



pit :: (Eq a, Show a) => (a -> Builder) -> (() -> Parser () a) -> [a] -> Spec
pit build tear as =
  for_ as $ \a ->
    it (show a) $ pit_ build tear a



binary :: Spec
binary = do
  describe "int8"       $ pit I.int8     O.int8     [0x01, -0x7F, -0x45]
  describe "word8"      $ pit I.word8    O.word8    [0x01,  0xFF,  0xAB]

  describe "int16BE"    $ pit I.int16BE  O.int16BE    [0x0102, -0x7FFE, -0x4567]
  describe "int16LE"    $ pit I.int16LE  O.int16LE    [0x0102, -0x7FFE, -0x4567]
  describe "word16BE"   $ pit I.word16BE O.word16BE   [0x0102,  0xFFFE,  0xABCD]
  describe "word16LE"   $ pit I.word16LE O.word16LE   [0x0102,  0xFFFE,  0xABCD]

  describe "int32BE"     $ pit I.int32BE  O.int32BE    [0x01020304, -0x7FFEFDFC, -0x45678901]
  describe "int32LE"     $ pit I.int32LE  O.int32LE    [0x01020304, -0x7FFEFDFC, -0x45678901]
  describe "word32BE"    $ pit I.word32BE O.word32BE   [0x01020304,  0xFFFEFDFC,  0xABCDEF01]
  describe "word32LE"    $ pit I.word32LE O.word32LE   [0x01020304,  0xFFFEFDFC,  0xABCDEF01]
  describe "floatBE"     $ pit I.floatBE  O.floatBE    [        pi,      exp 50,     -log 10]
  describe "floatLE"     $ pit I.floatLE  O.floatLE    [        pi,      exp 50,     -log 10]

  describe "int64BE"    $ pit I.int64BE  O.int64BE    [0x0102030405060708, -0x7FFEFDFCFBFAF9F8, -0x4567890123456789]
  describe "int64LE"    $ pit I.int64LE  O.int64LE    [0x0102030405060708, -0x7FFEFDFCFBFAF9F8, -0x4567890123456789]
  describe "word64BE"   $ pit I.word64BE O.word64BE   [0x0102030405060708,  0xFFFEFDFCFBFAF9F8,  0xABCDEF0123456789]
  describe "word64LE"   $ pit I.word64LE O.word64LE   [0x0102030405060708,  0xFFFEFDFCFBFAF9F8,  0xABCDEF0123456789]
  describe "doubleBE"   $ pit I.doubleBE O.doubleBE   [                pi,             exp 100,             -log 10]
  describe "doubleLE"   $ pit I.doubleLE O.doubleLE   [                pi,             exp 100,             -log 10]
