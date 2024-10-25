{-# LANGUAGE OverloadedStrings
           , StandaloneDeriving
           , TypeApplications #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Lathe.Numeric.Fractional where

import           Parser.Lathe
import           Parser.Lathe.Numeric.Fractional

import           Data.ByteString.Builder as I
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as L
import           Data.Foldable
import           Test.Hspec



deriving instance Eq a => Eq (OverUnder a)
deriving instance Eq a => Eq (FracWord a)
deriving instance Eq a => Eq (FracInt a)
deriving instance Eq a => Eq (FracFloat a)



pit_
  :: (Eq a, Eq e, Show a, Show e)
  => Parser e a -> B.ByteString -> L.ByteString -> Either e a -> Expectation
pit_ tear build rmn res =
  let (Scrap _ remaining more, ei) = parse tear (toLazyByteString $ I.byteString build)
  in (remaining, more, ei) `shouldBe` (rmn, End, res)



pit
  :: (Eq a, Eq e, Show a, Show e)
  => Parser e a -> [(B.ByteString, L.ByteString, Either e a)] -> Spec
pit tear brs =
  for_ brs $ \(raw, rmn, res) ->
    it (B.unpack raw) $
      pit_ tear raw rmn res



fractional :: Spec
fractional = do
  describe "fracToWord8" $ do
    it "1"     $ fracToWord8 (FracWord 1    1) 1 `shouldBe` Proper 1
    it "7.3e1" $ fracToWord8 (FracWord 73   2) 2 `shouldBe` Proper 73
    it "22e1"  $ fracToWord8 (FracWord 22   2) 3 `shouldBe` Proper 220
    it "255"   $ fracToWord8 (FracWord 0xFF 3) 3 `shouldBe` Proper 0xFF
    it "1e3"   $ fracToWord8 (FracWord 1    1) 4 `shouldBe` Over
    it "57e7"  $ fracToWord8 (FracWord 57   2) 9 `shouldBe` Over
    it "4.6"   $ fracToWord8 (FracWord 46   2) 1 `shouldBe` Under
    it "32e-4" $ fracToWord8 (FracWord 32   2) 0 `shouldBe` Under

  describe "fracToWord16" $ do
    it "1"       $ fracToWord16 (FracWord 1      1) 1  `shouldBe` Proper 1
    it "7.3e1"   $ fracToWord16 (FracWord 73     2) 2  `shouldBe` Proper 73
    it "22e2"    $ fracToWord16 (FracWord 22     2) 4  `shouldBe` Proper 2200
    it "65535"   $ fracToWord16 (FracWord 0xFFFF 3) 3  `shouldBe` Proper 0xFFFF
    it "1e5"     $ fracToWord16 (FracWord 1      1) 6  `shouldBe` Over
    it "57e10"   $ fracToWord16 (FracWord 57     2) 12 `shouldBe` Over
    it "465.7"   $ fracToWord16 (FracWord 4657   4) 3  `shouldBe` Under
    it "3298e-8" $ fracToWord16 (FracWord 3298   4) 0  `shouldBe` Under

  describe "fracToWord32" $ do
    it "1"          $ fracToWord32 (FracWord 1          1 ) 1  `shouldBe` Proper 1
    it "7.3e1"      $ fracToWord32 (FracWord 73         2 ) 2  `shouldBe` Proper 73
    it "2233e3"     $ fracToWord32 (FracWord 2233       4 ) 7  `shouldBe` Proper 2233000
    it "4294967295" $ fracToWord32 (FracWord 0xFFFFFFFF 10) 10 `shouldBe` Proper 0xFFFFFFFF
    it "1e10"       $ fracToWord32 (FracWord 1          1 ) 11 `shouldBe` Over
    it "57e20"      $ fracToWord32 (FracWord 57         2 ) 22 `shouldBe` Over
    it "46537.8"    $ fracToWord32 (FracWord 465378     6 ) 5  `shouldBe` Under
    it "329815e-6"  $ fracToWord32 (FracWord 329815     6 ) 0  `shouldBe` Under

  describe "fracToWord64" $ do
    it "1"                    $ fracToWord64 (FracWord 1                  1 ) 1  `shouldBe` Proper 1
    it "7.3e1"                $ fracToWord64 (FracWord 73                 2 ) 2  `shouldBe` Proper 73
    it "22334455e8"           $ fracToWord64 (FracWord 22334455           8 ) 16 `shouldBe` Proper 2233445500000000
    it "18446744073709551615" $ fracToWord64 (FracWord 0xFFFFFFFFFFFFFFFF 20) 20 `shouldBe` Proper 0xFFFFFFFFFFFFFFFF
    it "1e20"                 $ fracToWord64 (FracWord 1                  1 ) 21 `shouldBe` Over
    it "57e40"                $ fracToWord64 (FracWord 57                 2 ) 42 `shouldBe` Over
    it "4653978.1"            $ fracToWord64 (FracWord 46539781           8 ) 7  `shouldBe` Under
    it "32981506e-8"          $ fracToWord64 (FracWord 32981506           8 ) 0  `shouldBe` Under

  describe "fracToWord" $ do
    it "1"         $ fracToWord (FracWord 1      1) 1   `shouldBe` Proper 1
    it "7.3e1"     $ fracToWord (FracWord 73     2) 2   `shouldBe` Proper 73
    it "2233e2"    $ fracToWord (FracWord 2233   4) 6   `shouldBe` Proper 223300
    it "57e100"    $ fracToWord (FracWord 57     2) 102 `shouldBe` Over
    it "4653.9"    $ fracToWord (FracWord 46539  5) 4   `shouldBe` Under
    it "329815e-6" $ fracToWord (FracWord 329815 6) 0   `shouldBe` Under


  describe "fracToInt8" $ do
    it "1"      $ fracToInt8 Plus  (FracInt 1    1) 1 `shouldBe` Proper 1
    it "7.3e1"  $ fracToInt8 Plus  (FracInt 73   2) 2 `shouldBe` Proper 73
    it "-11e1"  $ fracToInt8 Minus (FracInt 11   2) 3 `shouldBe` Proper (-110)
    it "127"    $ fracToInt8 Plus  (FracInt 0x7F 3) 3 `shouldBe` Proper 0x7F
    it "-128"   $ fracToInt8 Minus (FracInt 0x80 3) 3 `shouldBe` Proper (-0x80)
    it "1e3"    $ fracToInt8 Plus  (FracInt 1    1) 4 `shouldBe` Over
    it "-57e7"  $ fracToInt8 Minus (FracInt 57   2) 9 `shouldBe` Over
    it "4.6"    $ fracToInt8 Plus  (FracInt 46   2) 1 `shouldBe` Under
    it "-32e-4" $ fracToInt8 Minus (FracInt 32   2) 0 `shouldBe` Under

  describe "fracToInt16" $ do
    it "1"        $ fracToInt16 Plus  (FracInt 1      1) 1  `shouldBe` Proper 1
    it "7.3e1"    $ fracToInt16 Plus  (FracInt 73     2) 2  `shouldBe` Proper 73
    it "-22e2"    $ fracToInt16 Minus (FracInt 22     2) 4  `shouldBe` Proper (-2200)
    it "32767"    $ fracToInt16 Plus  (FracInt 0x7FFF 3) 3  `shouldBe` Proper 0x7FFF
    it "-32768"   $ fracToInt16 Minus (FracInt 0x8000 3) 3  `shouldBe` Proper (-0x8000)
    it "1e5"      $ fracToInt16 Plus  (FracInt 1      1) 6  `shouldBe` Over
    it "-57e10"   $ fracToInt16 Minus (FracInt 57     2) 12 `shouldBe` Over
    it "465.7"    $ fracToInt16 Plus  (FracInt 4657   4) 3  `shouldBe` Under
    it "-3298e-8" $ fracToInt16 Minus (FracInt 3298   4) 0  `shouldBe` Under

  describe "fracToInt32" $ do
    it "1"           $ fracToInt32 Plus  (FracInt 1          1 ) 1  `shouldBe` Proper 1
    it "7.3e1"       $ fracToInt32 Plus  (FracInt 73         2 ) 2  `shouldBe` Proper 73
    it "-2233e3"     $ fracToInt32 Minus (FracInt 2233       4 ) 7  `shouldBe` Proper (-2233000)
    it "2147483647"  $ fracToInt32 Plus  (FracInt 0x7FFFFFFF 10) 10 `shouldBe` Proper 0x7FFFFFFF
    it "-2147483648" $ fracToInt32 Minus (FracInt 0x80000000 10) 10 `shouldBe` Proper (-0x80000000)
    it "1e10"        $ fracToInt32 Plus  (FracInt 1          1 ) 11 `shouldBe` Over
    it "-57e20"      $ fracToInt32 Minus (FracInt 57         2 ) 22 `shouldBe` Over
    it "46537.8"     $ fracToInt32 Plus  (FracInt 465378     6 ) 5  `shouldBe` Under
    it "-329815e-6"  $ fracToInt32 Minus (FracInt 329815     6 ) 0  `shouldBe` Under

  describe "fracToInt64" $ do
    it "1"                    $ fracToInt64 Plus  (FracInt 1                  1 ) 1  `shouldBe` Proper 1
    it "7.3e1"                $ fracToInt64 Plus  (FracInt 73                 2 ) 2  `shouldBe` Proper 73
    it "-22334455e8"          $ fracToInt64 Minus (FracInt 22334455           8 ) 16 `shouldBe` Proper (-2233445500000000)
    it "9223372036854775807"  $ fracToInt64 Plus  (FracInt 0x7FFFFFFFFFFFFFFF 19) 19 `shouldBe` Proper 0x7FFFFFFFFFFFFFFF
    it "-9223372036854775808" $ fracToInt64 Minus (FracInt 0x8000000000000000 19) 19 `shouldBe` Proper (-0x8000000000000000)
    it "1e19"                 $ fracToInt64 Plus  (FracInt 1                  1 ) 20 `shouldBe` Over
    it "57e20"                $ fracToInt64 Minus (FracInt 57                 2 ) 40 `shouldBe` Over
    it "4653978.1"            $ fracToInt64 Plus  (FracInt 46539781           8 ) 7  `shouldBe` Under
    it "32981506e-8"          $ fracToInt64 Minus (FracInt 32981506           8 ) 0  `shouldBe` Under

  describe "fracToInt" $ do
    it "1"         $ fracToInt Plus  (FracInt 1      1) 1   `shouldBe` Proper 1
    it "-7.3e1"    $ fracToInt Minus (FracInt 73     2) 2   `shouldBe` Proper (-73)
    it "-2233e2"   $ fracToInt Minus (FracInt 2233   4) 6   `shouldBe` Proper (-223300)
    it "57e100"    $ fracToInt Plus  (FracInt 57     2) 102 `shouldBe` Over
    it "-4653.9"   $ fracToInt Minus (FracInt 46539  5) 4   `shouldBe` Under
    it "329815e-6" $ fracToInt Plus  (FracInt 329815 6) 0   `shouldBe` Under


  describe "fracToFloat" $ do
    it "1"                 $ fracToFloat Plus  (FracFloat 1         1) 1     `shouldBe` 1
    it "-1.4012984643e-45" $ fracToFloat Minus (FracFloat 140129846 9) (-44) `shouldBe` -1e-45
    it "1.1754942107e-38"  $ fracToFloat Plus  (FracFloat 117549421 9) (-37) `shouldBe` 1.1754942e-38
    it "-1.1754943508e-38" $ fracToFloat Minus (FracFloat 117549435 9) (-37) `shouldBe` -1.1754944e-38
    it "3.4028234664e38"   $ fracToFloat Plus  (FracFloat 340282346 9) 39    `shouldBe` 3.4028235e38
    it "-0.99999994039535" $ fracToFloat Minus (FracFloat 999999940 9) 0     `shouldBe` -0.99999994
    it "3.14159274101257"  $ fracToFloat Plus  (FracFloat 314159274 9) 1     `shouldBe` 3.1415927
    it "-506.07e-7"        $ fracToFloat Minus (FracFloat 50607     5) (-4)  `shouldBe` -5.0607e-5
    it "1234567e7"         $ fracToFloat Plus  (FracFloat 1234567   7) 14    `shouldBe` 1234567e7

  describe "fracToDouble" $ do
    it "1"                        $ fracToDouble Plus  (FracFloat 1                 1 ) 1      `shouldBe` 1
    it "-4.9406564584124654e-324" $ fracToDouble Minus (FracFloat 49406564584124654 17) (-323) `shouldBe` -5e-324
    it "2.2250738585072009e-308"  $ fracToDouble Plus  (FracFloat 22250738585072009 17) (-307) `shouldBe` 2.2250738585072009e-308
    it "2.2250738585072014e-308"  $ fracToDouble Minus (FracFloat 22250738585072014 17) (-307) `shouldBe` -2.2250738585072014e-308
    it "1.7976931348623157e308"   $ fracToDouble Plus  (FracFloat 17976931348623157 17) 309    `shouldBe` 1.7976931348623157e308
    it "-0.9999999999999999"      $ fracToDouble Minus (FracFloat 9999999999999999  16) 0      `shouldBe` -0.9999999999999999
    it "3.141592653589793"        $ fracToDouble Plus  (FracFloat 3141592653589793  16) 1      `shouldBe` 3.141592653589793
    it "-506.070809e-12"          $ fracToDouble Minus (FracFloat 506070809         9 ) (-9)  `shouldBe` -5.06070809e-10
    it "12345678987e9"            $ fracToDouble Plus  (FracFloat 12345678987       11) 20    `shouldBe` 12345678987e9


  describe "fracWord8Dec" $ do
    describe "Zero" $
      pit (fracWord8Dec () (FracWord 0 0) 0)
        [ ("00000", "", Right (FracWord 0 0))
        , ("1/", "/", Right (FracWord 1 1))
        , ("78", "", Right (FracWord 78 2))
        , ("255:", ":", Right (FracWord 0xFF 3))
        , ("256", "6", Left ())
        , ("304", "4", Left ())
        , ("1001", "1", Left ())
        ]

    describe "One" $
      pit (fracWord8Dec () (FracWord 2 1) 1)
        [ ("00000", "", Right (FracWord 2 1))
        , ("1/", "/", Right (FracWord 21 2))
        , ("55:", ":", Right (FracWord 0xFF 3))
        , ("56", "6", Left ())
        , ("001", "1", Left ())
        ]

    describe "One-Two" $
      pit (fracWord8Dec () (FracWord 2 1) 2)
        [ ("0", "", Right (FracWord 2 1))
        , ("1", "", Right (FracWord 201 3))
        ]

  describe "fracWord16Dec" $ do
    describe "Zero" $
      pit (fracWord16Dec () (FracWord 0 0) 0)
        [ ("00000", "", Right (FracWord 0 0))
        , ("1/", "/", Right (FracWord 1 1))
        , ("78", "", Right (FracWord 78 2))
        , ("2004", "", Right (FracWord 2004 4))
        , ("65535:", ":", Right (FracWord 0xFFFF 5))
        , ("65536", "6", Left ())
        , ("65601", "1", Left ())
        , ("100001", "1", Left ())
        ]

    describe "Three" $
      pit (fracWord16Dec () (FracWord 655 3) 3)
        [ ("00000", "", Right (FracWord 655 3))
        , ("1/", "/", Right (FracWord 6551 4))
        , ("01", "", Right (FracWord 65501 5))
        , ("35:", ":", Right (FracWord 0xFFFF 5))
        , ("36", "6", Left ())
        , ("001", "1", Left ())
        ]

    describe "Two-Three" $
      pit (fracWord16Dec () (FracWord 65 2) 3)
        [ ("00", "", Right (FracWord 65 2))
        , ("10", "", Right (FracWord 6501 4))
        , ("01", "", Right (FracWord 65001 5))
        ]

  describe "fracWord32Dec" $ do
    describe "Zero" $
      pit (fracWord32Dec () (FracWord 0 0) 0)
        [ ("00000", "", Right (FracWord 0 0))
        , ("1/", "/", Right (FracWord 1 1))
        , ("78", "", Right (FracWord 78 2))
        , ("20030004", "", Right (FracWord 20030004 8))
        , ("4294967295:", ":", Right (FracWord 0xFFFFFFFF 10))
        , ("4294967296", "6", Left ())
        , ("4294970001", "1", Left ())
        , ("10000000001", "1", Left ())
        ]

    describe "Five" $
      pit (fracWord32Dec () (FracWord 42949 5) 5)
        [ ("00000", "", Right (FracWord 42949 5))
        , ("1/", "/", Right (FracWord 429491 6))
        , ("001", "", Right (FracWord 42949001 8))
        , ("67295:", ":", Right (FracWord 0xFFFFFFFF 10))
        , ("67296", "6", Left ())
        , ("67301", "1", Left ())
        , ("000001", "1", Left ())
        ]

    describe "Three-Five" $
      pit (fracWord32Dec () (FracWord 429 3) 5)
        [ ("000", "", Right (FracWord 429 3))
        , ("100", "", Right (FracWord 429001 6))
        , ("001", "", Right (FracWord 42900001 8))
        ]

  describe "fracWord64Dec" $ do
    describe "Zero" $
      pit (fracWord64Dec () (FracWord 0 0) 0)
        [ ("00000", "", Right (FracWord 0 0))
        , ("1/", "/", Right (FracWord 1 1))
        , ("78", "", Right (FracWord 78 2))
        , ("2003000400005", "", Right (FracWord 2003000400005 13))
        , ("18446744073709551615:", ":", Right (FracWord 0xFFFFFFFFFFFFFFFF 20))
        , ("18446744073709551616", "6", Left ())
        , ("18446744080000000001", "1", Left ())
        , ("100000000000000000001", "1", Left ())
        ]

    describe "Ten" $
      pit (fracWord64Dec () (FracWord 1844674407 10) 10)
        [ ("00000", "", Right (FracWord 1844674407 10))
        , ("1/", "/", Right (FracWord 18446744071 11))
        , ("0001", "", Right (FracWord 18446744070001 14))
        , ("3709551615:", ":", Right (FracWord 0xFFFFFFFFFFFFFFFF 20))
        , ("3709551616", "6", Left ())
        , ("3709600001", "1", Left ())
        , ("00000000001", "1", Left ())
        ]

    describe "Five-Ten" $
      pit (fracWord64Dec () (FracWord 18446 5) 10)
        [ ("0000", "", Right (FracWord 18446 5))
        , ("1000", "", Right (FracWord 18446000001 11))
        , ("0001", "", Right (FracWord 18446000000001 14))
        ]

  describe "fracWordDec" $ do
    describe "Plus" $ do
      pit (fracWordDec () (FracWord 0 0) 0)
        [ ("00000", "", Right (FracWord 0 0))
        , ("1/", "/", Right (FracWord 1 1))
        , ("78", "", Right (FracWord 78 2))
        , ("203004", "", Right (FracWord 203004 6))
        ]

    describe "Three" $
      pit (fracWordDec () (FracWord 123 3) 3)
        [ ("00000", "", Right (FracWord 123 3))
        , ("1/", "/", Right (FracWord 1231 4))
        , ("0001", "", Right (FracWord 1230001 7))
        ]

    describe "Two-three" $
      pit (fracWordDec () (FracWord 12 2) 3)
        [ ("0000", "", Right (FracWord 12 2))
        , ("1000", "", Right (FracWord 1201 4))
        , ("0001", "", Right (FracWord 1200001 7))
        ]


  describe "fracInt8Dec" $ do
    describe "Plus" $ do
      describe "Zero" $
        pit (fracInt8Dec () Plus (FracInt 0 0) 0)
          [ ("00000", "", Right (FracInt 0 0))
          , ("1/", "/", Right (FracInt 1 1))
          , ("78", "", Right (FracInt 78 2))
          , ("127:", ":", Right (FracInt 0x7F 3))
          , ("128", "8", Left ())
          , ("204", "4", Left ())
          , ("1001", "1", Left ())
          ]

      describe "One" $
        pit (fracInt8Dec () Plus (FracInt 1 1) 1)
          [ ("00000", "", Right (FracInt 1 1))
          , ("1/", "/", Right (FracInt 11 2))
          , ("27:", ":", Right (FracInt 0x7F 3))
          , ("28", "8", Left ())
          , ("001", "1", Left ())
          ]

      describe "One-Two" $
        pit (fracInt8Dec () Plus (FracInt 1 1) 2)
          [ ("0", "", Right (FracInt 1 1))
          , ("1", "", Right (FracInt 101 3))
          ]

    describe "Minus" $ do
      describe "Zero" $
        pit (fracInt8Dec () Minus (FracInt 0 0) 0)
          [ ("00000", "", Right (FracInt 0 0))
          , ("1/", "/", Right (FracInt 1 1))
          , ("108", "", Right (FracInt 108 3))
          , ("128:", ":", Right (FracInt 0x80 3))
          , ("129", "9", Left ())
          , ("204", "4", Left ())
          , ("1001", "1", Left ())
          ]

      describe "One" $
        pit (fracInt8Dec () Minus (FracInt 1 1) 1)
          [ ("00000", "", Right (FracInt 1 1))
          , ("1/", "/", Right (FracInt 11 2))
          , ("05/", "/", Right (FracInt 105 3))
          , ("28:", ":", Right (FracInt 0x80 3))
          , ("29", "9", Left ())
          , ("001", "1", Left ())
          ]

      describe "One-Two" $
        pit (fracInt8Dec () Minus (FracInt 1 1) 2)
          [ ("0", "", Right (FracInt 1 1))
          , ("1", "", Right (FracInt 101 3))
          ]

  describe "fracInt16Dec" $ do
    describe "Plus" $ do
      describe "Zero" $
        pit (fracInt16Dec () Plus (FracInt 0 0) 0)
          [ ("00000", "", Right (FracInt 0 0))
          , ("1/", "/", Right (FracInt 1 1))
          , ("78", "", Right (FracInt 78 2))
          , ("2004", "", Right (FracInt 2004 4))
          , ("32767:", ":", Right (FracInt 0x7FFF 5))
          , ("32768", "8", Left ())
          , ("32801", "1", Left ())
          , ("100001", "1", Left ())
          ]

      describe "Three" $
        pit (fracInt16Dec () Plus (FracInt 327 3) 3)
          [ ("00000", "", Right (FracInt 327 3))
          , ("1/", "/", Right (FracInt 3271 4))
          , ("01", "", Right (FracInt 32701 5))
          , ("67:", ":", Right (FracInt 0x7FFF 5))
          , ("68", "8", Left ())
          , ("001", "1", Left ())
          ]

      describe "Two-Three" $
        pit (fracInt16Dec () Plus (FracInt 32 2) 3)
          [ ("00", "", Right (FracInt 32 2))
          , ("10", "", Right (FracInt 3201 4))
          , ("01", "", Right (FracInt 32001 5))
          ]

    describe "Minus" $ do
      describe "Zero" $
        pit (fracInt16Dec () Minus (FracInt 0 0) 0)
          [ ("00000", "", Right (FracInt 0 0))
          , ("1/", "/", Right (FracInt 1 1))
          , ("78", "", Right (FracInt 78 2))
          , ("2004", "", Right (FracInt 2004 4))
          , ("32768:", ":", Right (FracInt 0x8000 5))
          , ("32769", "9", Left ())
          , ("32801", "1", Left ())
          , ("100001", "1", Left ())
          ]

      describe "Three" $
        pit (fracInt16Dec () Minus (FracInt 327 3) 3)
          [ ("00000", "", Right (FracInt 327 3))
          , ("1/", "/", Right (FracInt 3271 4))
          , ("04", "", Right (FracInt 32704 5))
          , ("68:", ":", Right (FracInt 0x8000 5))
          , ("69", "9", Left ())
          , ("001", "1", Left ())
          ]

      describe "Two-Three" $
        pit (fracInt16Dec () Minus (FracInt 32 2) 3)
          [ ("00", "", Right (FracInt 32 2))
          , ("10", "", Right (FracInt 3201 4))
          , ("01", "", Right (FracInt 32001 5))
          ]

  describe "fracInt32Dec" $ do
    describe "Plus" $ do
      describe "Zero" $
        pit (fracInt32Dec () Plus (FracInt 0 0) 0)
          [ ("00000", "", Right (FracInt 0 0))
          , ("1/", "/", Right (FracInt 1 1))
          , ("78", "", Right (FracInt 78 2))
          , ("20030004", "", Right (FracInt 20030004 8))
          , ("2147483647:", ":", Right (FracInt 0x7FFFFFFF 10))
          , ("2147483648", "8", Left ())
          , ("2147490001", "1", Left ())
          , ("10000000001", "1", Left ())
          ]

      describe "Five" $
        pit (fracInt32Dec () Plus (FracInt 21474 5) 5)
          [ ("00000", "", Right (FracInt 21474 5))
          , ("1/", "/", Right (FracInt 214741 6))
          , ("001", "", Right (FracInt 21474001 8))
          , ("83647:", ":", Right (FracInt 0x7FFFFFFF 10))
          , ("83648", "8", Left ())
          , ("83701", "1", Left ())
          , ("000001", "1", Left ())
          ]

      describe "Three-Five" $
        pit (fracInt32Dec () Plus (FracInt 214 3) 5)
          [ ("000", "", Right (FracInt 214 3))
          , ("100", "", Right (FracInt 214001 6))
          , ("001", "", Right (FracInt 21400001 8))
          ]

    describe "Minus" $ do
      describe "Zero" $
        pit (fracInt32Dec () Minus (FracInt 0 0) 0)
          [ ("00000", "", Right (FracInt 0 0))
          , ("1/", "/", Right (FracInt 1 1))
          , ("78", "", Right (FracInt 78 2))
          , ("20030004", "", Right (FracInt 20030004 8))
          , ("2147483648:", ":", Right (FracInt 0x80000000 10))
          , ("2147483649", "9", Left ())
          , ("2147490001", "1", Left ())
          , ("10000000001", "1", Left ())
          ]

      describe "Five" $
        pit (fracInt32Dec () Minus (FracInt 21474 5) 5)
          [ ("00000", "", Right (FracInt 21474 5))
          , ("1/", "/", Right (FracInt 214741 6))
          , ("001", "", Right (FracInt 21474001 8))
          , ("83648:", ":", Right (FracInt 0x80000000 10))
          , ("83649", "9", Left ())
          , ("83701", "1", Left ())
          , ("000001", "1", Left ())
          ]

      describe "Three-Five" $
        pit (fracInt32Dec () Minus (FracInt 214 3) 5)
          [ ("000", "", Right (FracInt 214 3))
          , ("100", "", Right (FracInt 214001 6))
          , ("001", "", Right (FracInt 21400001 8))
          ]

  describe "fracInt64Dec" $ do
    describe "Plus" $ do
      describe "Zero" $
        pit (fracInt64Dec () Plus (FracInt 0 0) 0)
          [ ("00000", "", Right (FracInt 0 0))
          , ("1/", "/", Right (FracInt 1 1))
          , ("78", "", Right (FracInt 78 2))
          , ("2003000400005", "", Right (FracInt 2003000400005 13))
          , ("9223372036854775807:", ":", Right (FracInt 0x7FFFFFFFFFFFFFFF 19))
          , ("9223372036854775808", "8", Left ())
          , ("9223372037000000001", "1", Left ())
          , ("10000000000000000001", "1", Left ())
          ]

      describe "Ten" $
        pit (fracInt64Dec () Plus (FracInt 9223372036 10) 10)
          [ ("00000", "", Right (FracInt 9223372036 10))
          , ("1/", "/", Right (FracInt 92233720361 11))
          , ("0001", "", Right (FracInt 92233720360001 14))
          , ("854775807:", ":", Right (FracInt 0x7FFFFFFFFFFFFFFF 19))
          , ("854775808", "8", Left ())
          , ("854780001", "1", Left ())
          , ("0000000001", "1", Left ())
          ]

      describe "Five-Ten" $
        pit (fracInt64Dec () Plus (FracInt 92233 5) 10)
          [ ("0000", "", Right (FracInt 92233 5))
          , ("1000", "", Right (FracInt 92233000001 11))
          , ("0001", "", Right (FracInt 92233000000001 14))
          ]

    describe "Minus" $ do
      describe "Zero" $
        pit (fracInt64Dec () Minus (FracInt 0 0) 0)
          [ ("00000", "", Right (FracInt 0 0))
          , ("1/", "/", Right (FracInt 1 1))
          , ("78", "", Right (FracInt 78 2))
          , ("2003000400005", "", Right (FracInt 2003000400005 13))
          , ("9223372036854775808:", ":", Right (FracInt 0x8000000000000000 19))
          , ("9223372036854775809", "9", Left ())
          , ("9223372037000000001", "1", Left ())
          , ("10000000000000000001", "1", Left ())
          ]

      describe "Ten" $
        pit (fracInt64Dec () Minus (FracInt 9223372036 10) 10)
          [ ("00000", "", Right (FracInt 9223372036 10))
          , ("1/", "/", Right (FracInt 92233720361 11))
          , ("0001", "", Right (FracInt 92233720360001 14))
          , ("854775808:", ":", Right (FracInt 0x8000000000000000 19))
          , ("854775809", "9", Left ())
          , ("854780001", "1", Left ())
          , ("0000000001", "1", Left ())
          ]

      describe "Five-Ten" $
        pit (fracInt64Dec () Minus (FracInt 92233 5) 10)
          [ ("0000", "", Right (FracInt 92233 5))
          , ("1000", "", Right (FracInt 92233000001 11))
          , ("0001", "", Right (FracInt 92233000000001 14))
          ]

  describe "fracIntDec" $ do
    describe "Plus" $ do
      describe "Zero" $
        pit (fracIntDec () Plus (FracInt 0 0) 0)
          [ ("00000", "", Right (FracInt 0 0))
          , ("1/", "/", Right (FracInt 1 1))
          , ("78", "", Right (FracInt 78 2))
          , ("203004", "", Right (FracInt 203004 6))
          ]

      describe "Three" $
        pit (fracIntDec () Plus (FracInt 123 3) 3)
          [ ("00000", "", Right (FracInt 123 3))
          , ("1/", "/", Right (FracInt 1231 4))
          , ("0001", "", Right (FracInt 1230001 7))
          ]

      describe "Two-three" $
        pit (fracIntDec () Plus (FracInt 12 2) 3)
          [ ("0000", "", Right (FracInt 12 2))
          , ("1000", "", Right (FracInt 1201 4))
          , ("0001", "", Right (FracInt 1200001 7))
          ]

    describe "Minus" $ do
      describe "Zero" $
        pit (fracIntDec () Minus (FracInt 0 0) 0)
          [ ("00000", "", Right (FracInt 0 0))
          , ("1/", "/", Right (FracInt 1 1))
          , ("78", "", Right (FracInt 78 2))
          , ("203004", "", Right (FracInt 203004 6))
          ]

      describe "Three" $
        pit (fracIntDec () Minus (FracInt 123 3) 3)
          [ ("00000", "", Right (FracInt 123 3))
          , ("1/", "/", Right (FracInt 1231 4))
          , ("0001", "", Right (FracInt 1230001 7))
          ]

      describe "Two-three" $
        pit (fracIntDec () Minus (FracInt 12 2) 3)
          [ ("0000", "", Right (FracInt 12 2))
          , ("1000", "", Right (FracInt 1201 4))
          , ("0001", "", Right (FracInt 1200001 7))
          ]


  describe "fracFloat23Dec" $ do
    describe "Zero" $
      pit @_ @() (fracFloat23Dec (FracFloat 0 0))
        [ ("00000", "", Right (FracFloat 0 5))
        , ("1", "", Right (FracFloat 1 1))
        , ("78", "", Right (FracFloat 78 2))
        , ("2030040", "", Right (FracFloat 2030040 7))
        , ("123400056789", "", Right (FracFloat 123400056 9))
        ]

    describe "Five" $
      pit @_ @() (fracFloat23Dec (FracFloat 54321 5))
        [ ("00000", "", Right (FracFloat 543210000 9))
        , ("1", "", Right (FracFloat 543211 6))
        , ("78", "", Right (FracFloat 5432178 7))
        , ("2030040", "", Right (FracFloat 543212030 9))
        , ("123400056789", "", Right (FracFloat 543211234 9))
        ]

  describe "fracFloat52Dec" $ do
    describe "Zero" $
      pit @_ @() (fracFloat52Dec (FracFloat 0 0))
        [ ("00000", "", Right (FracFloat 0 5))
        , ("1", "", Right (FracFloat 1 1))
        , ("78", "", Right (FracFloat 78 2))
        , ("2030040", "", Right (FracFloat 2030040 7))
        , ("123400567800009876000054321", "", Right (FracFloat 12340056780000987 17))
        ]

    describe "Ten" $
      pit @_ @() (fracFloat52Dec (FracFloat 5432198765 10))
        [ ("00000", "", Right (FracFloat 543219876500000 15))
        , ("1", "", Right (FracFloat 54321987651 11))
        , ("78", "", Right (FracFloat 543219876578 12))
        , ("20300400050", "", Right (FracFloat 54321987652030040 17))
        , ("123400567800009876000054321", "", Right (FracFloat 54321987651234005 17))
        ]
