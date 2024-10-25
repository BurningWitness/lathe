{-# LANGUAGE OverloadedStrings
           , StandaloneDeriving #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Lathe.Numeric.Integral
  ( integral
  ) where

import           Parser.Lathe
import           Parser.Lathe.Numeric.Integral

import           Data.ByteString.Builder as I
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as L
import           Data.Foldable
import           Numeric.Natural
import           System.Random
import           Test.Hspec
import           Text.Read



deriving instance Eq a => Eq (WholeInt a)



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



check
  :: (Eq a, Show a)
  => Parser () a -> L.ByteString -> (L.ByteString, More, Either () a) -> Expectation
check tear build out =
  let (Scrap _ rest more, ei) = parse tear build
  in (rest, more, ei) `shouldBe` out



integral :: Spec
integral = do
  describe "wholeToInt8" $ do
    it "Plus"  $ wholeToInt8 Plus  (WholeInt 73) `shouldBe` 73
    it "Minus" $ wholeToInt8 Minus (WholeInt 73) `shouldBe` -73

  describe "wholeToInt16" $ do
    it "Plus"  $ wholeToInt16 Plus  (WholeInt 7364) `shouldBe` 7364
    it "Minus" $ wholeToInt16 Minus (WholeInt 7364) `shouldBe` -7364

  describe "wholeToInt32" $ do
    it "Plus"  $ wholeToInt32 Plus  (WholeInt 736451) `shouldBe` 736451
    it "Minus" $ wholeToInt32 Minus (WholeInt 736451) `shouldBe` -736451

  describe "wholeToInt64" $ do
    it "Plus"  $ wholeToInt64 Plus  (WholeInt 73645180) `shouldBe` 73645180
    it "Minus" $ wholeToInt64 Minus (WholeInt 73645180) `shouldBe` -73645180

  describe "wholeToInt" $ do
    it "Plus"  $ wholeToInt Plus  (WholeInt 736423) `shouldBe` 736423
    it "Minus" $ wholeToInt Minus (WholeInt 736423) `shouldBe` -736423

  describe "wholeWord8Dec" $ do
    describe "Zero" $
      pit (wholeWord8Dec () 0 0)
        [ ("000", "", Right (0, 3))
        , ("1/", "/", Right (1, 1))
        , ("123", "", Right (123, 3))
        , ("255:", ":", Right (0xFF, 3))
        , ("256", "6", Left ())
        , ("260", "0", Left ())
        , ("300", "0", Left ())
        ]

    describe "One" $
      pit (wholeWord8Dec () 2 1)
        [ ("00", "", Right (200, 3))
        , ("1/", "/", Right (21, 2))
        , ("55:", ":", Right (0xFF, 3))
        , ("56", "6", Left ())
        , ("60", "0", Left ())
        ]

  describe "wholeWord16Dec" $ do
    describe "Zero" $
      pit (wholeWord16Dec () 0 0)
        [ ("00000", "", Right (0, 5))
        , ("1/", "/", Right (1, 1))
        , ("1234", "", Right (1234, 4))
        , ("65535:", ":", Right (0xFFFF, 5))
        , ("65536", "6", Left ())
        , ("65600", "0", Left ())
        , ("70000", "0", Left ())
        ]

    describe "Three" $
      pit (wholeWord16Dec () 655 3)
        [ ("00", "", Right (65500, 5))
        , ("1/", "/", Right (6551, 4))
        , ("35:", ":", Right (0xFFFF, 5))
        , ("36", "6", Left ())
        , ("40", "0", Left ())
        ]

  describe "wholeWord32Dec" $ do
    describe "Zero" $
      pit (wholeWord32Dec () 0 0)
        [ ("0000000000", "", Right (0, 10))
        , ("1/", "/", Right (1, 1))
        , ("12345678", "", Right (12345678, 8))
        , ("4294967295:", ":", Right (0xFFFFFFFF, 10))
        , ("4294967296", "6", Left ())
        , ("4294970000", "0", Left ())
        , ("5000000000", "0", Left ())
        ]

    describe "Five" $
      pit (wholeWord32Dec () 42949 5)
        [ ("00000", "", Right (4294900000, 10))
        , ("1/", "/", Right (429491, 6))
        , ("67295:", ":", Right (0xFFFFFFFF, 10))
        , ("67296", "6", Left ())
        , ("67300", "0", Left ())
        , ("70000", "0", Left ())
        ]

  describe "wholeWord64Dec" $ do
    describe "Zero" $
      pit (wholeWord64Dec () 0 0)
        [ ("00000000000000000000", "", Right (0, 20))
        , ("1/", "/", Right (1, 1))
        , ("123456789012345", "", Right (123456789012345, 15))
        , ("18446744073709551615:", ":", Right (0xFFFFFFFFFFFFFFFF, 20))
        , ("18446744073709551616", "6", Left ())
        , ("18446744080000000000", "0", Left ())
        , ("20000000000000000000", "0", Left ())
        ]

    describe "Ten" $
      pit (wholeWord64Dec () 1844674407 10)
        [ ("0000000000", "", Right (18446744070000000000, 20))
        , ("1/", "/", Right (18446744071, 11))
        , ("3709551615:", ":", Right (0xFFFFFFFFFFFFFFFF, 20))
        , ("3709551616", "6", Left ())
        , ("3709560000", "0", Left ())
        , ("4000000000", "0", Left ())
        ]

  describe "wholeWordDec" $ do
    describe "Zero" $
      pit (wholeWordDec () 0 0)
        [ ("000", "", Right (0, 3))
        , ("1/", "/", Right (1, 1))
        , ("12345", "", Right (12345, 5))
        , ("9876543:", ":", Right (9876543, 7))
        ]

    describe "Three" $
      pit (wholeWordDec () 543 3)
        [ ("000", "", Right (543000, 6))
        , ("1/", "/", Right (5431, 4))
        , ("1234:", ":", Right (5431234, 7))
        ]


  describe "wholeNaturalDec" $ do
    it "None" $
      check (wholeNaturalDec 0) "" ("", End, Right 0)
  
    it "Miss" $
      check (wholeNaturalDec 12345) " " (" ", End, Right 12345)
  
    it "Zeroes" $
      check (wholeNaturalDec 3456789012345) "000000000000/"
        ("/", End, Right 3456789012345000000000000)
  
    it "Ones" $
      check (wholeNaturalDec 8765432109876543210) "111111111111111,"
        (",", End, Right 8765432109876543210111111111111111)
  
    it "Nines" $
      check (wholeNaturalDec 1000000000000000000000000000000) "99999999999999999999:"
        (":", End, Right 100000000000000000000000000000099999999999999999999)
  
    it "Big" $
      let ~(i, _) = uniformR (10 ^ (10000 :: Int), 10 ^ (20000 :: Int)) (mkStdGen 0)
          ~(j, _) = uniformR (10 ^ (10000 :: Int), 10 ^ (20000 :: Int)) (mkStdGen 1)
  
          r = integerDec (fromIntegral (i :: Natural))
          o = integerDec (fromIntegral (j :: Natural))
  
      in case readEither . L.unpack . toLazyByteString $ r <> o of
           Left _  -> errorWithoutStackTrace "unparseable reference"
           Right q ->
             check (wholeNaturalDec i) (toLazyByteString $ o <> ",")
               (",", End, Right q)


  describe "wholeInt8Dec" $ do
    describe "Plus" $ do
      describe "Zero" $
        pit (wholeInt8Dec () Plus (WholeInt 0) 0)
          [ ("000", "", Right (WholeInt 0, 3))
          , ("1/", "/", Right (WholeInt 1, 1))
          , ("78", "", Right (WholeInt 78, 2))
          , ("127:", ":", Right (WholeInt 0x7F, 3))
          , ("128", "8", Left ())
          , ("130", "0", Left ())
          , ("200", "0", Left ())
          ]

      describe "One" $
        pit (wholeInt8Dec () Plus (WholeInt 1) 1)
          [ ("00", "", Right (WholeInt 100, 3))
          , ("1/", "/", Right (WholeInt 11, 2))
          , ("27:", ":", Right (WholeInt 0x7F, 3))
          , ("28", "8", Left ())
          , ("30", "0", Left ())
          ]

    describe "Minus" $ do
      describe "Zero" $
        pit (wholeInt8Dec () Minus (WholeInt 0) 0)
          [ ("000", "", Right (WholeInt 0, 3))
          , ("1/", "/", Right (WholeInt 1, 1))
          , ("78", "", Right (WholeInt 78, 2))
          , ("128:", ":", Right (WholeInt 0x80, 3))
          , ("129", "9", Left ())
          , ("130", "0", Left ())
          , ("200", "0", Left ())
          ]

      describe "One" $
        pit (wholeInt8Dec () Minus (WholeInt 1) 1)
          [ ("00", "", Right (WholeInt 100, 3))
          , ("1/", "/", Right (WholeInt 11, 2))
          , ("28:", ":", Right (WholeInt 0x80, 3))
          , ("29", "9", Left ())
          , ("30", "0", Left ())
          ]

  describe "wholeInt16Dec" $ do
    describe "Plus" $ do
      describe "Zero" $
        pit (wholeInt16Dec () Plus (WholeInt 0) 0)
          [ ("00000", "", Right (WholeInt 0, 5))
          , ("1/", "/", Right (WholeInt 1, 1))
          , ("1234", "", Right (WholeInt 1234, 4))
          , ("32767:", ":", Right (WholeInt 0x7FFF, 5))
          , ("32768", "8", Left ())
          , ("32800", "0", Left ())
          , ("40000", "0", Left ())
          ]

      describe "Three" $
        pit (wholeInt16Dec () Plus (WholeInt 327) 3)
          [ ("00", "", Right (WholeInt 32700, 5))
          , ("1/", "/", Right (WholeInt 3271, 4))
          , ("67:", ":", Right (WholeInt 0x7FFF, 5))
          , ("68", "8", Left ())
          , ("70", "0", Left ())
          ]

    describe "Minus" $ do
      describe "Zero" $
        pit (wholeInt16Dec () Minus (WholeInt 0) 0)
          [ ("00000", "", Right (WholeInt 0, 5))
          , ("1/", "/", Right (WholeInt 1, 1))
          , ("1234", "", Right (WholeInt 1234, 4))
          , ("32768:", ":", Right (WholeInt 0x8000, 5))
          , ("32769", "9", Left ())
          , ("32800", "0", Left ())
          , ("40000", "0", Left ())
          ]

      describe "Three" $
        pit (wholeInt16Dec () Minus (WholeInt 327) 3)
          [ ("00", "", Right (WholeInt 32700, 5))
          , ("1/", "/", Right (WholeInt 3271, 4))
          , ("68:", ":", Right (WholeInt 0x8000, 5))
          , ("69", "9", Left ())
          , ("70", "0", Left ())
          ]

  describe "wholeInt32Dec" $ do
    describe "Plus" $ do
      describe "Zero" $
        pit (wholeInt32Dec () Plus (WholeInt 0) 0)
          [ ("0000000000", "", Right (WholeInt 0, 10))
          , ("1/", "/", Right (WholeInt 1, 1))
          , ("12345678", "", Right (WholeInt 12345678, 8))
          , ("2147483647:", ":", Right (WholeInt 0x7FFFFFFF, 10))
          , ("2147483648", "8", Left ())
          , ("2147490000", "0", Left ())
          , ("3000000000", "0", Left ())
          ]
  
      describe "Five" $
        pit (wholeInt32Dec () Plus (WholeInt 21474) 5)
          [ ("00000", "", Right (WholeInt 2147400000, 10))
          , ("1/", "/", Right (WholeInt 214741, 6))
          , ("83647:", ":", Right (WholeInt 0x7FFFFFFF, 10))
          , ("83648", "8", Left ())
          , ("83700", "0", Left ())
          , ("90000", "0", Left ())
          ]

    describe "Minus" $ do
      describe "Zero" $
        pit (wholeInt32Dec () Minus (WholeInt 0) 0)
          [ ("0000000000", "", Right (WholeInt 0, 10))
          , ("1/", "/", Right (WholeInt 1, 1))
          , ("12345678", "", Right (WholeInt 12345678, 8))
          , ("2147483648:", ":", Right (WholeInt 0x80000000, 10))
          , ("2147483649", "9", Left ())
          , ("2147490000", "0", Left ())
          , ("3000000000", "0", Left ())
          ]
  
      describe "Five" $
        pit (wholeInt32Dec () Minus (WholeInt 21474) 5)
          [ ("00000", "", Right (WholeInt 2147400000, 10))
          , ("1/", "/", Right (WholeInt 214741, 6))
          , ("83648:", ":", Right (WholeInt 0x80000000, 10))
          , ("83649", "9", Left ())
          , ("83700", "0", Left ())
          , ("90000", "0", Left ())
          ]

  describe "wholeInt64Dec" $ do
    describe "Plus" $ do
      describe "Zero" $
        pit (wholeInt64Dec () Plus (WholeInt 0) 0)
          [ ("0000000000000000000", "", Right (WholeInt 0, 19))
          , ("1/", "/", Right (WholeInt 1, 1))
          , ("123456789012345", "", Right (WholeInt 123456789012345, 15))
          , ("9223372036854775807:", ":", Right (WholeInt 0x7FFFFFFFFFFFFFFF, 19))
          , ("9223372036854775808", "8", Left ())
          , ("9223372037000000000", "0", Left ())
          , ("10000000000000000000", "0", Left ())
          ]
  
      describe "Ten" $
        pit (wholeInt64Dec () Plus (WholeInt 9223372036) 10)
          [ ("000000000", "", Right (WholeInt 9223372036000000000, 19))
          , ("1/", "/", Right (WholeInt 92233720361, 11))
          , ("854775807:", ":", Right (WholeInt 0x7FFFFFFFFFFFFFFF, 19))
          , ("854775808", "8", Left ())
          , ("856780000", "0", Left ())
          , ("900000000", "0", Left ())
          ]

    describe "Minus" $ do
      describe "Zero" $
        pit (wholeInt64Dec () Minus (WholeInt 0) 0)
          [ ("0000000000000000000", "", Right (WholeInt 0, 19))
          , ("1/", "/", Right (WholeInt 1, 1))
          , ("123456789012345", "", Right (WholeInt 123456789012345, 15))
          , ("9223372036854775808:", ":", Right (WholeInt 0x8000000000000000, 19))
          , ("9223372036854775809", "9", Left ())
          , ("9223372037000000000", "0", Left ())
          , ("10000000000000000000", "0", Left ())
          ]
  
      describe "Ten" $
        pit (wholeInt64Dec () Minus (WholeInt 9223372036) 10)
          [ ("000000000", "", Right (WholeInt 9223372036000000000, 19))
          , ("1/", "/", Right (WholeInt 92233720361, 11))
          , ("854775808:", ":", Right (WholeInt 0x8000000000000000, 19))
          , ("854775809", "9", Left ())
          , ("856780000", "0", Left ())
          , ("900000000", "0", Left ())
          ]

  describe "wholeIntDec" $ do
    describe "Plus" $ do
      describe "Zero" $
        pit (wholeIntDec () Plus (WholeInt 0) 0)
          [ ("000", "", Right (WholeInt 0, 3))
          , ("1/", "/", Right (WholeInt 1, 1))
          , ("12345", "", Right (WholeInt 12345, 5))
          , ("9876543:", ":", Right (WholeInt 9876543, 7))
          ]

      describe "Three" $
        pit (wholeIntDec () Plus (WholeInt 543) 3)
          [ ("000", "", Right (WholeInt 543000, 6))
          , ("1/", "/", Right (WholeInt 5431, 4))
          , ("1234:", ":", Right (WholeInt 5431234, 7))
          ]

    describe "Minus" $ do
      describe "Zero" $
        pit (wholeIntDec () Minus (WholeInt 0) 0)
          [ ("000", "", Right (WholeInt 0, 3))
          , ("1/", "/", Right (WholeInt 1, 1))
          , ("12345", "", Right (WholeInt 12345, 5))
          , ("9876543:", ":", Right (WholeInt 9876543, 7))
          ]

      describe "Three" $
        pit (wholeIntDec () Minus (WholeInt 543) 3)
          [ ("000", "", Right (WholeInt 543000, 6))
          , ("1/", "/", Right (WholeInt 5431, 4))
          , ("1234:", ":", Right (WholeInt 5431234, 7))
          ]

  describe "wholeWord8Hex" $ do
    describe "Zero" $
      pit (wholeWord8Hex () 0 0)
        [ ("00", "", Right (0x0, 2))
        , ("1/", "/", Right (0x1, 1))
        , ("7B", "", Right (0x7B, 2))
        , ("FF:", ":", Right (0xFF, 2))
        , ("FF0", "0", Left ())
        ]

    describe "One" $
      pit (wholeWord8Hex () 0xF 1)
        [ ("0", "", Right (0xF0, 2))
        , ("1/", "/", Right (0xF1, 2))
        , ("F:", ":", Right (0xFF, 2))
        , ("F0", "0", Left ())
        ]

  describe "wholeWord16Hex" $ do
    describe "Zero" $
      pit (wholeWord16Hex () 0 0)
        [ ("0000", "", Right (0, 4))
        , ("1/", "/", Right (1, 1))
        , ("52B", "", Right (0x52B, 3))
        , ("FFFF:", ":", Right (0xFFFF, 4))
        , ("FFFF0", "0", Left ())
        ]

    describe "Two" $
      pit (wholeWord16Hex () 0xFF 2)
        [ ("00", "", Right (0xFF00, 4))
        , ("1/", "/", Right (0xFF1, 3))
        , ("FF:", ":", Right (0xFFFF, 4))
        , ("FF0", "0", Left ())
        ]

  describe "wholeWord32Hex" $ do
    describe "Zero" $
      pit (wholeWord32Hex () 0 0)
        [ ("00000000", "", Right (0, 8))
        , ("1/", "/", Right (1, 1))
        , ("789ABC", "", Right (0x789ABC, 6))
        , ("FFFFFFFF:", ":", Right (0xFFFFFFFF, 8))
        , ("FFFFFFFF0", "0", Left ())
        ]

    describe "Four" $
      pit (wholeWord32Hex () 0xFFFF 4)
        [ ("0000", "", Right (0xFFFF0000, 8))
        , ("1/", "/", Right (0xFFFF1, 5))
        , ("FFFF:", ":", Right (0xFFFFFFFF, 8))
        , ("FFFF0", "0", Left ())
        ]

  describe "wholeWord64Hex" $ do
    describe "Zero" $
      pit (wholeWord64Hex () 0 0)
        [ ("0000000000000000", "", Right (0, 16))
        , ("1/", "/", Right (1, 1))
        , ("456789ABCDEF", "", Right (0x456789ABCDEF, 12))
        , ("FFFFFFFFFFFFFFFF:", ":", Right (0xFFFFFFFFFFFFFFFF, 16))
        , ("FFFFFFFFFFFFFFFF0", "0", Left ())
        ]

    describe "Eight" $
      pit (wholeWord64Hex () 0xFFFFFFFF 8)
        [ ("00000000", "", Right (0xFFFFFFFF00000000, 16))
        , ("1/", "/", Right (0xFFFFFFFF1, 9))
        , ("FFFFFFFF:", ":", Right (0xFFFFFFFFFFFFFFFF, 16))
        , ("FFFFFFFF0", "0", Left ())
        ]

  describe "wholeWordHex" $ do
    describe "Zero" $
      pit (wholeWordHex () 0 0)
        [ ("000", "", Right (0, 3))
        , ("1/", "/", Right (1, 1))
        , ("123AB", "", Right (0x123AB, 5))
        , ("9F8E7D:", ":", Right (0x9F8E7D, 6))
        ]

    describe "Three" $
      pit (wholeWordHex () 0x1A2 3)
        [ ("000", "", Right (0x1A2000, 6))
        , ("1/", "/", Right (0x1A21, 4))
        , ("B3C:", ":", Right (0x1A2B3C, 6))
        ]
