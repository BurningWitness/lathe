module Test.Lathe.Radix
  ( radix
  ) where

import           Parser.Lathe.Radix

import           Control.Applicative
import           Data.Word
import           Test.Hspec



brute :: (Word8 -> Maybe Word8) -> (Word8 -> Maybe Word8) -> Expectation
brute f g = fmap f [0..255] `shouldBe` fmap g [0..255]



refBin :: Word8 -> Maybe Word8
refBin w =
  case toEnum $ fromIntegral w of
    '0' -> Just 0
    '1' -> Just 1
    _   -> Nothing

refOct :: Word8 -> Maybe Word8
refOct w =
  case toEnum $ fromIntegral w of
    '0' -> Just 0
    '1' -> Just 1
    '2' -> Just 2
    '3' -> Just 3
    '4' -> Just 4
    '5' -> Just 5
    '6' -> Just 6
    '7' -> Just 7
    _   -> Nothing

refDec :: Word8 -> Maybe Word8
refDec w =
  case toEnum $ fromIntegral w of
    '0' -> Just 0
    '1' -> Just 1
    '2' -> Just 2
    '3' -> Just 3
    '4' -> Just 4
    '5' -> Just 5
    '6' -> Just 6
    '7' -> Just 7
    '8' -> Just 8
    '9' -> Just 9
    _   -> Nothing



refHexUpperDigits :: Word8 -> Maybe Word8
refHexUpperDigits w = 
  case toEnum $ fromIntegral w of
    'A' -> Just 10
    'B' -> Just 11
    'C' -> Just 12
    'D' -> Just 13
    'E' -> Just 14
    'F' -> Just 15
    _   -> Nothing

refHexLowerDigits :: Word8 -> Maybe Word8
refHexLowerDigits w = 
  case toEnum $ fromIntegral w of
    'a' -> Just 10
    'b' -> Just 11
    'c' -> Just 12
    'd' -> Just 13
    'e' -> Just 14
    'f' -> Just 15
    _   -> Nothing

refHex :: Word8 -> Maybe Word8
refHex w = refDec w <|> refHexLowerDigits w <|> refHexUpperDigits w



radix :: Spec
radix = do
  it "bin"      $ brute bin      refBin
  it "oct"      $ brute oct      refOct
  it "dec"      $ brute dec      refDec
  it "hex"      $ brute hex      refHex
