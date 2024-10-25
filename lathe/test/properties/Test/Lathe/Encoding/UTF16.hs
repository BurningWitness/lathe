{-# LANGUAGE OverloadedStrings
           , StandaloneDeriving #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Lathe.Encoding.UTF16
  ( utf16
  ) where

import           Parser.Lathe
import           Parser.Lathe.Encoding.UTF16 as O

import           Data.Bits
import qualified Data.ByteString as B
import           Data.ByteString.Builder (Builder, toLazyByteString)
import qualified Data.ByteString.Builder as I
import           Data.Foldable
import           Data.Word
import           GHC.ByteOrder
import           Test.Hspec



data UTF16Error = UTF16Error_1
                | UTF16Error_2
                  deriving (Show, Eq)



pit_
  :: (Eq a, Eq e, Show a, Show e)
  => Parser e a -> Builder -> Either e a -> Expectation
pit_ tear build res =
  let (Scrap _ remaining more, ei) = parse tear (toLazyByteString build)
  in (remaining, more, ei) `shouldBe` ("", End, res)



pit
  :: (Eq a, Eq e, Show a, Show e)
  => (Char -> Builder) -> Parser e a -> [(Either B.ByteString Char, Either e a)] -> Spec
pit build tear brs =
  for_ brs $ \(raw, res) ->
    it (either (show . B.unpack) (:[]) raw) $
      pit_ tear (either I.byteString build raw) res



data E0 = Malformed0
        | EoF0
          deriving (Show, Eq)

data E1 a = Malformed1 a
          | EoF1
            deriving (Show, Eq)



utf16Pair :: Word32 -> (Word16, Word16)
utf16Pair w =
  let w' = w - 0x10000
  in ( 0xD800 .|. (fromIntegral $ unsafeShiftR w' 10)
     , 0xDC00 .|. (fromIntegral w' Data.Bits..&. 0x3FF)
     )

buildUtf16BE :: Char -> Builder
buildUtf16BE c =
  let i = fromEnum c
  in if i < 0x10000
       then I.word16BE (fromIntegral i)
       else let (hi, lo) = utf16Pair (fromIntegral i)
            in I.word16BE hi <> I.word16BE lo

buildUtf16LE :: Char -> Builder
buildUtf16LE c =
  let i = fromEnum c
  in if i < 0x10000
       then I.word16LE (fromIntegral i)
       else let (hi, lo) = utf16Pair (fromIntegral i)
            in I.word16LE hi <> I.word16LE lo


charUtf16BE :: Parser (E1 UTF16Error) Char
charUtf16BE = do
  u <- O.unitUtf16BE (Malformed1 UTF16Error_1) EoF1
  case u of
    UTF16_1 u1 -> pure $! fromUtf16 u1
    UTF16_2 u2 -> O.contUtf16BE_2 (Malformed1 UTF16Error_2) EoF1 u2

charUtf16LE :: Parser (E1 UTF16Error) Char
charUtf16LE = do
  u <- O.unitUtf16LE (Malformed1 UTF16Error_1) EoF1
  case u of
    UTF16_1 u1 -> pure $! fromUtf16 u1
    UTF16_2 u2 -> O.contUtf16LE_2 (Malformed1 UTF16Error_2) EoF1 u2


skipUtf16BE :: Parser (E1 UTF16Error) Int
skipUtf16BE = do
  u <- O.unitUtf16BE (Malformed1 UTF16Error_1) EoF1
  case u of
    UTF16_1 _  -> pure 2
    UTF16_2 u2 -> 4 <$ O.skipUtf16BE_2 (Malformed1 UTF16Error_2) EoF1 u2

skipUtf16LE :: Parser (E1 UTF16Error) Int
skipUtf16LE = do
  u <- O.unitUtf16LE (Malformed1 UTF16Error_1) EoF1
  case u of
    UTF16_1 _  -> pure 2
    UTF16_2 u2 -> 4 <$ O.skipUtf16LE_2 (Malformed1 UTF16Error_2) EoF1 u2



utf16 :: Spec
utf16 = do
  describe "utf16BOM" $
    pit (\_ -> "") (O.utf16BOM Malformed0 EoF0)
      [ (Left "\xFE\xFF", Right BigEndian)
      , (Left "\xFF\xFE", Right LittleEndian)
      , (Left "\xAF\xFF", Left Malformed0)
      ]

  describe "charUtf16BE" $
    pit buildUtf16BE charUtf16BE
      [ (Left "\x00\x00"        , Right '\x00')
      , (Right '1'              , Right '1')
      , (Right 'λ'              , Right 'λ')
      , (Left "\xD7\xFF"        , Right '\xD7FF')
      , (Left "\xE0\x00"        , Right '\xE000')
      , (Right '�'              , Right '�')
      , (Left "\xFF\xFF"        , Right '\xFFFF')
      , (Left "\xD8\x00\xDC\x00", Right '\x10000')
      , (Right '𝜆'              , Right '𝜆')
      , (Right '🁊'              , Right '🁊')
      , (Left "\xDB\xE5\xDE\xB3", Right '\x1096B3')
      , (Left "\xDB\xFF\xDF\xFF", Right '\x10FFFF')

      , (Left "\xDC\x00"        , Left $ Malformed1 UTF16Error_1)
      , (Left "\xDF\xFF"        , Left $ Malformed1 UTF16Error_1)
      , (Left "\xD8\x00\x00\x00", Left $ Malformed1 UTF16Error_2)
      , (Left "\xDB\xFF\xFF\xFF", Left $ Malformed1 UTF16Error_2)
      ]

  describe "skipUtf16BE" $
    pit buildUtf16BE skipUtf16BE
      [ (Left "\x00\x00"        , Right 2)
      , (Right '1'              , Right 2)
      , (Right 'λ'              , Right 2)
      , (Left "\xD7\xFF"        , Right 2)
      , (Left "\xE0\x00"        , Right 2)
      , (Right '�'              , Right 2)
      , (Left "\xFF\xFF"        , Right 2)
      , (Left "\xD8\x00\xDC\x00", Right 4)
      , (Right '𝜆'              , Right 4)
      , (Right '🁊'              , Right 4)
      , (Left "\xDB\xE5\xDE\xB3", Right 4)
      , (Left "\xDB\xFF\xDF\xFF", Right 4)

      , (Left "\xDC\x00"        , Left $ Malformed1 UTF16Error_1)
      , (Left "\xDF\xFF"        , Left $ Malformed1 UTF16Error_1)
      , (Left "\xD8\x00\x00\x00", Left $ Malformed1 UTF16Error_2)
      , (Left "\xDB\xFF\xFF\xFF", Left $ Malformed1 UTF16Error_2)
      ]

  describe "charUtf16LE" $
    pit buildUtf16LE charUtf16LE
      [ (Left "\x00\x00"        , Right '\x00')
      , (Right '1'              , Right '1')
      , (Right 'λ'              , Right 'λ')
      , (Left "\xFF\xD7"        , Right '\xD7FF')
      , (Left "\x00\xE0"        , Right '\xE000')
      , (Right '�'              , Right '�')
      , (Left "\xFF\xFF"        , Right '\xFFFF')
      , (Left "\x00\xD8\x00\xDC", Right '\x10000')
      , (Right '𝜆'              , Right '𝜆')
      , (Right '🁊'              , Right '🁊')
      , (Left "\xE5\xDB\xB3\xDE", Right '\x1096B3')
      , (Left "\xFF\xDB\xFF\xDF", Right '\x10FFFF')

      , (Left "\x00\xDC"        , Left $ Malformed1 UTF16Error_1)
      , (Left "\xFF\xDF"        , Left $ Malformed1 UTF16Error_1)
      , (Left "\x00\xD8\x00\x00", Left $ Malformed1 UTF16Error_2)
      , (Left "\xFF\xDB\xFF\xFF", Left $ Malformed1 UTF16Error_2)
      ]

  describe "skipUtf16LE" $
    pit buildUtf16LE skipUtf16LE
      [ (Left "\x00\x00"        , Right 2)
      , (Right '1'              , Right 2)
      , (Right 'λ'              , Right 2)
      , (Left "\xFF\xD7"        , Right 2)
      , (Left "\x00\xE0"        , Right 2)
      , (Right '�'              , Right 2)
      , (Left "\xFF\xFF"        , Right 2)
      , (Left "\x00\xD8\x00\xDC", Right 4)
      , (Right '𝜆'              , Right 4)
      , (Right '🁊'              , Right 4)
      , (Left "\xE5\xDB\xB3\xDE", Right 4)
      , (Left "\xFF\xDB\xFF\xDF", Right 4)

      , (Left "\x00\xDC"        , Left $ Malformed1 UTF16Error_1)
      , (Left "\xFF\xDF"        , Left $ Malformed1 UTF16Error_1)
      , (Left "\x00\xD8\x00\x00", Left $ Malformed1 UTF16Error_2)
      , (Left "\xFF\xDB\xFF\xFF", Left $ Malformed1 UTF16Error_2)
      ]
