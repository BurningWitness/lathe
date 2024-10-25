{-# LANGUAGE OverloadedStrings
           , StandaloneDeriving #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Lathe.Encoding.UTF32
  ( utf32
  ) where

import           Parser.Lathe
import           Parser.Lathe.Encoding.UTF32 as O

import qualified Data.ByteString as B
import           Data.ByteString.Builder (Builder, toLazyByteString)
import qualified Data.ByteString.Builder as I
import           Data.Foldable
import           GHC.ByteOrder
import           Test.Hspec



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



buildUtf32BE :: Char -> Builder
buildUtf32BE c = I.int32BE (fromIntegral $ fromEnum c)

buildUtf32LE :: Char -> Builder
buildUtf32LE c = I.int32LE (fromIntegral $ fromEnum c)



utf32 :: Spec
utf32 = do
  describe "utf32BOM" $
    pit (\_ -> "") (O.utf32BOM Malformed0 EoF0)
      [ (Left "\x00\x00\xFE\xFF", Right BigEndian)
      , (Left "\xFF\xFE\x00\x00", Right LittleEndian)
      , (Left "\xAF\xFF\x00\x00", Left Malformed0)
      ]

  describe "charUtf32BE" $
    pit buildUtf32BE ( do r <- O.unitUtf32BE Malformed0 EoF0
                          pure (fromUtf32 r, isSurrogate r)
                     )
      [ (Left "\x00\x00\x00\x00", Right ('\x00'    , False))
      , (Right '1'              , Right ('1'       , False))
      , (Right 'λ'              , Right ('λ'       , False))
      , (Left "\x00\x00\xD7\xFF", Right ('\xD7FF'  , False))
      , (Left "\x00\x00\xD8\x00", Right ('\xD800'  , True))
      , (Left "\x00\x00\xDB\xFF", Right ('\xDBFF'  , True))
      , (Left "\x00\x00\xDC\x00", Right ('\xDC00'  , True))
      , (Left "\x00\x00\xDF\xFF", Right ('\xDFFF'  , True))
      , (Left "\x00\x00\xE0\x00", Right ('\xE000'  , False))
      , (Right '�'              , Right ('�'       , False))
      , (Right '𝜆'              , Right ('𝜆'       , False))
      , (Left "\x00\x10\xFF\xFF", Right ('\x10FFFF', False))

      , (Left "\x00\x11\x00\x00", Left Malformed0)
      , (Left "\xFF\xFF\xFF\xFF", Left Malformed0)
      ]

  describe "charUtf32LE" $
    pit buildUtf32LE ( do r <- O.unitUtf32LE Malformed0 EoF0
                          pure (fromUtf32 r, isSurrogate r)
                     )
      [ (Left "\x00\x00\x00\x00", Right ('\x00'    , False))
      , (Right '1'              , Right ('1'       , False))
      , (Right 'λ'              , Right ('λ'       , False))
      , (Left "\xFF\xD7\x00\x00", Right ('\xD7FF'  , False))
      , (Left "\x00\xD8\x00\x00", Right ('\xD800'  , True))
      , (Left "\xFF\xDB\x00\x00", Right ('\xDBFF'  , True))
      , (Left "\x00\xDC\x00\x00", Right ('\xDC00'  , True))
      , (Left "\xFF\xDF\x00\x00", Right ('\xDFFF'  , True))
      , (Left "\x00\xE0\x00\x00", Right ('\xE000'  , False))
      , (Right '�'              , Right ('�'       , False))
      , (Right '𝜆'              , Right ('𝜆'       , False))
      , (Left "\xFF\xFF\x10\x00", Right ('\x10FFFF', False))

      , (Left "\x00\x00\x11\x00", Left Malformed0)
      , (Left "\xFF\xFF\xFF\xFF", Left Malformed0)
      ]
