{-# LANGUAGE OverloadedStrings
           , StandaloneDeriving #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Lathe.Encoding.UTF8
  ( utf8
  ) where

import           Parser.Lathe
import           Parser.Lathe.Encoding.UTF8 as O

import qualified Data.ByteString as B
import           Data.ByteString.Builder (Builder, toLazyByteString)
import qualified Data.ByteString.Builder as I
import           Data.Foldable
import           Test.Hspec



data UTF8Error = UTF8Error_1 UTF8Error_1
               | UTF8Incomplete_2_2
               | UTF8Error_3 UTF8Error_3
               | UTF8Error_4 UTF8Error_4
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



charUtf8 :: Parser (E1 UTF8Error) (Char, Bool)
charUtf8 = do
  u <- O.unitUtf8 (Malformed1 . UTF8Error_1) EoF1
  case u of
    UTF8_1 u1 -> pure (fromUtf8 u1, False)
    UTF8_2 u2 -> do
      r <- O.contUtf8_2 (Malformed1 UTF8Incomplete_2_2) EoF1 u2
      pure (r, False)

    UTF8_3 u3 -> do
      r <- O.contUtf8_3 (Malformed1 . UTF8Error_3) EoF1 u3
      pure (fromUtf8 r, isSurrogate r)

    UTF8_4 u4 -> do
      r <- O.contUtf8_4 (Malformed1 . UTF8Error_4) EoF1 u4
      pure (r, False)


skipUtf8 :: Parser (E1 UTF8Error) Int
skipUtf8 = do
  u <- O.unitUtf8 (Malformed1 . UTF8Error_1) EoF1
  case u of
    UTF8_1 _  -> pure 1
    UTF8_2 u2 -> 2 <$ O.skipUtf8_2 (Malformed1 UTF8Incomplete_2_2) EoF1 u2
    UTF8_3 u3 -> do
      r <- O.skipUtf8_3 (Malformed1 . UTF8Error_3) EoF1 u3
      pure $ if r
               then 5
               else 3

    UTF8_4 u4 -> 4 <$ O.skipUtf8_4 (Malformed1 . UTF8Error_4) EoF1 u4



utf8 :: Spec
utf8 = do
  describe "utf8BOM" $
    pit (\_ -> "") (O.utf8BOM Malformed0 EoF0)
      [ (Left "\xEF\xBB\xBF", Right ())
      , (Left "\xAF\xBB\xBF", Left Malformed0)
      ]

  describe "charUtf8" $
    pit I.charUtf8 charUtf8
      [ (Left "\x00"            , Right ('\x00'    , False))
      , (Right '1'              , Right ('1'       , False))
      , (Right 'N'              , Right ('N'       , False))
      , (Right 'c'              , Right ('c'       , False))
      , (Left "\x7F"            , Right ('\x7F'    , False))
      , (Left "\xC2\x80"        , Right ('\x80'    , False))
      , (Right '£'              , Right ('£'       , False))
      , (Right 'ߋ'              , Right ('ߋ'       , False))
      , (Right 'λ'              , Right ('λ'       , False))
      , (Left "\xDF\xBF"        , Right ('\x07FF'  , False))
      , (Left "\xE0\xA0\x80"    , Right ('\x0800'  , False))
      , (Right 'ჹ'              , Right ('ჹ'       , False))
      , (Right 'גּ'              , Right ('גּ'       , False))
      , (Right '�'              , Right ('�'       , False))
      , (Left "\xED\x9F\xBF"    , Right ('\xD7FF'  , False))
      , (Left "\xED\xA0\x80"    , Right ('\xD800'  , True))
      , (Left "\xED\xBF\xBF"    , Right ('\xDFFF'  , True))
      , (Left "\xEE\x80\x80"    , Right ('\xE000'  , False))
      , (Left "\xEF\xBF\xBF"    , Right ('\xFFFF'  , False))
      , (Left "\xF0\x90\x80\x80", Right ('\x10000' , False))
      , (Right '𝜆'              , Right ('𝜆'       , False))
      , (Right '🁊'              , Right ('🁊'       , False))
      , (Left "\xF4\x89\x9A\xB3", Right ('\x1096B3', False))
      , (Left "\xF4\x8F\xBF\xBF", Right ('\x10FFFF', False))

      , (Left "\x80"            , Left . Malformed1 $ UTF8Error_1 UTF8Continuation)
      , (Left "\xBF"            , Left . Malformed1 $ UTF8Error_1 UTF8Continuation)
      , (Left "\xF8"            , Left . Malformed1 $ UTF8Error_1 UTF8Invalid)
      , (Left "\xC0"            , Left . Malformed1 $ UTF8Error_1 UTF8Overlong_2)
      , (Left "\xC1"            , Left . Malformed1 $ UTF8Error_1 UTF8Overlong_2)
      , (Left "\xE0\x80\x00"    , Left . Malformed1 $ UTF8Error_3 UTF8Overlong_3)
      , (Left "\xE0\x9F\x00"    , Left . Malformed1 $ UTF8Error_3 UTF8Overlong_3)
      , (Left "\xF0\x80\x00\x00", Left . Malformed1 $ UTF8Error_4 UTF8Overlong_4)
      , (Left "\xF0\x8F\x00\x00", Left . Malformed1 $ UTF8Error_4 UTF8Overlong_4)

      , (Left "\xC2\x00"        , Left $ Malformed1 UTF8Incomplete_2_2)
      , (Left "\xDF\xFF"        , Left $ Malformed1 UTF8Incomplete_2_2)
      , (Left "\xE0\x40\x00"    , Left . Malformed1 $ UTF8Error_3 UTF8Incomplete_2_3)
      , (Left "\xEF\xFF\x00"    , Left . Malformed1 $ UTF8Error_3 UTF8Incomplete_2_3)
      , (Left "\xF0\x00\x00\x00", Left . Malformed1 $ UTF8Error_4 UTF8Incomplete_2_4)
      , (Left "\xF4\xFF\x00\x00", Left . Malformed1 $ UTF8Error_4 UTF8Incomplete_2_4)
      , (Left "\xE0\xA0\x00"    , Left . Malformed1 $ UTF8Error_3 UTF8Incomplete_3_3)
      , (Left "\xEF\xBF\xFF"    , Left . Malformed1 $ UTF8Error_3 UTF8Incomplete_3_3)
      , (Left "\xF0\x90\x00\x00", Left . Malformed1 $ UTF8Error_4 UTF8Incomplete_3_4)
      , (Left "\xF4\x8F\xFF\x00", Left . Malformed1 $ UTF8Error_4 UTF8Incomplete_3_4)
      , (Left "\xF0\x90\x80\x00", Left . Malformed1 $ UTF8Error_4 UTF8Incomplete_4_4)
      , (Left "\xF4\x8F\xBF\xFF", Left . Malformed1 $ UTF8Error_4 UTF8Incomplete_4_4)

      , (Left "\xF5"            , Left . Malformed1 $ UTF8Error_1 UTF8Overflow_1)
      , (Left "\xF4\x90\x00\x00", Left . Malformed1 $ UTF8Error_4 UTF8Overflow_2)
      ]

  describe "skipUtf8" $
    pit I.charUtf8 skipUtf8
      [ (Left "\x00"            , Right 1)
      , (Right '1'              , Right 1)
      , (Right 'N'              , Right 1)
      , (Right 'c'              , Right 1)
      , (Left "\x7F"            , Right 1)
      , (Left "\xC2\x80"        , Right 2)
      , (Right '£'              , Right 2)
      , (Right 'ߋ'              , Right 2)
      , (Right 'λ'              , Right 2)
      , (Left "\xDF\xBF"        , Right 2)
      , (Left "\xE0\xA0\x80"    , Right 3)
      , (Right 'ჹ'              , Right 3)
      , (Right 'גּ'              , Right 3)
      , (Right '�'              , Right 3)
      , (Left "\xED\x9F\xBF"    , Right 3)
      , (Left "\xED\xA0\x80"    , Right 5)
      , (Left "\xED\xBF\xBF"    , Right 5)
      , (Left "\xEE\x80\x80"    , Right 3)
      , (Left "\xEF\xBF\xBF"    , Right 3)
      , (Left "\xF0\x90\x80\x80", Right 4)
      , (Right '𝜆'              , Right 4)
      , (Right '🁊'              , Right 4)
      , (Left "\xF4\x89\x9A\xB3", Right 4)
      , (Left "\xF4\x8F\xBF\xBF", Right 4)

      , (Left "\x80"            , Left . Malformed1 $ UTF8Error_1 UTF8Continuation)
      , (Left "\xBF"            , Left . Malformed1 $ UTF8Error_1 UTF8Continuation)
      , (Left "\xF8"            , Left . Malformed1 $ UTF8Error_1 UTF8Invalid)
      , (Left "\xC0"            , Left . Malformed1 $ UTF8Error_1 UTF8Overlong_2)
      , (Left "\xC1"            , Left . Malformed1 $ UTF8Error_1 UTF8Overlong_2)
      , (Left "\xE0\x80\x00"    , Left . Malformed1 $ UTF8Error_3 UTF8Overlong_3)
      , (Left "\xE0\x9F\x00"    , Left . Malformed1 $ UTF8Error_3 UTF8Overlong_3)
      , (Left "\xF0\x80\x00\x00", Left . Malformed1 $ UTF8Error_4 UTF8Overlong_4)
      , (Left "\xF0\x8F\x00\x00", Left . Malformed1 $ UTF8Error_4 UTF8Overlong_4)

      , (Left "\xC2\x00"        , Left $ Malformed1 UTF8Incomplete_2_2)
      , (Left "\xDF\xFF"        , Left $ Malformed1 UTF8Incomplete_2_2)
      , (Left "\xE0\x40\x00"    , Left . Malformed1 $ UTF8Error_3 UTF8Incomplete_2_3)
      , (Left "\xEF\xFF\x00"    , Left . Malformed1 $ UTF8Error_3 UTF8Incomplete_2_3)
      , (Left "\xF0\x00\x00\x00", Left . Malformed1 $ UTF8Error_4 UTF8Incomplete_2_4)
      , (Left "\xF4\xFF\x00\x00", Left . Malformed1 $ UTF8Error_4 UTF8Incomplete_2_4)
      , (Left "\xE0\xA0\x00"    , Left . Malformed1 $ UTF8Error_3 UTF8Incomplete_3_3)
      , (Left "\xEF\xBF\xFF"    , Left . Malformed1 $ UTF8Error_3 UTF8Incomplete_3_3)
      , (Left "\xF0\x90\x00\x00", Left . Malformed1 $ UTF8Error_4 UTF8Incomplete_3_4)
      , (Left "\xF4\x8F\xFF\x00", Left . Malformed1 $ UTF8Error_4 UTF8Incomplete_3_4)
      , (Left "\xF0\x90\x80\x00", Left . Malformed1 $ UTF8Error_4 UTF8Incomplete_4_4)
      , (Left "\xF4\x8F\xBF\xFF", Left . Malformed1 $ UTF8Error_4 UTF8Incomplete_4_4)

      , (Left "\xF5"            , Left . Malformed1 $ UTF8Error_1 UTF8Overflow_1)
      , (Left "\xF4\x90\x00\x00", Left . Malformed1 $ UTF8Error_4 UTF8Overflow_2)
      ]
