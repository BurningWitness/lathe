{-# LANGUAGE BangPatterns
           , OverloadedStrings
           , TypeApplications
           , UnboxedTuples #-}

module Test.Lathe where

import           Parser.Lathe
import           Parser.Lathe.Binary (word8, int8)
import           Parser.Lathe.Unsafe

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import           Test.Hspec



feed
  :: [B.ByteString] -> [B.ByteString] -> Parser e a
  -> (ByteOffset, [B.ByteString], More, Either e a)
feed this that parser =
  let (bs, lbs) = case this of
                    []   -> ("", "")
                    t:hs -> (t, L.fromChunks hs)

      more = case that of
               [] -> End
               _  -> More

  in go that (draw parser $ prepare 0 bs lbs more)
  where
    go xs f =
      case f of
        Partial re        -> case xs of
                               []   -> go [] $ re EndOfInput
                               x:ys -> go ys $ re (Supply x)

        Done (blank', res) ->
          let Scrap off lbs more = scrap blank'
          in (off, L.toChunks lbs, more, res)



data E = E
         deriving (Show, Eq)



core :: Spec
core = do
  describe "bytesRead" $ do
    it "empty" $ do
      feed [] []
        (bytesRead @E)
          `shouldBe` (0, [], End, Right 0)

    it "single" $ do
      feed ["abcdef"] []
        ( do skipEndOr 3
             bytesRead @E
        )
          `shouldBe` (3, ["def"], End, Right 3)

    it "border" $ do
      feed ["abc", "def", "ghi"] ["jkl"]
        ( do skipEndOr 3
             bytesRead @E
        )
          `shouldBe` (3, ["def", "ghi"], More, Right 3)


  describe "atEnd" $ do
    it "empty" $ do
      feed [] []
        (atEnd @E)
          `shouldBe` (0, [], End, Right True)

    it "single" $ do
      feed ["abcdef"] []
        ( do skipEndOr 3
             atEnd @E
        )
          `shouldBe` (3, ["def"], End, Right False)

    describe "border" $ do
      it "plain" $ do
        feed ["abc", "def", "ghi"] []
          ( do skipEndOr 3
               atEnd @E
          )
            `shouldBe` (3, ["def", "ghi"], End, Right False)

      it "feed" $ do
        feed ["abc"] ["def", "ghi"]
          ( do skipEndOr 3
               atEnd @E
          )
            `shouldBe` (3, ["def"], More, Right False)

      it "stagger" $ do
        feed ["abc"] []
          ( do skipEndOr 3
               atEnd @E
          )
            `shouldBe` (3, [], End, Right True)


  describe "mapError" $ do
    it "short" $
      feed ["abcdefghi", "jkl"] []
        ( do skipEndOr 2
             (cs, ()) <- match @E $ skipEndOr 5
             pure $ L.toChunks cs
        )
          `shouldBe` (7, ["hi", "jkl"], End, Right ["cdefg"])

    describe "long" $ do
      it "known" $ do
        feed ["abc", "def", "ghi"] []
          ( do skipEndOr 2
               (cs, ()) <- match @E $ skipEndOr 5
               pure $ L.toChunks cs
          )
            `shouldBe` (7, ["hi"], End, Right ["c", "def", "g"])

      it "future" $ do
        feed ["abc"] ["def", "ghi", "jkl"]
          ( do skipEndOr 2
               (cs, ()) <- match @E $ skipEndOr 5
               pure $ L.toChunks cs
          )
            `shouldBe` (7, ["hi"], More, Right ["c", "def", "g"])

      it "wall" $ do
        feed ["abc"] ["def", "ghi", "jkl"]
          ( do skipEndOr 2
               (cs, ()) <- match @E $ skip 30 E
               pure $ L.toChunks cs
          )
            `shouldBe` (12, [], End, Left E)


  describe "catch" $ do
    it "short" $
      feed ["abcdefghi", "jkl"] []
        ( do skipEndOr 2
             skipEndOr 5 `catch` \_ -> err E
        )
         `shouldBe` (7, ["hi", "jkl"], End, Right ())

    describe "long" $ do
      it "known" $ do
        feed ["abc", "def", "ghi"] []
          ( do skipEndOr 2
               skipEndOr 5 `catch` \_ -> err E
          )
            `shouldBe` (7, ["hi"], End, Right ())

      it "future" $ do
        feed ["abc"] ["def", "ghi", "jkl"]
          ( do skipEndOr 2
               skipEndOr 5 `catch` \_ -> err E
          )
            `shouldBe` (7, ["hi"], More, Right ())

      it "wall" $ do
        feed ["abc"] ["def", "ghi", "jkl"]
          ( do skipEndOr 2
               skip 30 () `catch` \_ -> err E
          )
            `shouldBe` (2, ["c", "def", "ghi", "jkl"], End, Left E)

    describe "rollback" $ do
      it "known" $
        feed ["abc", "def", "ghi"] []
          ( do skipEndOr 2
               ( do skipEndOr 5
                    err ()
                )
                 `catch` \_ -> skipEndOr @E 2
          )

            `shouldBe` (4, ["ef", "ghi"], End, Right ())

      it "future" $ do
        feed ["abc"] ["def", "ghi", "jkl"]
          ( do skipEndOr 2
               ( do skipEndOr 5
                    err ()
                )
                 `catch` \_ -> skipEndOr @E 2
          )
            `shouldBe` (4, ["ef", "ghi"], More, Right ())

      it "wall" $ do
        feed ["abc"] ["def", "ghi", "jkl"]
          ( do skipEndOr 2
               skip 30 ()
                 `catch` \_ -> skipEndOr @E 2
          )
            `shouldBe` (4, ["ef", "ghi", "jkl"], End, Right ())

    describe "double rollback" $ do
      it "known" $
        feed ["abc", "def", "ghi"] []
          ( do skipEndOr 2
               ( do skipEndOr 3
                    ( do skipEndOr 2
                         err ()
                     )
                      `catch` \_ -> err ()
                )
                 `catch` \_ -> skipEndOr @E 2
          )
            `shouldBe` (4, ["ef", "ghi"], End, Right ())

      it "future" $ do
        feed ["abc"] ["def", "ghi", "jkl"]
          ( do skipEndOr 2
               ( do skipEndOr 3
                    ( do skipEndOr 2
                         err ()
                     )
                      `catch` \_ -> err ()
                )
                 `catch` \_ -> skipEndOr @E 2
          )
            `shouldBe` (4, ["ef", "ghi"], More, Right ())


  describe "match" $ do
    it "short" $
      feed ["abcdefghi", "jkl"] []
        ( do skipEndOr 2
             (cs, ()) <- match @E $ skipEndOr 5
             pure $ L.toChunks cs
        )
          `shouldBe` (7, ["hi", "jkl"], End, Right ["cdefg"])

    describe "long" $ do
      it "known" $ do
        feed ["abc", "def", "ghi"] []
          ( do skipEndOr 2
               (cs, ()) <- match @E $ skipEndOr 5
               pure $ L.toChunks cs
          )
            `shouldBe` (7, ["hi"], End, Right ["c", "def", "g"])

      it "future" $ do
        feed ["abc"] ["def", "ghi", "jkl"]
          ( do skipEndOr 2
               (cs, ()) <- match @E $ skipEndOr 5
               pure $ L.toChunks cs
          )
            `shouldBe` (7, ["hi"], More, Right ["c", "def", "g"])

      it "wall" $ do
        feed ["abc"] ["def", "ghi", "jkl"]
          ( do skipEndOr 2
               (cs, ()) <- match @E $ skip 30 E
               pure $ L.toChunks cs
          )
            `shouldBe` (12, [], End, Left E)

    describe "rollback" $ do
      it "plain" $
        feed ["abcdefghi", "jkl"] ["nop"]
          ( do skipEndOr 2
               ( do _ <- match $ skipEndOr 3
                    err ()
                )
                 `catch` \_ -> do (cs, _) <- match @E $ skipEndOr @E 2
                                  pure $ L.toChunks cs
          )
            `shouldBe` (4, ["efghi", "jkl"], More, Right ["cd"])

      it "known" $
        feed ["abc", "def", "ghi", "jkl"] ["nop"]
          ( do skipEndOr 2
               ( do _ <- match $ skipEndOr 5
                    err ()
                )
                 `catch` \_ -> do (cs, _) <- match @E $ skipEndOr @E 5
                                  pure $ L.toChunks cs
          )
            `shouldBe` (7, ["hi", "jkl"], More, Right ["c", "def", "g"])

      it "future" $ do
        feed ["abc"] ["def", "ghi", "jkl", "nop"]
          ( do skipEndOr 2
               ( do _ <- match $ skipEndOr 8
                    err ()
                )
                 `catch` \_ -> do (cs, _) <- match @E $ skipEndOr @E 5
                                  pure $ L.toChunks cs
          )
            `shouldBe` (7, ["hi", "jkl"], More, Right ["c", "def", "g"])

      it "wall" $ do
        feed ["abc"] ["def", "ghi", "jkl"]
          ( do skipEndOr 2
               ( do _ <- match $ skip 30 ()
                    pure undefined
                )
                 `catch` \_ -> do (cs, _) <- match @E $ skipEndOr @E 2
                                  pure $ L.toChunks cs
          )
            `shouldBe` (4, ["ef", "ghi", "jkl"], End, Right ["c", "d"])


  describe "word8" $ do
    it "short" $ do
      feed ["abcdef"] []
       ( do skipEndOr 2
            word8 E
       )
         `shouldBe` (3, ["def"], End, Right 0x63)

    describe "border" $ do
      it "known" $ do
        feed ["abc", "def"] ["ghi"]
          ( do skipEndOr 2
               word8 E
          )
           `shouldBe` (3, ["def"], More, Right 0x63)

      it "future" $ do
        feed ["abc"] ["def", "ghi"]
          ( do skipEndOr 2
               word8 E
          )
           `shouldBe` (3, [], More, Right 0x63)

    it "wall" $ do
      feed ["ab"] []
        ( do skipEndOr 2
             word8 E
        )
         `shouldBe` (2, [], End, Left E)


  describe "int8" $ do
    it "short" $ do
      feed ["abcdef"] []
       ( do skipEndOr 2
            int8 E
       )
         `shouldBe` (3, ["def"], End, Right 0x63)

    describe "border" $ do
      it "known" $ do
        feed ["abc", "def"] ["ghi"]
          ( do skipEndOr 2
               int8 E
          )
           `shouldBe` (3, ["def"], More, Right 0x63)

      it "future" $ do
        feed ["abc"] ["def", "ghi"]
          ( do skipEndOr 2
               int8 E
          )
           `shouldBe` (3, [], More, Right 0x63)

    it "wall" $ do
      feed ["ab"] []
        ( do skipEndOr 2
             int8 E
        )
         `shouldBe` (2, [], End, Left E)


  describe "skip1" $ do
    it "short" $ do
      feed ["abcdef"] []
       ( do skipEndOr 2
            skip1 E
       )
         `shouldBe` (3, ["def"], End, Right ())

    describe "border" $ do
      it "known" $ do
        feed ["abc", "def"] ["ghi"]
          ( do skipEndOr 2
               skip1 E
          )
           `shouldBe` (3, ["def"], More, Right ())

      it "future" $ do
        feed ["abc"] ["def", "ghi"]
          ( do skipEndOr 2
               skip1 E
          )
           `shouldBe` (3, [], More, Right ())

    it "wall" $ do
      feed ["ab"] []
        ( do skipEndOr 2
             skip1 E
        )
         `shouldBe` (2, [], End, Left E)


  describe "skipEndOr1" $ do
    it "short" $ do
      feed ["abcdef"] []
       ( do skipEndOr 2
            skipEndOr1 @E
       )
         `shouldBe` (3, ["def"], End, Right ())

    describe "border" $ do
      it "known" $ do
        feed ["abc", "def"] ["ghi"]
          ( do skipEndOr 2
               skipEndOr1 @E
          )
           `shouldBe` (3, ["def"], More, Right ())

      it "future" $ do
        feed ["abc"] ["def", "ghi"]
          ( do skipEndOr 2
               skipEndOr1 @E
          )
           `shouldBe` (3, [], More, Right ())

    it "wall" $ do
      feed ["ab"] []
        ( do skipEndOr 2
             skipEndOr1 @E
        )
         `shouldBe` (2, [], End, Right ())


  describe "skip" $ do
    it "empty" $ do
      feed [] []
        (skip (-1) E)
          `shouldBe` (0, [], End, Right ())

    it "short" $ do
      feed ["abcdefghi"] ["jkl"]
        ( do skipEndOr 2
             skip 5 E
        )
          `shouldBe` (7, ["hi"], More, Right ())

    it "known" $ do
      feed ["abc", "def", "ghi"] ["jkl"]
        ( do skipEndOr 2
             skip 5 E
        )
          `shouldBe` (7, ["hi"], More, Right ())

    it "future" $ do
      feed ["abc"] ["def", "ghi", "jkl"]
        ( do skipEndOr 2
             skip 5 E
        )
          `shouldBe` (7, ["hi"], More, Right ())

    it "wall" $ do
      feed ["abc"] ["def", "ghi"]
        ( do skipEndOr 2
             skip 30 E
        )
          `shouldBe` (9, [], End, Left E)


  describe "skipEndOr" $ do
    it "empty" $ do
      feed [] []
        (skipEndOr @E (-1))
          `shouldBe` (0, [], End, Right ())

    it "short" $ do
      feed ["abcdefghi"] ["jkl"]
        ( do skipEndOr 2
             skipEndOr @E 5
        )
          `shouldBe` (7, ["hi"], More, Right ())

    it "known" $ do
      feed ["abc", "def", "ghi"] ["jkl"]
        ( do skipEndOr 2
             skipEndOr @E 5
        )
          `shouldBe` (7, ["hi"], More, Right ())

    it "future" $ do
      feed ["abc"] ["def", "ghi", "jkl"]
        ( do skipEndOr 2
             skipEndOr @E 5
        )
          `shouldBe` (7, ["hi"], More, Right ())

    it "wall" $ do
      feed ["abc"] ["def", "ghi"]
        ( do skipEndOr 2
             skipEndOr @E 30
        )
          `shouldBe` (9, [], End, Right ())


  describe "skipNul" $ do
    it "short" $ do
      feed ["abcdefg\0i"] ["jkl"]
        ( do skipEndOr 2
             skipNul E
        )
          `shouldBe` (8, ["i"], More, Right ())

    it "known" $ do
      feed ["abc", "def", "g\0i"] ["jkl"]
        ( do skipEndOr 2
             skipNul E
        )
          `shouldBe` (8, ["i"], More, Right ())

    it "future" $ do
      feed ["abc"] ["def", "g\0i", "jkl"]
        ( do skipEndOr 2
             skipNul E
        )
          `shouldBe` (8, ["i"], More, Right ())

    it "wall" $ do
      feed ["abc"] ["def", "ghi"]
        ( do skipEndOr 2
             skipNul E
        )
          `shouldBe` (9, [], End, Left E)


  describe "skipUntil" $ do
    it "short" $ do
      feed ["abcdefghi"] ["jkl"]
        ( do skipEndOr 2
             skipUntil (== 0x68) E
        )
          `shouldBe` (7, ["hi"], More, Right ())

    it "known" $ do
      feed ["abc", "def", "ghi"] ["jkl"]
        ( do skipEndOr 2
             skipUntil (== 0x68) E
        )
          `shouldBe` (7, ["hi"], More, Right ())

    it "future" $ do
      feed ["abc"] ["def", "ghi", "jkl"]
        ( do skipEndOr 2
             skipUntil (== 0x68) E
        )
          `shouldBe` (7, ["hi"], More, Right ())

    it "wall" $ do
      feed ["abc"] ["def", "ghi"]
        ( do skipEndOr 2
             skipUntil (== 0x2D) E
        )
          `shouldBe` (9, [], End, Left E)


  describe "skipUntilEndOr" $ do
    it "short" $ do
      feed ["abcdefghi"] ["jkl"]
        ( do skipEndOr 2
             skipUntilEndOr @E (== 0x68)
        )
          `shouldBe` (7, ["hi"], More, Right ())

    it "known" $ do
      feed ["abc", "def", "ghi"] ["jkl"]
        ( do skipEndOr 2
             skipUntilEndOr @E (== 0x68)
        )
          `shouldBe` (7, ["hi"], More, Right ())

    it "future" $ do
      feed ["abc"] ["def", "ghi", "jkl"]
        ( do skipEndOr 2
             skipUntilEndOr @E (== 0x68)
        )
          `shouldBe` (7, ["hi"], More, Right ())

    it "wall" $ do
      feed ["abc"] ["def", "ghi"]
        ( do skipEndOr 2
             skipUntilEndOr @E (== 0x2D)
        )
          `shouldBe` (9, [], End, Right ())


  describe "unsafeRead" $ do
    it "empty" $ do
      feed [] []
        (unsafeRead 0 (\_ -> (# Yes () #)) E)
          `shouldBe` (0, [], End, Right ())

    it "short" $ do
      feed ["abcdefghi"] ["jkl"]
        ( do skipEndOr 2
             unsafeRead 5 (\_ -> (# Yes () #)) E
        )
          `shouldBe` (7, ["hi"], More, Right ())

    it "known" $ do
      feed ["abc", "def", "ghi"] ["jkl"]
        ( do skipEndOr 2
             unsafeRead 5 (\_ -> (# Yes () #)) E
        )
          `shouldBe` (7, ["hi"], More, Right ())

    it "future" $ do
      feed ["abc"] ["def", "ghi", "jkl"]
        ( do skipEndOr 2
             unsafeRead 5 (\_ -> (# Yes () #)) E
        )
          `shouldBe` (7, ["hi"], More, Right ())

    it "wall" $ do
      feed ["abc"] ["def", "ghi"]
        ( do skipEndOr 2
             unsafeRead 30 (\_ -> (# Yes () #)) E
        )
          `shouldBe` (9, [], End, Left E)


  describe "byteString" $ do
    it "empty" $ do
      feed [] []
        (byteString (-1) E)
          `shouldBe` (0, [], End, Right "")

    it "short" $ do
      feed ["abcdefghi"] ["jkl"]
        ( do skipEndOr 2
             byteString 5 E
        )
          `shouldBe` (7, ["hi"], More, Right "cdefg")

    it "known" $ do
      feed ["abc", "def", "ghi"] ["jkl"]
        ( do skipEndOr 2
             byteString 5 E
        )
          `shouldBe` (7, ["hi"], More, Right "cdefg")

    it "future" $ do
      feed ["abc"] ["def", "ghi", "jkl"]
        ( do skipEndOr 2
             byteString 5 E
        )
          `shouldBe` (7, ["hi"], More, Right "cdefg")

    it "wall" $ do
      feed ["abc"] ["def", "ghi"]
        ( do skipEndOr 2
             byteString 30 E
        )
          `shouldBe` (9, [], End, Left E)


  describe "byteStringNul" $ do
    it "short" $ do
      feed ["abcdefg\0i"] ["jkl"]
        ( do skipEndOr 2
             byteStringNul E
        )
          `shouldBe` (8, ["i"], More, Right "cdefg")

    it "known" $ do
      feed ["abc", "def", "g\0i"] ["jkl"]
        ( do skipEndOr 2
             byteStringNul E
        )
          `shouldBe` (8, ["i"], More, Right "cdefg")

    it "future" $ do
      feed ["abc"] ["def", "g\0i", "jkl"]
        ( do skipEndOr 2
             byteStringNul E
        )
          `shouldBe` (8, ["i"], More, Right "cdefg")

    it "wall" $ do
      feed ["abc"] ["def", "ghi"]
        ( do skipEndOr 2
             byteStringNul E
        )
          `shouldBe` (9, [], End, Left E)


  describe "byteStringUntil" $ do
    it "short" $ do
      feed ["abcdefghi"] ["jkl"]
        ( do skipEndOr 2
             byteStringUntil (== 0x68) E
        )
          `shouldBe` (7, ["hi"], More, Right "cdefg")

    it "known" $ do
      feed ["abc", "def", "ghi"] ["jkl"]
        ( do skipEndOr 2
             byteStringUntil (== 0x68) E
        )
          `shouldBe` (7, ["hi"], More, Right "cdefg")

    it "future" $ do
      feed ["abc"] ["def", "ghi", "jkl"]
        ( do skipEndOr 2
             byteStringUntil (== 0x68) E
        )
          `shouldBe` (7, ["hi"], More, Right "cdefg")

    it "wall" $ do
      feed ["abc"] ["def", "ghi"]
        ( do skipEndOr 2
             byteStringUntil (== 0x2D) E
        )
          `shouldBe` (9, [], End, Left E)


  describe "shortByteString" $ do
    it "empty" $ do
      feed [] []
        (shortByteString (-1) E)
          `shouldBe` (0, [], End, Right "")

    it "short" $ do
      feed ["abcdefghi"] ["jkl"]
        ( do skipEndOr 2
             shortByteString 5 E
        )
          `shouldBe` (7, ["hi"], More, Right "cdefg")

    it "known" $ do
      feed ["abc", "def", "ghi"] ["jkl"]
        ( do skipEndOr 2
             shortByteString 5 E
        )
          `shouldBe` (7, ["hi"], More, Right "cdefg")

    it "future" $ do
      feed ["abc"] ["def", "ghi", "jkl"]
        ( do skipEndOr 2
             shortByteString 5 E
        )
          `shouldBe` (7, ["hi"], More, Right "cdefg")

    it "wall" $ do
      feed ["abc"] ["def", "ghi"]
        ( do skipEndOr 2
             shortByteString 30 E
        )
          `shouldBe` (9, [], End, Left E)


  describe "shortByteStringNul" $ do
    it "short" $ do
      feed ["abcdefg\0i"] ["jkl"]
        ( do skipEndOr 2
             shortByteStringNul E
        )
          `shouldBe` (8, ["i"], More, Right "cdefg")

    it "known" $ do
      feed ["abc", "def", "g\0i"] ["jkl"]
        ( do skipEndOr 2
             shortByteStringNul E
        )
          `shouldBe` (8, ["i"], More, Right "cdefg")

    it "future" $ do
      feed ["abc"] ["def", "g\0i", "jkl"]
        ( do skipEndOr 2
             shortByteStringNul E
        )
          `shouldBe` (8, ["i"], More, Right "cdefg")

    it "wall" $ do
      feed ["abc"] ["def", "ghi"]
        ( do skipEndOr 2
             shortByteStringNul E
        )
          `shouldBe` (9, [], End, Left E)


  describe "shortByteStringUntil" $ do
    it "short" $ do
      feed ["abcdefghi"] ["jkl"]
        ( do skipEndOr 2
             shortByteStringUntil (== 0x68) E
        )
          `shouldBe` (7, ["hi"], More, Right "cdefg")

    it "known" $ do
      feed ["abc", "def", "ghi"] ["jkl"]
        ( do skipEndOr 2
             shortByteStringUntil (== 0x68) E
        )
          `shouldBe` (7, ["hi"], More, Right "cdefg")

    it "future" $ do
      feed ["abc"] ["def", "ghi", "jkl"]
        ( do skipEndOr 2
             shortByteStringUntil (== 0x68) E
        )
          `shouldBe` (7, ["hi"], More, Right "cdefg")

    it "wall" $ do
      feed ["abc"] ["def", "ghi"]
        ( do skipEndOr 2
             shortByteStringUntil (== 0x2D) E
        )
          `shouldBe` (9, [], End, Left E)


  describe "lazyByteString" $ do
    it "empty" $ do
      feed [] []
        (lazyByteString (-1) E)
          `shouldBe` (0, [], End, Right "")

    it "short" $ do
      feed ["abcdefghi"] ["jkl"]
        ( do skipEndOr 2
             lazyByteString 5 E
        )
          `shouldBe` (7, ["hi"], More, Right "cdefg")

    it "known" $ do
      feed ["abc", "def", "ghi"] ["jkl"]
        ( do skipEndOr 2
             lazyByteString 5 E
        )
          `shouldBe` (7, ["hi"], More, Right "cdefg")

    it "future" $ do
      feed ["abc"] ["def", "ghi", "jkl"]
        ( do skipEndOr 2
             lazyByteString 5 E
        )
          `shouldBe` (7, ["hi"], More, Right "cdefg")

    it "wall" $ do
      feed ["abc"] ["def", "ghi"]
        ( do skipEndOr 2
             lazyByteString 30 E
        )
          `shouldBe` (9, [], End, Left E)


  describe "lazyByteStringNul" $ do
    it "short" $ do
      feed ["abcdefg\0i"] ["jkl"]
        ( do skipEndOr 2
             lazyByteStringNul E
        )
          `shouldBe` (8, ["i"], More, Right "cdefg")

    it "known" $ do
      feed ["abc", "def", "g\0i"] ["jkl"]
        ( do skipEndOr 2
             lazyByteStringNul E
        )
          `shouldBe` (8, ["i"], More, Right "cdefg")

    it "future" $ do
      feed ["abc"] ["def", "g\0i", "jkl"]
        ( do skipEndOr 2
             lazyByteStringNul E
        )
          `shouldBe` (8, ["i"], More, Right "cdefg")

    it "wall" $ do
      feed ["abc"] ["def", "ghi"]
        ( do skipEndOr 2
             lazyByteStringNul E
        )
          `shouldBe` (9, [], End, Left E)


  describe "lazyByteStringUntil" $ do
    it "short" $ do
      feed ["abcdefghi"] ["jkl"]
        ( do skipEndOr 2
             lazyByteStringUntil (== 0x68) E
        )
          `shouldBe` (7, ["hi"], More, Right "cdefg")

    it "known" $ do
      feed ["abc", "def", "ghi"] ["jkl"]
        ( do skipEndOr 2
             lazyByteStringUntil (== 0x68) E
        )
          `shouldBe` (7, ["hi"], More, Right "cdefg")

    it "future" $ do
      feed ["abc"] ["def", "ghi", "jkl"]
        ( do skipEndOr 2
             lazyByteStringUntil (== 0x68) E
        )
          `shouldBe` (7, ["hi"], More, Right "cdefg")

    it "wall" $ do
      feed ["abc"] ["def", "ghi"]
        ( do skipEndOr 2
             lazyByteStringUntil (== 0x2D) E
        )
          `shouldBe` (9, [], End, Left E)


  describe "lazyByteStringRest" $ do
    it "empty" $ do
      feed [] []
        (L.toChunks <$> lazyByteStringRest @E)
          `shouldBe` (0, [], End, Right [])

    it "short" $ do
      feed ["abcdefghi"] []
        ( do skipEndOr 2
             L.toChunks <$> lazyByteStringRest @E
        )
          `shouldBe` (9, [], End, Right ["cdefghi"])

    it "known" $ do
      feed ["abc", "def", "ghi"] []
        ( do skipEndOr 2
             L.toChunks <$> lazyByteStringRest @E
        )
          `shouldBe` (9, [], End, Right ["c", "def", "ghi"])

    it "future" $ do
      feed ["abc"] ["def", "ghi", "jkl"]
        ( do skipEndOr 2
             L.toChunks <$> lazyByteStringRest @E
        )
          `shouldBe` (12, [], End, Right ["c", "def", "ghi", "jkl"])
