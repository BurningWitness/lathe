{-# LANGUAGE BangPatterns
           , NumericUnderscores
           , UnboxedTuples #-}

{- | Functions for parsing bounded numbers expressed as fractions.

     Parsing functions in this module are only guaranteed to operate correctly when
     their arguments are sensible and the input number has no leading zeroes.

     == Example

     Converting @"-1200.00340e6"@ to an 'Int32', skipping operations
     outside of this module:

 @
 >>> 'parse' ('fracInt32Dec' () Minus (FracInt 0 0) 0) "1200.00340e6"
 (Scrap 4 ".00340e6" End,Right (FracInt 12 2))

 >>> parse (fracInt32Dec () Minus (FracInt 12 2) 4) "00340e6"
 (Scrap 5 "e6" End,Right (FracInt 12000034 8))

 >>> let ex = 6 + 4 -- Exponent plus number of digits before decimal point

 >>> 'fracToInt32' Minus (FracInt 12000034 8) ex
 Proper (-12000034000)
 @

     Similarly, @"123456789098.7654321e-17"@ to 'Float':

 @
 >>> parse ('fracFloat23Dec' (FracFloat 0 0)) "123456789098.7654321e-17"
 (Scrap 12 ".7654321e-17" End,Right (FracFloat 123456789 9))

 >>> parse (fracFloat23Dec (FracFloat 123456789 9)) "7654321e-17"
 (Scrap 7 "e-17" End,Right (FracFloat 123456789 9))

 >>> 'fracToFloat' Plus (FracFloat 123456789 9) ((-17) + 12)
 1.2345679e-6
 @
 -}

module Parser.Lathe.Numeric.Fractional
  ( -- * Representation
    Sign (..)
  , OverUnder (..)
    -- ** Unsigned integral
  , FracWord (..)

    -- | === Conversions
  , fracToWord8
  , fracToWord16
  , fracToWord32
  , fracToWord64
  , fracToWord

    -- ** Signed integral
  , FracInt (..)

    -- | === Conversions
  , fracToInt8
  , fracToInt16
  , fracToInt32
  , fracToInt64
  , fracToInt

    -- ** Floating-point
  , FracFloat (..)

  , fracToFloat
  , fracToDouble

    -- * Parsing
    -- ** Decimal
    -- *** Unsigned integral
  , fracWord8Dec
  , fracWord16Dec
  , fracWord32Dec
  , fracWord64Dec
  , fracWordDec

    -- *** Signed integral
  , fracInt8Dec
  , fracInt16Dec
  , fracInt32Dec
  , fracInt64Dec
  , fracIntDec

    -- *** Floating-point
  , fracFloat23Dec
  , fracFloat52Dec
  ) where

import           Parser.Lathe.Radix
import           Parser.Lathe.Internal
import           Parser.Lathe.Internal.Bitness
import           Parser.Lathe.Numeric.Integral.Internal
import           Parser.Lathe.Numeric.Internal

import           Control.Monad.ST
import           Data.Primitive.PrimArray
import           Data.Int
import           Data.Ratio
import           Data.Word
import           GHC.Float (castWord32ToFloat, castWord64ToDouble)



-- All powers of 10 that fit into an unsigned platform integer.
powersOf10 :: PrimArray Word
powersOf10 =
  runST $ do
    let len = caseWordSize_32_64 10 20

    arr <- newPrimArray len

    let go !v !n
          | n >= len  = pure ()
          | otherwise = do
              writePrimArray arr n v

              let n' = n + 1
                  v' = v * 10
              go v' n'

    go 1 0

    unsafeFreezePrimArray arr



-- | Whether the integer can be represented properly.
data OverUnder a = Proper !a
                 | Over      -- ^ Overflow.
                 | Under     -- ^ Underflow.
                   deriving Show



-- | Intermediate representation of an unsigned integer.
data FracWord word = FracWord
                       !word -- ^ Either @0@ or a number with a non-zero lowest digit.
                       !Int  -- ^ Number of decimal digits consumed by this integer.
                     deriving Show

-- | Convert the intermediate representation into a 'Word8', if possible.
fracToWord8
  :: FracWord Word8
  -> Integer        -- ^ Radix-10 order of magnitude of the expected result.
  -> OverUnder Word8
fracToWord8 = fracToWord_ u8

-- | Convert the intermediate representation into a 'Word16', if possible.
fracToWord16
  :: FracWord Word16
  -> Integer        -- ^ Radix-10 order of magnitude of the expected result.
  -> OverUnder Word16
fracToWord16 = fracToWord_ u16

-- | Convert the intermediate representation into a 'Word32', if possible.
fracToWord32
  :: FracWord Word32
  -> Integer        -- ^ Radix-10 order of magnitude of the expected result.
  -> OverUnder Word32
fracToWord32 = fracToWord_ u32

-- | Convert the intermediate representation into a 'Word64', if possible.
fracToWord64
  :: FracWord Word64
  -> Integer        -- ^ Radix-10 order of magnitude of the expected result.
  -> OverUnder Word64
fracToWord64 = fracToWord_ u64

-- | Convert the intermediate representation into a 'Word', if possible.
fracToWord
  :: FracWord Word
  -> Integer        -- ^ Radix-10 order of magnitude of the expected result.
  -> OverUnder Word
fracToWord = fracToWord_ (caseWordSize_32_64 u32 u64)

{-# INLINE fracToWord #-}
fracToWord_
  :: (Num w, Ord w) => (Int, w, w) -> FracWord w -> Integer -> OverUnder w
fracToWord_ (len, bound, _tip) (FracWord v e) n =
  case compare n (fromIntegral e) of
    LT -> Under
    EQ -> Proper v
    GT ->
      let n' = n - 1
      in case compare n' (fromIntegral len) of
           LT -> Proper $ v * fromIntegral
                                (indexPrimArray powersOf10 (fromIntegral n - e))

           EQ -> let v' = v * fromIntegral
                                (indexPrimArray powersOf10 (fromIntegral n' - e))

                 in if v' <= bound
                      then Proper $ v' * 10
                      else Over

           GT -> Over



-- | Consume up to 3 decimal digits and any number of zeroes after into a
--   'Word8'-compatible container.
fracWord8Dec
  :: overflow
  -> FracWord Word8
  -> Int64          -- ^ Number of decimal digits consumed by this number.
  -> Parser overflow (FracWord Word8)
fracWord8Dec = fracWordDec_ u8

-- | Consume up to 5 decimal digits and any number of zeroes after into a
--   'Word16'-compatible container.
fracWord16Dec
  :: overflow
  -> FracWord Word16
  -> Int64           -- ^ Number of decimal digits consumed by this number.
  -> Parser overflow (FracWord Word16)
fracWord16Dec = fracWordDec_ u16

-- | Consume up to 9 decimal digits and any number of zeroes after into a
--   'Word32'-compatible container.
fracWord32Dec
  :: overflow
  -> FracWord Word32
  -> Int64           -- ^ Number of decimal digits consumed by this number.
  -> Parser overflow (FracWord Word32)
fracWord32Dec = fracWordDec_ u32

-- | Consume up to 20 decimal digits and any number of zeroes after into a
--   'Word64'-compatible container.
fracWord64Dec
  :: overflow
  -> FracWord Word64
  -> Int64           -- ^ Number of decimal digits consumed by this number.
  -> Parser overflow (FracWord Word64)
fracWord64Dec = fracWordDec_ u64

-- | Consume up to 9 or 20 decimal digits (depending on current platform's integer size)
--   and any number of zeroes after into a 'Word'-compatible container.
fracWordDec
  :: overflow
  -> FracWord Word
  -> Int64         -- ^ Number of decimal digits consumed by this number.
  -> Parser overflow (FracWord Word)
fracWordDec = fracWordDec_ (caseWordSize_32_64 u32 u64)


data State a = State
                 {-# UNPACK #-} !(Flow a)
                 {-# UNPACK #-} !Int      -- ^ Number of bytes consumed total.

data Flow a = Number
                 {-# UNPACK #-} !Enough
                                !a
                 {-# UNPACK #-} !Int      -- ^ Number of significand bytes.
            | Overflow

data Enough = Enough
            | Rush

{-# INLINE fracWordDec_ #-}
fracWordDec_
  :: (Ord w, Num w)
  => (Int, w, w)
  -> overflow -> FracWord w -> Int64
  -> Parser overflow (FracWord w)
fracWordDec_ (len, bound, tip) overflow = \(FracWord v0 e0) n0 ->
  if n0 > fromIntegral len
    then fast v0 e0
    else do

      let slow !v !e !n = do
            w <- word8 $ State (Number Enough v e) n
            case dec w of
              Nothing -> err $ State (Number Enough v e) n
              Just i  ->
                let n' = n + 1
                in if n' <= len
                     then if i == 0x00
                            then slow v e n'
                            else let v' = v * fromIntegral
                                                (indexPrimArray powersOf10 (n' - e))

                                        + fromIntegral i

                                 in slow v' n' n'

                     else err $!
                            if i == 0x00
                              then State (Number Rush v e) n'
                              else let v' = v * fromIntegral
                                                  (indexPrimArray powersOf10 (n - e))

                                       v'' = v' * 10 + fromIntegral i

                                   in case compare v' bound of
                                        LT -> State (Number Rush v'' n') n'

                                        EQ -> if fromIntegral i <= tip
                                                then State (Number Rush v'' n') n'
                                                else State Overflow n

                                        GT -> State Overflow n

      State res n' <- slow v0 e0 (fromIntegral n0) `catch` pure

      let !n1 = fromIntegral n'

      unsafeSkipEndOr (n1 - n0)

      case res of
        Number rush v1 e1 -> do
          case rush of
            Enough -> pure (FracWord v1 e1)
            Rush   -> fast v1 e1

        Overflow -> err overflow
  where
    fast !v !e = do
      skipUntilEndOr (/= 0x30)

      let checkOverflow = do
            w <- word8 False
            err (maybe False (\_ -> True) $ dec w)

      over <- checkOverflow `catch` pure
      if over
        then err overflow
        else pure (FracWord v e)



-- | Intermediate representation of a signed integer.
data FracInt word = FracInt
                      !word -- ^ Either @0@ or a number with a non-zero lowest digit.
                      !Int  -- ^ Number of decimal digits consumed by this integer.
                    deriving Show

-- | Convert the intermediate representation into an 'Int8', if possible.
fracToInt8
  :: Sign
  -> FracInt Word8
  -> Integer       -- ^ Radix-10 order of magnitude of the expected result.
  -> OverUnder Int8
fracToInt8 = fracToInt_ i8

-- | Convert the intermediate representation into an 'Int16', if possible.
fracToInt16
  :: Sign
  -> FracInt Word16
  -> Integer        -- ^ Radix-10 order of magnitude of the expected result.
  -> OverUnder Int16
fracToInt16 = fracToInt_ i16

-- | Convert the intermediate representation into an 'Int32', if possible.
fracToInt32
  :: Sign
  -> FracInt Word32
  -> Integer        -- ^ Radix-10 order of magnitude of the expected result.
  -> OverUnder Int32
fracToInt32 = fracToInt_ i32

-- | Convert the intermediate representation into an 'Int64', if possible.
fracToInt64
  :: Sign
  -> FracInt Word64
  -> Integer        -- ^ Radix-10 order of magnitude of the expected result.
  -> OverUnder Int64
fracToInt64 = fracToInt_ i64

-- | Convert the intermediate representation into an 'Int', if possible.
fracToInt
  :: Sign
  -> FracInt Word
  -> Integer      -- ^ Radix-10 order of magnitude of the expected result.
  -> OverUnder Int
fracToInt = fracToInt_ (caseWordSize_32_64 i32 i64)

{-# INLINE fracToInt #-}
fracToInt_
  :: (Integral w, Num i) => (Int, w, w) -> Sign -> FracInt w -> Integer -> OverUnder i
fracToInt_ (len, bound, _tip) sign (FracInt v e) n =
  let {-# INLINE signed #-}
      signed x = case sign of
                   Plus  -> fromIntegral x
                   Minus -> fromIntegral (negate x)

  in case compare n (fromIntegral e) of
       LT -> Under
       EQ -> Proper $ signed v
       GT ->
         let n' = n - 1
         in case compare n' (fromIntegral len) of
              LT -> Proper . signed $ v * fromIntegral
                                            (indexPrimArray powersOf10 (fromIntegral n - e))

              EQ -> let v' = v * fromIntegral
                                   (indexPrimArray powersOf10 (fromIntegral n' - e))

                    in if v' <= bound
                         then Proper . signed $ v' * 10
                         else Over

              GT -> Over



-- | Consume up to 3 decimal digits and any number of zeroes after into an
--   'Int8'-compatible container.
fracInt8Dec
  :: overflow
  -> Sign
  -> FracInt Word8
  -> Int64         -- ^ Number of decimal digits consumed by this number.
  -> Parser overflow (FracInt Word8)
fracInt8Dec = fracIntDec_ i8

-- | Consume up to 5 decimal digits and any number of zeroes after into an
--   'Int16'-compatible container.
fracInt16Dec
  :: overflow
  -> Sign
  -> FracInt Word16
  -> Int64          -- ^ Number of decimal digits consumed by this number.
  -> Parser overflow (FracInt Word16)
fracInt16Dec = fracIntDec_ i16

-- | Consume up to 9 decimal digits and any number of zeroes after into an
--   'Int32'-compatible container.
fracInt32Dec
  :: overflow
  -> Sign
  -> FracInt Word32
  -> Int64          -- ^ Number of decimal digits consumed by this number.
  -> Parser overflow (FracInt Word32)
fracInt32Dec = fracIntDec_ i32

-- | Consume up to 19 decimal digits and any number of zeroes after into an
--   'Int64'-compatible container.
fracInt64Dec
  :: overflow
  -> Sign
  -> FracInt Word64
  -> Int64          -- ^ Number of decimal digits consumed by this number.
  -> Parser overflow (FracInt Word64)
fracInt64Dec = fracIntDec_ i64

-- | Consume up to 9 or 19 decimal digits (depending on current platform's integer size)
--   and any number of zeroes after into an 'Int'-compatible container.
fracIntDec
  :: overflow
  -> Sign
  -> FracInt Word
  -> Int64        -- ^ Number of decimal digits consumed by this number.
  -> Parser overflow (FracInt Word)
fracIntDec = fracIntDec_ (caseWordSize_32_64 i32 i64)


{-# INLINE fracIntDec_ #-}
fracIntDec_
  :: (Show w, Ord w, Num w)
  => (Int, w, w)
  -> overflow -> Sign -> FracInt w -> Int64
  -> Parser overflow (FracInt w)
fracIntDec_ (len, bound, tip) overflow sign = \(FracInt v0 e0) n0 ->
  if n0 > fromIntegral len
    then fast v0 e0
    else do

      let slow !v !e !n = do
            w <- word8 $ State (Number Enough v e) n
            case dec w of
              Nothing -> err $ State (Number Enough v e) n
              Just i  ->
                let n' = n + 1
                in if n' <= len
                     then if i == 0x00
                            then slow v e n'
                            else let v' = v * fromIntegral
                                                (indexPrimArray powersOf10 (n' - e))

                                        + fromIntegral i

                                 in slow v' n' n'

                     else err $!
                            if i == 0x00
                              then State (Number Rush v e) n'
                              else let v' = v * fromIntegral
                                                  (indexPrimArray powersOf10 (n - e))

                                       v'' = v' * 10 + fromIntegral i

                                   in case compare v' bound of
                                        LT -> State (Number Rush v'' n') n'

                                        EQ -> if fromIntegral i <= case sign of
                                                                     Plus  -> tip
                                                                     Minus -> tip + 1

                                                then State (Number Rush v'' n') n'
                                                else State Overflow n

                                        GT -> State Overflow n

      State res n' <- slow v0 e0 (fromIntegral n0) `catch` pure

      let !n1 = fromIntegral n'

      unsafeSkipEndOr (n1 - n0)

      case res of
        Number rush v1 e1 -> do
          case rush of
            Enough -> pure (FracInt v1 e1)
            Rush   -> fast v1 e1

        Overflow -> err overflow
  where
    fast !v !e = do
      skipUntilEndOr (/= 0x30)

      let checkOverflow = do
            w <- word8 False
            err (maybe False (\_ -> True) $ dec w)

      over <- checkOverflow `catch` pure
      if over
        then err overflow
        else pure (FracInt v e)



-- | Intermediate representation of a floating-point number's significand.
data FracFloat word = FracFloat
                        !word -- ^ Significand in untrimmed integer form.
                        !Int  -- ^ Number of decimal digits consumed by the significand.
                      deriving Show



-- | Convert the intermediate representation to a 'Float'.
fracToFloat
  :: Sign
  -> FracFloat Word32
  -> Integer          -- ^ Radix-10 order of magnitude of the expected result.
  -> Float
fracToFloat sig (FracFloat v e) n =
  let f | n >   39  = castWord32ToFloat 0x7F80_0000 -- infinity
        | n >    0  = fromRational (fromIntegral v * (10 ^ n) % (10 ^ e))
        | n < (-44) = 0
        | otherwise = fromRational (fromIntegral v % (10 ^ (fromIntegral e - n)))

  in case sig of
       Plus  -> f
       Minus -> negate f

-- | Convert the intermediate representation to a 'Double'.
fracToDouble
  :: Sign
  -> FracFloat Word64
  -> Integer          -- ^ Radix-10 order of magnitude of the expected result.
  -> Double
fracToDouble sig (FracFloat v e) n =
  let f | n >   309  = castWord64ToDouble 0x7FF0_0000_0000_0000 -- infinity
        | n >     0  = fromRational (fromIntegral v * (10 ^ n) % (10 ^ e))
        | n < (-323) = 0
        | otherwise  = fromRational (fromIntegral v % (10 ^ (fromIntegral e - n)))

  in case sig of
       Plus  -> f
       Minus -> negate f



-- | Consume any number of consecutive decimal digits into a 'Float'-compatible
--   container.
--
--   The container retains no more than 9 leading decimal digits.
fracFloat23Dec :: FracFloat Word32 -> Parser never (FracFloat Word32)
fracFloat23Dec = fracFloatDec_ 8

-- | Consume any number of consecutive decimal digits into a 'Double'-compatible
--   container.
--
--   The container retains no more than 17 leading decimal digits.
fracFloat52Dec :: FracFloat Word64 -> Parser never (FracFloat Word64)
fracFloat52Dec = fracFloatDec_ 16



data Fate a = Fate
                {-# UNPACK #-} !Gear
                               !a
                {-# UNPACK #-} !Int  -- ^ Number of bytes consumed.

data Gear = Slow
          | Fast

{-# INLINE fracFloatDec_ #-}
fracFloatDec_ :: Num w => Int -> FracFloat w -> Parser e (FracFloat w)
fracFloatDec_ len = \(FracFloat v0 e) ->
  if e >= len
    then fast v0 e
    else do

      let slow !v !n = do
            w <- word8 $ Fate Slow v n
            case dec w of
              Nothing -> err $ Fate Slow v n
              Just i  -> let !v' = v * 10 + fromIntegral i
                             !n' = n + 1

                         in if n < len - e
                              then slow v' n'
                              else err $ Fate Fast v' n'

      Fate gear v1 n <- slow v0 0 `catch` pure
      unsafeSkipEndOr $ fromIntegral n

      let !e' = e + n
      case gear of
        Slow -> pure (FracFloat v1 e')
        Fast -> fast v1 e'
  where
    fast !v !e = do
      skipUntilEndOr (maybe True (\_ -> False) . dec)
      pure (FracFloat v e)
