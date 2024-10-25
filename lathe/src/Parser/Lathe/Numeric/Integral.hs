{-# LANGUAGE BangPatterns
           , UnboxedTuples #-}

{- | Functions for parsing bounded integers expressed as numbers
     with no fractional part or exponent.

     Parsing functions in this module are only guaranteed to operate correctly when
     their arguments are sensible and the input number has no leading zeroes.

     == Example

     Converting @"-123,040,567 "@ to an 'Int32', skipping operations
     outside of this module:

 @
 >>> 'parse' ('wholeInt32Dec' () Minus (WholeInt 0) 0) "123,040,567 "
 (Scrap 3 ",040,567 " End,Right (WholeInt 123,3))

 >>> parse (wholeInt32Dec () Minus (WholeInt 123) 3) "040,567 "
 (Scrap 3 ",567 " End,Right (WholeInt 123040,6))

 >>> parse (wholeInt32Dec () Minus (WholeInt 123040) 6) "567 "
 (Scrap 3 " " End,Right (WholeInt 123040567,9))

 >>> 'wholeToInt32' Minus (WholeInt 123040567)
 -123040567
 @
 -}

module Parser.Lathe.Numeric.Integral
  ( -- * Representation
    -- ** Signed
    Sign (..)
  , WholeInt (..)

    -- | === Conversions
  , wholeToInt8
  , wholeToInt16
  , wholeToInt32
  , wholeToInt64
  , wholeToInt

    -- * Parsing
    -- ** Decimal
    -- *** Unsigned
  , wholeWord8Dec
  , wholeWord16Dec
  , wholeWord32Dec
  , wholeWord64Dec
  , wholeWordDec
  , wholeNaturalDec

    -- *** Signed
  , wholeInt8Dec
  , wholeInt16Dec
  , wholeInt32Dec
  , wholeInt64Dec
  , wholeIntDec

    -- ** Hexadecimal
    -- *** Unsigned
  , wholeWord8Hex
  , wholeWord16Hex
  , wholeWord32Hex
  , wholeWord64Hex
  , wholeWordHex
  ) where

import           Parser.Lathe.Radix
import           Parser.Lathe.Internal
import           Parser.Lathe.Internal.Bitness
import           Parser.Lathe.Numeric.Integral.Internal
import           Parser.Lathe.Numeric.Internal

import           Data.Bits
import qualified Data.ByteString as B (ByteString)
import qualified Data.ByteString.Unsafe as B
import           Data.Int
import           Data.Word
import           Numeric.Natural



-- | Consume up to 3 decimal digits into a 'Word8'.
wholeWord8Dec
  :: overflow
  -> Word8
  -> Int      -- ^ Number of decimal digits consumed by this number.
  -> Parser overflow (Word8, Int)
wholeWord8Dec = wholeWordDec_ u8

-- | Consume up to 5 decimal digits into a 'Word16'.
wholeWord16Dec
  :: overflow
  -> Word16
  -> Int      -- ^ Number of decimal digits consumed by this number.
  -> Parser overflow (Word16, Int)
wholeWord16Dec = wholeWordDec_ u16

-- | Consume up to 10 decimal digits into a 'Word32'.
wholeWord32Dec
  :: overflow
  -> Word32
  -> Int      -- ^ Number of decimal digits consumed by this number.
  -> Parser overflow (Word32, Int)
wholeWord32Dec = wholeWordDec_ u32

-- | Consume up to 20 decimal digits into a 'Word64'.
wholeWord64Dec
  :: overflow
  -> Word64
  -> Int      -- ^ Number of decimal digits consumed by this number.
  -> Parser overflow (Word64, Int)
wholeWord64Dec = wholeWordDec_ u64

-- | Consume up to 10 or 20 decimal digits
--   (depending on machine integer size) into a 'Word'.
wholeWordDec
  :: overflow
  -> Word
  -> Int      -- ^ Number of decimal digits consumed by this number.
  -> Parser overflow (Word, Int)
wholeWordDec = wholeWordDec_ (caseWordSize_32_64 u32 u64)


data State a = State
                 {-# UNPACK #-} !(Flow a)
                 {-# UNPACK #-} !Int      -- ^ Number of bytes consumed

data Flow a = Number !a
            | Overflow

{-# INLINE wholeWordDec_ #-}
wholeWordDec_
  :: (Ord a, Num a)
  => (Int, a, a)
  -> overflow -> a -> Int -> Parser overflow (a, Int)
wholeWordDec_ (len, bound, tip) overflow = \v n -> do
  State r n' <- go v n `catch` pure
  unsafeSkipEndOr $ fromIntegral (n' - n)
  case r of
    Number v' -> pure (v', n')
    Overflow  -> err overflow
  where
    go !v !n = do
      w <- word8 $ State (Number v) n
      case dec w of
        Nothing -> err $ State (Number v) n
        Just i  -> let !v' = v * 10 + fromIntegral i
                       n' = n + 1

                   in if n' <= len
                        then go v' n'
                        else case compare v bound of
                               LT -> checkOverflow v' n'

                               EQ -> if fromIntegral i <= tip
                                       then checkOverflow v' n'
                                       else err $ State Overflow n

                               GT -> err $ State Overflow n

    checkOverflow !v !n = do
      w <- word8 $ State (Number v) n

      let !(# r #) = case dec w of
                       Nothing -> (# Number v #)
                       Just _  -> (# Overflow #)
      err $ State r n



data Digit = Digit !Word !Int

data Delim = Chunk
           | Rest
             deriving Show

data Return = Return !Delim !Word !Int
              deriving Show

data Semi = Edge
          | Part !Word !Int
            deriving Show

unsafeWordDec :: Parser never Return
unsafeWordDec = do
  Digit v n <- go 0 0 `catch` pure
  unsafeSkipEndOr (fromIntegral n)
  pure (Return Rest v n)
  where
    go !v !n = do
      w <- word8 $ Digit v n
      case dec w of
        Nothing -> err $ Digit v n
        Just i  -> let !v' = v * 10 + fromIntegral i
                       !n' = n + 1

                   in go v' n'

bulkDec :: Int -> B.ByteString -> (# Res Semi Return #)
bulkDec len = \b ->
  
  let go n v
        | n >= len  = (# Yes (Return Chunk v len) #)
        | otherwise =
            case dec $ B.unsafeIndex b n of
              Nothing -> (# No (Part v n) #)
              Just i  -> go (n + 1) $ v * 10 + fromIntegral i


  in go 0 0

-- | Consume any number of decimal digits into a 'Natural'.
wholeNaturalDec :: Natural -> Parser never Natural
wholeNaturalDec v0 = go v0
  where
    digitsOf10 = caseWordSize_32_64 9 19

    go !vT = do
      Return delim v n <- unsafeRead digitsOf10 (bulkDec digitsOf10) Edge
                            `catch` \semi ->
                              case semi of
                                Edge     -> unsafeWordDec
                                Part v n -> do
                                  unsafeSkipEndOr (fromIntegral n)
                                  pure $ Return Rest v n

      let !vT' = vT * (10 ^ n) + fromIntegral v

      case delim of
        Chunk -> go vT'
        Rest  -> pure vT'




-- | Consume up to 2 hexadecimal digits into a 'Word8'.
wholeWord8Hex
  :: overflow
  -> Word8
  -> Int      -- ^ Number of hexadecimal digits consumed by this number.
  -> Parser overflow (Word8, Int)
wholeWord8Hex = wholeWordHex_ 2

-- | Consume up to 4 hexadecimal digits into a 'Word16'.
wholeWord16Hex
  :: overflow
  -> Word16
  -> Int      -- ^ Number of hexadecimal digits consumed by this number.
  -> Parser overflow (Word16, Int)
wholeWord16Hex = wholeWordHex_ 4

-- | Consume up to 8 hexadecimal digits into a 'Word32'.
wholeWord32Hex
  :: overflow
  -> Word32
  -> Int      -- ^ Number of hexadecimal digits consumed by this number.
  -> Parser overflow (Word32, Int)
wholeWord32Hex = wholeWordHex_ 8

-- | Consume up to 16 hexadecimal digits into a 'Word64'.
wholeWord64Hex
  :: overflow
  -> Word64
  -> Int      -- ^ Number of hexadecimal digits consumed by this number.
  -> Parser overflow (Word64, Int)
wholeWord64Hex = wholeWordHex_ 16

-- | Consume up to 8 or 16 hexadecimal digits
--   (depending on current platform's integer size) into a 'Word'.
wholeWordHex
  :: overflow
  -> Word
  -> Int      -- ^ Number of hexadecimal digits consumed by this number.
  -> Parser overflow (Word, Int)
wholeWordHex = wholeWordHex_ (caseWordSize_32_64 8 16)


{-# INLINE wholeWordHex_ #-}
wholeWordHex_
  :: (Bits a, Num a)
  => Int
  -> overflow -> a -> Int -> Parser overflow (a, Int)
wholeWordHex_ len overflow = \v n -> do
  State r n' <- go v n `catch` pure
  unsafeSkipEndOr $ fromIntegral (n' - n)
  case r of
    Number v' -> pure (v', n')
    Overflow  -> err overflow
  where
    go !v !n = do
      w <- word8 $ State (Number v) n
      case hex w of
        Nothing -> err $ State (Number v) n
        Just i  -> let !v' = unsafeShiftL v 4 + fromIntegral i
                       n' = n + 1

                   in if n' < len
                        then go v' n'
                        else checkOverflow v' n'

    checkOverflow !v !n = do
      w <- word8 $ State (Number v) n

      let !(# r #) = case hex w of
                       Nothing -> (# Number v #)
                       Just _  -> (# Overflow #)
      err $ State r n



-- | Intermediate representation of a signed integer.
newtype WholeInt word = WholeInt
                          word   -- ^ Number as an unsigned integer.
                        deriving Show

-- | Convert the intermediate representation into an 'Int8'.
wholeToInt8 :: Sign -> WholeInt Word8 -> Int8
wholeToInt8 = wholeToInt_

-- | Convert the intermediate representation into an 'Int16'.
wholeToInt16 :: Sign -> WholeInt Word16 -> Int16
wholeToInt16 = wholeToInt_

-- | Convert the intermediate representation into an 'Int32'.
wholeToInt32 :: Sign -> WholeInt Word32 -> Int32
wholeToInt32 = wholeToInt_

-- | Convert the intermediate representation into an 'Int64'.
wholeToInt64 :: Sign -> WholeInt Word64 -> Int64
wholeToInt64 = wholeToInt_

-- | Convert the intermediate representation into an 'Int'.
wholeToInt :: Sign -> WholeInt Word -> Int
wholeToInt = wholeToInt_

{-# INLINE wholeToInt_ #-}
wholeToInt_ :: (Integral w, Num i) => Sign -> WholeInt w -> i
wholeToInt_ Plus  (WholeInt w) = fromIntegral w
wholeToInt_ Minus (WholeInt w) = fromIntegral (negate w)



-- | Consume up to 3 decimal digits into an 'Int8'-compatible container.
wholeInt8Dec
  :: overflow
  -> Sign
  -> WholeInt Word8
  -> Int            -- ^ Number of decimal digits consumed by this number.
  -> Parser overflow (WholeInt Word8, Int)
wholeInt8Dec = wholeIntDec_ i8

-- | Consume up to 5 decimal digits into an 'Int16'-compatible container.
wholeInt16Dec
  :: overflow
  -> Sign
  -> WholeInt Word16
  -> Int             -- ^ Number of decimal digits consumed by this number.
  -> Parser overflow (WholeInt Word16, Int)
wholeInt16Dec = wholeIntDec_ i16

-- | Consume up to 10 decimal digits into an 'Int32'-compatible container.
wholeInt32Dec
  :: overflow
  -> Sign
  -> WholeInt Word32
  -> Int             -- ^ Number of decimal digits consumed by this number.
  -> Parser overflow (WholeInt Word32, Int)
wholeInt32Dec = wholeIntDec_ i32

-- | Consume up to 19 decimal digits into an 'Int64'-compatible container.
wholeInt64Dec
  :: overflow
  -> Sign
  -> WholeInt Word64
  -> Int             -- ^ Number of decimal digits consumed by this number.
  -> Parser overflow (WholeInt Word64, Int)
wholeInt64Dec = wholeIntDec_ i64

-- | Consume up to 10 or 19 decimal digits (depending on current platform's integer size)
--   into an 'Int'-compatible container.
wholeIntDec
  :: overflow
  -> Sign
  -> WholeInt Word
  -> Int           -- ^ Number of decimal digits consumed by this number.
  -> Parser overflow (WholeInt Word, Int)
wholeIntDec = wholeIntDec_ (caseWordSize_32_64 i32 i64)

{-# INLINE wholeIntDec_ #-}
wholeIntDec_
  :: (Ord a, Num a)
  => (Int, a, a)
  -> overflow -> Sign -> WholeInt a -> Int -> Parser overflow (WholeInt a, Int)
wholeIntDec_ (len, bound, tip) overflow sign = \(WholeInt v) n -> do
  State r n' <- go v n `catch` pure
  unsafeSkipEndOr $ fromIntegral (n' - n)
  case r of
    Number v' -> pure (WholeInt v', n')
    Overflow  -> err overflow
  where
    go !v !n = do
      w <- word8 $ State (Number v) n
      case dec w of
        Nothing -> err $ State (Number v) n
        Just i  -> let !v' = v * 10 + fromIntegral i
                       n' = n + 1

                   in if n' <= len
                        then go v' n'
                        else case compare v bound of
                               LT -> checkOverflow v' n'

                               EQ -> if fromIntegral i <= case sign of
                                                            Plus  -> tip
                                                            Minus -> tip + 1

                                       then checkOverflow v' n'
                                       else err $ State Overflow n

                               GT -> err $ State Overflow n

    checkOverflow !v !n = do
      w <- word8 $ State (Number v) n

      let !(# r #) = case dec w of
                       Nothing -> (# Number v #)
                       Just _  -> (# Overflow #)
      err $ State r n
