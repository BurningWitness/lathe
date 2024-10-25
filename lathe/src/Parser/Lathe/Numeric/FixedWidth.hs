{-# LANGUAGE BangPatterns
           , UnboxedTuples #-}

{- | Functions for parsing fixed-width human-readable numbers.
 -}

module Parser.Lathe.Numeric.FixedWidth
  ( int8HexFixed
  , int16HexFixed
  , int32HexFixed
  , int64HexFixed
  , word8HexFixed
  , word16HexFixed
  , word32HexFixed
  , word64HexFixed
  , floatHexFixed
  , doubleHexFixed
  ) where

import           Parser.Lathe.Internal
import           Parser.Lathe.Numeric.FixedWidth.Internal

import           Data.Int
import           Data.Word



{-# INLINE int8HexFixed #-}
-- | Consume 2 hexadecimal digits into an 'Int8'.
int8HexFixed
  :: e             -- ^ Malformed.
  -> e             -- ^ Reached end.
  -> Parser e Int8
int8HexFixed e = unsafeRead 2 (i8HexFixed e)

{-# INLINE word8HexFixed #-}
-- | Consume 2 hexadecimal digits into a 'Word8'.
word8HexFixed
  :: e              -- ^ Malformed.
  -> e              -- ^ Reached end.
  -> Parser e Word8
word8HexFixed e = unsafeRead 2 (w8HexFixed e)



{-# INLINE int16HexFixed #-}
-- | Consume 4 hexadecimal digits into an 'Int16'.
int16HexFixed
  :: e              -- ^ Malformed.
  -> e              -- ^ Reached end.
  -> Parser e Int16
int16HexFixed e = unsafeRead 4 (i16HexFixed e)

{-# INLINE word16HexFixed #-}
-- | Consume 4 hexadecimal digits into a 'Word16'.
word16HexFixed
  :: e              -- ^ Malformed.
  -> e              -- ^ Reached end.
  -> Parser e Word16
word16HexFixed e = unsafeRead 4 (w16HexFixed e)



{-# INLINE int32HexFixed #-}
-- | Consume 8 hexadecimal digits into an 'Int32'.
int32HexFixed
  :: e              -- ^ Malformed.
  -> e              -- ^ Reached end.
  -> Parser e Int32
int32HexFixed e = unsafeRead 8 (i32HexFixed e)

{-# INLINE word32HexFixed #-}
-- | Consume 8 hexadecimal digits into a 'Word32'.
word32HexFixed
  :: e               -- ^ Malformed.
  -> e               -- ^ Reached end.
  -> Parser e Word32
word32HexFixed e = unsafeRead 8 (w32HexFixed e)

{-# INLINE floatHexFixed #-}
-- | Consume 8 hexadecimal digits into a 'Float'.
floatHexFixed
  :: e              -- ^ Malformed.
  -> e              -- ^ Reached end.
  -> Parser e Float
floatHexFixed e = unsafeRead 8 (f32HexFixed e)



{-# INLINE int64HexFixed #-}
-- | Consume 16 hexadecimal digits into an 'Int64'.
int64HexFixed
  :: e              -- ^ Malformed.
  -> e              -- ^ Reached end.
  -> Parser e Int64
int64HexFixed e = unsafeRead 16 (i64HexFixed e)

{-# INLINE word64HexFixed #-}
-- | Consume 16 hexadecimal digits into a 'Word64'.
word64HexFixed
  :: e               -- ^ Malformed.
  -> e               -- ^ Reached end.
  -> Parser e Word64
word64HexFixed e = unsafeRead 16 (w64HexFixed e)

{-# INLINE doubleHexFixed #-}
-- | Consume 16 hexadecimal digits into a 'Double'.
doubleHexFixed
  :: e               -- ^ Malformed.
  -> e               -- ^ Reached end.
  -> Parser e Double
doubleHexFixed e = unsafeRead 16 (f64HexFixed e)
