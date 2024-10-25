{-# LANGUAGE BangPatterns
           , DataKinds
           , KindSignatures
           , UnboxedTuples #-}

{- | Functions for parsing UTF-32, both little-endian and big-endian.
 -}

module Parser.Lathe.Encoding.UTF32
  ( -- * Byte-order mark
    utf32BOM
    
    -- * UTF-32
  , UTF32Point (..)
  , fromUtf32
  , isSurrogate

    -- ** Parsers
  , unitUtf32BE
  , unitUtf32LE
  ) where

import           Parser.Lathe.Binary.Internal
import           Parser.Lathe.Internal

import           Data.Bits
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Unsafe as B
import           Data.Word
import           GHC.Base (unsafeChr)
import           GHC.ByteOrder



{-# INLINE utf32BOM #-}
-- | Consume 4 bytes that represent a UTF-32 byte-order mark and return
--   the corresponding 'ByteOrder'.
utf32BOM
  :: e                  -- ^ Malformed.
  -> e                  -- ^ Reached end.
  -> Parser e ByteOrder
utf32BOM malformed = unsafeRead 4 (convUtf32BOM malformed)

convUtf32BOM :: e -> ByteString -> (# Res e ByteOrder #)
convUtf32BOM e = \b ->
  let w0 = B.unsafeIndex b 0
      w1 = B.unsafeIndex b 1
      w2 = B.unsafeIndex b 2
      w3 = B.unsafeIndex b 3

  in go w0 w1 w2 w3
  where
    go w0 w1 w2 w3
      | w0 == 0x00, w1 == 0x00, w2 == 0xFE, w3 == 0xFF = (# Yes BigEndian #)
      | w0 == 0xFF, w1 == 0xFE, w2 == 0x00, w3 == 0x00 = (# Yes LittleEndian #)
      | otherwise                                      = (# No e #)



-- | Unicode code unit.
newtype UTF32Point = UTF32Point Word32

-- | Convert a code point into a 'Char'.
fromUtf32 :: UTF32Point -> Char
fromUtf32 (UTF32Point u) = unsafeChr $ fromIntegral u

-- | Check whether a code point lies in the surrogate range (@U+D800@ to @U+DFFF@).
isSurrogate :: UTF32Point -> Bool
isSurrogate (UTF32Point u) = (u .&. 0xFFFFF800) == 0x0000D800



{-# INLINE unitUtf32BE #-}
-- | Consume 4 bytes that represents a big-endian UTF-32 character.
unitUtf32BE
  :: e                   -- ^ Code unit is greater than @U+10FFFF@.
  -> e                   -- ^ Reached end.
  -> Parser e UTF32Point
unitUtf32BE e = unsafeRead 4 (u32BE e)

u32BE :: e -> ByteString -> (# Res e UTF32Point #)
u32BE e = \b ->
  let w = w32BE b
  in if w >= 0x00110000
       then (# No e #)
       else (# Yes (UTF32Point w) #)



{-# INLINE unitUtf32LE #-}
-- | Consume 4 bytes that represents a little-endian UTF-32 character.
unitUtf32LE
  :: e                   -- ^ Code unit is greater than @U+10FFFF@.
  -> e                   -- ^ Reached end.
  -> Parser e UTF32Point
unitUtf32LE e = unsafeRead 4 (u32LE e)

u32LE :: e -> ByteString -> (# Res e UTF32Point #)
u32LE e = \b ->
  let w = w32LE b
  in if w >= 0x00110000
       then (# No e #)
       else (# Yes (UTF32Point w) #)
