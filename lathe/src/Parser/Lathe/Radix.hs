{-# LANGUAGE BangPatterns #-}

{- | Conversions from individual bytes to numbers in specific formats.
 -}

module Parser.Lathe.Radix
  ( -- | === Base-2
    bin

    -- | === Base-8
  , oct

    -- | === Base-10
  , dec

    -- | === Base-16
  , hex
  , hexUpper
  , hexLower
  ) where

import           Data.Bits
import           Data.Word



{-# INLINE bin #-}
-- | Convert a binary ASCII byte into a number.
bin :: Word8 -> Maybe Word8
bin a =
  let b = a - 0x30
  in if b < 0x02
       then Just b
       else Nothing



{-# INLINE oct #-}
-- | Convert an octal ASCII byte into a number.
oct :: Word8 -> Maybe Word8
oct a =
  let b = a - 0x30
  in if b < 0x08
       then Just b
       else Nothing



{-# INLINE dec #-}
-- | Convert a decimal ASCII byte into a number.
dec :: Word8 -> Maybe Word8
dec a =
  let b = a - 0x30
  in if b < 0x0A
       then Just b
       else Nothing



{-# INLINE hex #-}
-- | Convert a hexadecimal ASCII byte into a number. Case-insensitive.
hex :: Word8 -> Maybe Word8
hex a =
  let b = a - 0x30

      c = a .&. 0xDF

  in if b < 0x0A || c - 0x41 < 0x6
       then Just $! if b < 0x0A
                      then b
                      else c - 0x37
       else Nothing



{-# INLINE hexUpper #-}
-- | Convert an upper-case hexadecimal ASCII byte into a number.
hexUpper :: Word8 -> Maybe Word8
hexUpper a =
  let b = a - 0x30
  in if b < 0x0A || b - 0x11 < 0x6
       then Just $! if b < 0x0A
                      then b
                      else b - 0x07
       else Nothing



{-# INLINE hexLower #-}
-- | Convert a lower-case hexadecimal ASCII byte into a number.
hexLower :: Word8 -> Maybe Word8
hexLower a =
  let b = a - 0x30
  in if b < 0x0A || b - 0x31 < 0x6
       then Just $! if b < 0x0A
                      then b
                      else b - 0x27
       else Nothing
