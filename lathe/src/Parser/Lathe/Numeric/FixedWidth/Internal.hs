{-# LANGUAGE BangPatterns
           , UnboxedTuples #-}

{-# OPTIONS_HADDOCK hide #-}

module Parser.Lathe.Numeric.FixedWidth.Internal
  ( i8HexFixed
  , w8HexFixed

  , i16HexFixed
  , w16HexFixed

  , i32HexFixed
  , w32HexFixed
  , f32HexFixed

  , i64HexFixed
  , w64HexFixed
  , f64HexFixed
  ) where

import           Parser.Lathe.Radix
import           Parser.Lathe.Internal

import           Data.Bits
import qualified Data.ByteString as B (ByteString)
import qualified Data.ByteString.Unsafe as B
import           Data.Int
import           Data.Word
import           GHC.Float (castWord32ToFloat, castWord64ToDouble)



i8HexFixed :: e -> B.ByteString -> (# Res e Int8 #)
i8HexFixed e = x8HexFixed fromIntegral e

w8HexFixed :: e -> B.ByteString -> (# Res e Word8 #)
w8HexFixed e = x8HexFixed id e

{-# INLINE x8HexFixed #-}
x8HexFixed :: (Word8 -> a) -> e -> B.ByteString -> (# Res e a #)
x8HexFixed f e = go
  where
    go b
      | Just i0 <- hex (B.unsafeIndex b 0), let !n0 =        unsafeShiftL i0 0x4
      , Just i1 <- hex (B.unsafeIndex b 1), let !n1 = n0 .|.              i1
          = let !r = f n1
            in (# Yes r #)

      | otherwise = (# No e #)



i16HexFixed :: e -> B.ByteString -> (# Res e Int16 #)
i16HexFixed e = x16HexFixed fromIntegral e

w16HexFixed :: e -> B.ByteString -> (# Res e Word16 #)
w16HexFixed e = x16HexFixed id e

{-# INLINE x16HexFixed #-}
x16HexFixed :: (Word16 -> a) -> e -> B.ByteString -> (# Res e a #)
x16HexFixed f e = go
  where
    go b
      | Just i0 <- hex (B.unsafeIndex b 0), let !n0 =        unsafeShiftL (fromIntegral i0) 0xC
      , Just i1 <- hex (B.unsafeIndex b 1), let !n1 = n0 .|. unsafeShiftL (fromIntegral i1) 0x8
      , Just i2 <- hex (B.unsafeIndex b 2), let !n2 = n1 .|. unsafeShiftL (fromIntegral i2) 0x4
      , Just i3 <- hex (B.unsafeIndex b 3), let !n3 = n2 .|.               fromIntegral i3
          = let !r = f n3
            in (# Yes r #)

      | otherwise = (# No e #)



i32HexFixed :: e -> B.ByteString -> (# Res e Int32 #)
i32HexFixed e = x32HexFixed fromIntegral e

w32HexFixed :: e -> B.ByteString -> (# Res e Word32 #)
w32HexFixed e = x32HexFixed id e

f32HexFixed :: e -> B.ByteString -> (# Res e Float #)
f32HexFixed e = x32HexFixed castWord32ToFloat e

{-# INLINE x32HexFixed #-}
x32HexFixed :: (Word32 -> a) -> e -> B.ByteString -> (# Res e a #)
x32HexFixed f e = go
  where
    go b
      | Just i0 <- hex (B.unsafeIndex b 0), let !n0 =        unsafeShiftL (fromIntegral i0) 0x1C
      , Just i1 <- hex (B.unsafeIndex b 1), let !n1 = n0 .|. unsafeShiftL (fromIntegral i1) 0x18
      , Just i2 <- hex (B.unsafeIndex b 2), let !n2 = n1 .|. unsafeShiftL (fromIntegral i2) 0x14
      , Just i3 <- hex (B.unsafeIndex b 3), let !n3 = n2 .|. unsafeShiftL (fromIntegral i3) 0x10
      , Just i4 <- hex (B.unsafeIndex b 4), let !n4 = n3 .|. unsafeShiftL (fromIntegral i4) 0x0C
      , Just i5 <- hex (B.unsafeIndex b 5), let !n5 = n4 .|. unsafeShiftL (fromIntegral i5) 0x08
      , Just i6 <- hex (B.unsafeIndex b 6), let !n6 = n5 .|. unsafeShiftL (fromIntegral i6) 0x04
      , Just i7 <- hex (B.unsafeIndex b 7), let !n7 = n6 .|.               fromIntegral i7
          = let !r = f n7
            in (# Yes r #)

      | otherwise = (# No e #)



i64HexFixed :: e -> B.ByteString -> (# Res e Int64 #)
i64HexFixed e = x64HexFixed fromIntegral e

w64HexFixed :: e -> B.ByteString -> (# Res e Word64 #)
w64HexFixed e = x64HexFixed id e

f64HexFixed :: e -> B.ByteString -> (# Res e Double #)
f64HexFixed e = x64HexFixed castWord64ToDouble e

{-# INLINE x64HexFixed #-}
x64HexFixed :: (Word64 -> a) -> e -> B.ByteString -> (# Res e a #)
x64HexFixed f e = go
  where
    go b
      | Just i0 <- hex (B.unsafeIndex b 0x0), let !n0 =        unsafeShiftL (fromIntegral i0) 0x3C
      , Just i1 <- hex (B.unsafeIndex b 0x1), let !n1 = n0 .|. unsafeShiftL (fromIntegral i1) 0x38
      , Just i2 <- hex (B.unsafeIndex b 0x2), let !n2 = n1 .|. unsafeShiftL (fromIntegral i2) 0x34
      , Just i3 <- hex (B.unsafeIndex b 0x3), let !n3 = n2 .|. unsafeShiftL (fromIntegral i3) 0x30
      , Just i4 <- hex (B.unsafeIndex b 0x4), let !n4 = n3 .|. unsafeShiftL (fromIntegral i4) 0x2C
      , Just i5 <- hex (B.unsafeIndex b 0x5), let !n5 = n4 .|. unsafeShiftL (fromIntegral i5) 0x28
      , Just i6 <- hex (B.unsafeIndex b 0x6), let !n6 = n5 .|. unsafeShiftL (fromIntegral i6) 0x24
      , Just i7 <- hex (B.unsafeIndex b 0x7), let !n7 = n6 .|. unsafeShiftL (fromIntegral i7) 0x20
      , Just i8 <- hex (B.unsafeIndex b 0x8), let !n8 = n7 .|. unsafeShiftL (fromIntegral i8) 0x1C
      , Just i9 <- hex (B.unsafeIndex b 0x9), let !n9 = n8 .|. unsafeShiftL (fromIntegral i9) 0x18
      , Just iA <- hex (B.unsafeIndex b 0xA), let !nA = n9 .|. unsafeShiftL (fromIntegral iA) 0x14
      , Just iB <- hex (B.unsafeIndex b 0xB), let !nB = nA .|. unsafeShiftL (fromIntegral iB) 0x10
      , Just iC <- hex (B.unsafeIndex b 0xC), let !nC = nB .|. unsafeShiftL (fromIntegral iC) 0x0C
      , Just iD <- hex (B.unsafeIndex b 0xD), let !nD = nC .|. unsafeShiftL (fromIntegral iD) 0x08
      , Just iE <- hex (B.unsafeIndex b 0xE), let !nE = nD .|. unsafeShiftL (fromIntegral iE) 0x04
      , Just iF <- hex (B.unsafeIndex b 0xF), let !nF = nE .|.               fromIntegral iF
          = let !r = f nF
            in (# Yes r #)

      | otherwise = (# No e #)
