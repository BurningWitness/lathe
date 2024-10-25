{-# OPTIONS_HADDOCK hide #-}

module Parser.Lathe.Binary.Internal
  ( w16LE
  , w16BE

  , w32LE
  , w32BE

  , w64LE
  , w64BE
  ) where

import           Data.Bits
import qualified Data.ByteString as B (ByteString)
import qualified Data.ByteString.Unsafe as B
import           Data.Word



{-# INLINE w16LE #-}
w16LE :: B.ByteString -> Word16
w16LE s =
      (fromIntegral (s `B.unsafeIndex` 1) `unsafeShiftL` 0x08)
  .|.  fromIntegral (s `B.unsafeIndex` 0)

{-# INLINE w16BE #-}
w16BE :: B.ByteString -> Word16
w16BE s =
      (fromIntegral (s `B.unsafeIndex` 0) `unsafeShiftL` 0x08)
  .|.  fromIntegral (s `B.unsafeIndex` 1)



{-# INLINE w32LE #-}
w32LE :: B.ByteString -> Word32
w32LE s =
      (fromIntegral (s `B.unsafeIndex` 3) `unsafeShiftL` 0x18)
  .|. (fromIntegral (s `B.unsafeIndex` 2) `unsafeShiftL` 0x10)
  .|. (fromIntegral (s `B.unsafeIndex` 1) `unsafeShiftL` 0x08)
  .|.  fromIntegral (s `B.unsafeIndex` 0)

{-# INLINE w32BE #-}
w32BE :: B.ByteString -> Word32
w32BE s =
      (fromIntegral (s `B.unsafeIndex` 0) `unsafeShiftL` 0x18)
  .|. (fromIntegral (s `B.unsafeIndex` 1) `unsafeShiftL` 0x10)
  .|. (fromIntegral (s `B.unsafeIndex` 2) `unsafeShiftL` 0x08)
  .|.  fromIntegral (s `B.unsafeIndex` 3)




{-# INLINE w64LE #-}
w64LE :: B.ByteString -> Word64
w64LE s =
      (fromIntegral (s `B.unsafeIndex` 7) `unsafeShiftL` 0x38)
  .|. (fromIntegral (s `B.unsafeIndex` 6) `unsafeShiftL` 0x30)
  .|. (fromIntegral (s `B.unsafeIndex` 5) `unsafeShiftL` 0x28)
  .|. (fromIntegral (s `B.unsafeIndex` 4) `unsafeShiftL` 0x20)
  .|. (fromIntegral (s `B.unsafeIndex` 3) `unsafeShiftL` 0x18)
  .|. (fromIntegral (s `B.unsafeIndex` 2) `unsafeShiftL` 0x10)
  .|. (fromIntegral (s `B.unsafeIndex` 1) `unsafeShiftL` 0x08)
  .|.  fromIntegral (s `B.unsafeIndex` 0)

{-# INLINE w64BE #-}
w64BE :: B.ByteString -> Word64
w64BE s =
      (fromIntegral (s `B.unsafeIndex` 0) `unsafeShiftL` 0x38)
  .|. (fromIntegral (s `B.unsafeIndex` 1) `unsafeShiftL` 0x30)
  .|. (fromIntegral (s `B.unsafeIndex` 2) `unsafeShiftL` 0x28)
  .|. (fromIntegral (s `B.unsafeIndex` 3) `unsafeShiftL` 0x20)
  .|. (fromIntegral (s `B.unsafeIndex` 4) `unsafeShiftL` 0x18)
  .|. (fromIntegral (s `B.unsafeIndex` 5) `unsafeShiftL` 0x10)
  .|. (fromIntegral (s `B.unsafeIndex` 6) `unsafeShiftL` 0x08)
  .|.  fromIntegral (s `B.unsafeIndex` 7)
