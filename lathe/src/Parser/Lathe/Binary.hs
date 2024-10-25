{-# LANGUAGE BangPatterns
           , UnboxedTuples #-}

{- | Functions for parsing binary data.
 -}

module Parser.Lathe.Binary
  ( int8
  , word8

    -- * Big-endian
  , int16BE
  , int32BE
  , int64BE
  , word16BE
  , word32BE
  , word64BE
  , floatBE
  , doubleBE

    -- * Little-endian
  , int16LE
  , int32LE
  , int64LE
  , word16LE
  , word32LE
  , word64LE
  , floatLE
  , doubleLE
  ) where

import qualified Parser.Lathe.Binary.Internal as X
import           Parser.Lathe.Internal

import           Data.ByteString (ByteString)
import           Data.Int
import           Data.Word
import           GHC.Float (castWord32ToFloat, castWord64ToDouble)



{-# INLINE int16LE #-}
-- | Consume 2 bytes into a little-endian 'Int16'.
int16LE :: end -> Parser end Int16
int16LE = unsafeRead 2 i16LE

{-# INLINE int16BE #-}
-- | Consume 2 bytes into a big-endian 'Int16'.
int16BE :: end -> Parser end Int16
int16BE = unsafeRead 2 i16BE

{-# INLINE word16LE #-}
-- | Consume 2 bytes into a little-endian 'Word16'.
word16LE :: end -> Parser end Word16
word16LE = unsafeRead 2 w16LE

{-# INLINE word16BE #-}
-- | Consume 2 bytes into a big-endian 'Word16'.
word16BE :: end -> Parser end Word16
word16BE = unsafeRead 2 w16BE


i16LE :: ByteString -> (# Res e Int16 #)
i16LE = \b ->
  let !r = fromIntegral (X.w16LE b)
  in (# Yes r #)

i16BE :: ByteString -> (# Res e Int16 #)
i16BE = \b ->
  let !r = fromIntegral (X.w16BE b)
  in (# Yes r #)

w16LE :: ByteString -> (# Res e Word16 #)
w16LE = \b ->
  let !r = X.w16LE b
  in (# Yes r #)

w16BE :: ByteString -> (# Res e Word16 #)
w16BE = \b ->
  let !r = X.w16BE b
  in (# Yes r #)



{-# INLINE int32LE #-}
-- | Consume 4 bytes into a little-endian 'Int32'.
int32LE :: end -> Parser end Int32
int32LE = unsafeRead 4 i32LE

{-# INLINE int32BE #-}
-- | Consume 4 bytes into a big-endian 'Int32'.
int32BE :: end -> Parser end Int32
int32BE = unsafeRead 4 i32BE

{-# INLINE word32LE #-}
-- | Consume 4 bytes into a little-endian 'Word32'.
word32LE :: end -> Parser end Word32
word32LE = unsafeRead 4 w32LE

{-# INLINE word32BE #-}
-- | Consume 4 bytes into a big-endian 'Word32'.
word32BE :: end -> Parser end Word32
word32BE = unsafeRead 4 w32BE

{-# INLINE floatLE #-}
-- | Consume 4 bytes into a little-endian 'Float'.
floatLE :: end -> Parser end Float
floatLE = unsafeRead 4 f32LE

{-# INLINE floatBE #-}
-- | Consume 4 bytes into a big-endian 'Float'.
floatBE :: end -> Parser end Float
floatBE = unsafeRead 4 f32BE


i32LE :: ByteString -> (# Res e Int32 #)
i32LE = \b ->
  let !r = fromIntegral (X.w32LE b)
  in (# Yes r #)

i32BE :: ByteString -> (# Res e Int32 #)
i32BE = \b ->
  let !r = fromIntegral (X.w32BE b)
  in (# Yes r #)

w32LE :: ByteString -> (# Res e Word32 #)
w32LE = \b ->
  let !r = X.w32LE b
  in (# Yes r #)

w32BE :: ByteString -> (# Res e Word32 #)
w32BE = \b ->
  let !r = X.w32BE b
  in (# Yes r #)

f32LE :: ByteString -> (# Res e Float #)
f32LE = \b ->
  let !r = castWord32ToFloat (X.w32LE b)
  in (# Yes r #)

f32BE :: ByteString -> (# Res e Float #)
f32BE = \b ->
  let !r = castWord32ToFloat (X.w32BE b)
  in (# Yes r #)



{-# INLINE int64LE #-} 
-- | Consume 8 bytes into a little-endian 'Int64'.
int64LE :: end -> Parser end Int64
int64LE = unsafeRead 8 i64LE

{-# INLINE int64BE #-}
-- | Consume 8 bytes into a big-endian 'Int64'.
int64BE :: end -> Parser end Int64
int64BE = unsafeRead 8 i64BE

{-# INLINE word64LE #-}
-- | Consume 8 bytes into a little-endian 'Word64'.
word64LE :: end -> Parser end Word64
word64LE = unsafeRead 8 w64LE

{-# INLINE word64BE #-}
-- | Consume 8 bytes into a big-endian 'Word64'.
word64BE :: end -> Parser end Word64
word64BE = unsafeRead 8 w64BE

{-# INLINE doubleLE #-}
-- | Consume 8 bytes into a little-endian 'Double'.
doubleLE :: end -> Parser end Double
doubleLE = unsafeRead 8 f64LE

{-# INLINE doubleBE #-}
-- | Consume 8 bytes into a big-endian 'Double'.
doubleBE :: end -> Parser end Double
doubleBE = unsafeRead 8 f64BE


i64LE :: ByteString -> (# Res e Int64 #)
i64LE = \b ->
  let !r = fromIntegral (X.w64LE b)
  in (# Yes r #)

i64BE :: ByteString -> (# Res e Int64 #)
i64BE = \b ->
  let !r = fromIntegral (X.w64BE b)
  in (# Yes r #)

w64LE :: ByteString -> (# Res e Word64 #)
w64LE = \b ->
  let !r = fromIntegral (X.w64LE b)
  in (# Yes r #)

w64BE :: ByteString -> (# Res e Word64 #)
w64BE = \b ->
  let !r = fromIntegral (X.w64BE b)
  in (# Yes r #)

f64LE :: ByteString -> (# Res e Double #)
f64LE = \b ->
  let !r = castWord64ToDouble (X.w64LE b)
  in (# Yes r #)

f64BE :: ByteString -> (# Res e Double #)
f64BE = \b ->
  let !r = castWord64ToDouble (X.w64BE b)
  in (# Yes r #)
