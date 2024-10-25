{-# LANGUAGE BangPatterns
           , DataKinds
           , KindSignatures
           , UnboxedTuples #-}

{- | Functions for parsing UTF-16, both little-endian and big-endian.

     Parsing of UTF-16 code points is broken down into two steps
     to allow for full or partial validation of characters as opposed to full
     'Char' conversions.

     The following is an example of parsing a little-endian UTF-16 code point
     into a character:

 @
 data Error = EoF | Malformed | …

 charUtf16LE :: t'Parser' Error Char
 charUtf16LE = do
   u <- 'unitUtf16LE' Malformed EoF
   case u of
     'UTF16_1' u1 -> pure $! 'fromUtf16' u1
     'UTF16_2' u2 -> 'contUtf16LE_2' Malformed EoF u2
 @

 -}

module Parser.Lathe.Encoding.UTF16
  ( -- * Byte-order mark
    utf16BOM

    -- * UTF-16
  , UTF16Unit (..)
  , UTF16Point (..)
  , fromUtf16

    -- ** Parsers
  , UTF16Branch (..)
  , unitUtf16BE
  , unitUtf16LE

    -- *** Continue
  , contUtf16BE_2
  , contUtf16LE_2

    -- *** Skip
  , skipUtf16BE_2
  , skipUtf16LE_2
  ) where

import           Parser.Lathe.Binary.Internal
import           Parser.Lathe.Internal

import           Data.Bits
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Unsafe as B
import           Data.Word
import           GHC.Base (unsafeChr)
import           GHC.ByteOrder
import           GHC.TypeNats (Nat)



{-# INLINE utf16BOM #-}
-- | Consume 2 bytes that represent a UTF-16 byte-order mark and return
--   the corresponding 'ByteOrder'.
utf16BOM
  :: e                  -- ^ Malformed.
  -> e                  -- ^ Reached end.
  -> Parser e ByteOrder
utf16BOM malformed = unsafeRead 2 (convUTF16BOM malformed)

convUTF16BOM :: e -> ByteString -> (# Res e ByteOrder #)
convUTF16BOM e = \b ->
  let w0 = B.unsafeIndex b 0
      w1 = B.unsafeIndex b 1

  in go w0 w1
  where
    go w0 w1
      | w0 == 0xFE, w1 == 0xFF = (# Yes BigEndian #)
      | w0 == 0xFF, w1 == 0xFE = (# Yes LittleEndian #)
      | otherwise              = (# No e #)



-- | Errors that may be encountered when parsing a UTF-16 character.
data UTF16Error = -- | First surrogate code unit is not a high one.
                  UTF16BadHigh {-# UNPACK #-} !Word16

                  -- | Second surrogate code unit is not a low one.
                | UTF16BadLow {-# UNPACK #-} !Word16 {-# UNPACK #-} !Word16
                  deriving Show



-- | First UTF-16 code unit.
--   @n@ represents the total number of code units in this code point.
newtype UTF16Unit (byteOrder :: ByteOrder) (n :: Nat) = UTF16Unit Word16

-- | UTF-16 code point.
newtype UTF16Point (n :: Nat) = UTF16Point Word32



-- | Convert a UTF-8 code point into a 'Char'.
fromUtf16 :: UTF16Point n -> Char
fromUtf16 (UTF16Point u0) = unsafeChr $ fromIntegral u0



{-# INLINE castUTF16 #-}
castUTF16 :: Word16 -> Word16 -> (# Res e Char #)
castUTF16 u0 u1 =
  let !c = unsafeChr . (fromIntegral :: Word32 -> Int) $
                           0x000010000
                         + (fromIntegral (u0 - 0xD800) `unsafeShiftL` 10)
                         +  fromIntegral (u1 - 0xDC00)
  in (# Yes c #)



-- | UTF-16 branching based on the first code unit.
data UTF16Branch (byteOrder :: ByteOrder) = UTF16_1 {-# UNPACK #-} !(UTF16Point 1)
                                          | UTF16_2 {-# UNPACK #-} !(UTF16Unit byteOrder 2)


{-# INLINE unitUtf16BE #-}
-- | Consume 2 bytes that represent the first code unit of a
--   big-endian UTF-16 code point.
unitUtf16BE
  :: e                                 -- ^ Code unit is a low surrogate.
  -> e                                 -- ^ Reached end.
  -> Parser e (UTF16Branch 'BigEndian)
unitUtf16BE e = unsafeRead 2 (u16BE e)

u16BE :: e -> ByteString -> (# Res e (UTF16Branch 'BigEndian) #)
u16BE e = \b ->
  let u0 = w16BE b
  in if (u0 .&. 0xF800) == 0xD800
       then if u0 >= 0xDC00
              then (# No e #)
              else let !r = UTF16_2 (UTF16Unit u0)
                   in (# Yes r #)

       else let !r = UTF16_1 (UTF16Point (fromIntegral u0))
            in (# Yes r #)



{-# INLINE contUtf16BE_2 #-}
-- | Consume 2 bytes that represent the second code unit of
--   a 2-unit big-endian UTF-16 code point and convert the two units into a 'Char'.
contUtf16BE_2
  :: e                      -- ^ Code unit is not a low surrogate.
  -> e                      -- ^ Reached end.
  -> UTF16Unit 'BigEndian 2
  -> Parser e Char
contUtf16BE_2 u16err end (UTF16Unit u0) = unsafeRead 2 (convUTF16BE_2 u16err u0) end

{-# INLINE skipUtf16BE_2 #-}
-- | Consume 2 bytes that represent the second code unit of
--   a 2-unit big-endian UTF-16 code point.
skipUtf16BE_2
  :: e                      -- ^ Code unit is not a low surrogate.
  -> e                      -- ^ Reached end.
  -> UTF16Unit 'BigEndian 2
  -> Parser e ()
skipUtf16BE_2 u16err end (UTF16Unit u0) = unsafeRead 2 (convUTF16BE_2_ u16err u0) end

convUTF16BE_2 :: e -> Word16 -> ByteString -> (# Res e Char #)
convUTF16BE_2 e u = convUTF16BE_2__ castUTF16 e u

convUTF16BE_2_ :: e -> Word16 -> ByteString -> (# Res e () #)
convUTF16BE_2_ e u = convUTF16BE_2__ (\_ _ -> (# Yes () #)) e u

{-# INLINE convUTF16BE_2__ #-}
convUTF16BE_2__
  :: (Word16 -> Word16 -> (# Res e a #))
  -> e -> Word16 -> ByteString -> (# Res e a #)
convUTF16BE_2__ f e u0 = \b ->
  let u1 = w16BE b
  in if (u1 .&. 0xFC00) == 0xDC00
       then f u0 u1
       else (# No e #)



{-# INLINE unitUtf16LE #-}
-- | Consume 2 bytes that represent the first code unit of a
--   little-endian UTF-16 code point.
unitUtf16LE
  :: e                                    -- ^ Code unit is a low surrogate.
  -> e                                    -- ^ Reached end.
  -> Parser e (UTF16Branch 'LittleEndian)
unitUtf16LE e = unsafeRead 2 (u16LE e)

u16LE :: e -> ByteString -> (# Res e (UTF16Branch 'LittleEndian) #)
u16LE e = \b ->
  let u0 = w16LE b
  in if (u0 .&. 0xF800) == 0xD800
       then if u0 >= 0xDC00
              then (# No e #)
              else let !r = UTF16_2 (UTF16Unit u0)
                   in (# Yes r #)

       else let !r = UTF16_1 (UTF16Point (fromIntegral u0))
            in (# Yes r #)


{-# INLINE contUtf16LE_2 #-}
-- | Consume 2 bytes that represent the second code unit of
--   a 2-unit little-endian UTF-16 code point and convert the two units into a 'Char'.
contUtf16LE_2
  :: e                         -- ^ Code unit is not a low surrogate.
  -> e                         -- ^ Reached end.
  -> UTF16Unit 'LittleEndian 2
  -> Parser e Char
contUtf16LE_2 u16err end (UTF16Unit u0) = unsafeRead 2 (convUTF16LE_2 u16err u0) end

{-# INLINE skipUtf16LE_2 #-}
-- | Consume 2 bytes that represent the second code unit of
--   a 2-unit little-endian UTF-16 code point.
skipUtf16LE_2
  :: e                         -- ^ Code unit is not a low surrogate.
  -> e                         -- ^ Reached end.
  -> UTF16Unit 'LittleEndian 2
  -> Parser e ()
skipUtf16LE_2 u16err end (UTF16Unit u0) = unsafeRead 2 (convUTF16LE_2_ u16err u0) end

convUTF16LE_2 :: e -> Word16 -> ByteString -> (# Res e Char #)
convUTF16LE_2 e u = convUTF16LE_2__ castUTF16 e u

convUTF16LE_2_ :: e -> Word16 -> ByteString -> (# Res e () #)
convUTF16LE_2_ e u = convUTF16LE_2__ (\_ _ -> (# Yes () #)) e u

{-# INLINE convUTF16LE_2__ #-}
convUTF16LE_2__
  :: (Word16 -> Word16 -> (# Res e a #))
  -> e -> Word16 -> ByteString -> (# Res e a #)
convUTF16LE_2__ f e u0 = \b ->
  let u1 = w16LE b
  in if (u1 .&. 0xFC00) == 0xDC00
       then f u0 u1
       else (# No e #)
