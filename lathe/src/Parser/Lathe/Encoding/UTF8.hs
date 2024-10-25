{-# LANGUAGE BangPatterns
           , DataKinds
           , KindSignatures
           , UnboxedTuples #-}

{- | Functions for parsing UTF-8.

     Parsing of UTF-8 code points is broken down into two steps
     to allow for full or partial validation of characters as opposed to full
     'Char' conversions.

     The following is an example of parsing a UTF-8 code point into a character:

 @
 data Error = EoF | Malformed | Surrogate | …

 charUtf8 :: t'Parser' Error Char
 charUtf8 = do
   u \<- 'unitUtf8' (\\_ -> Malformed) EoF
   case u of
     'UTF8_1' u1 -> pure $! 'fromUtf8' u1
     'UTF8_2' u2 -> 'contUtf8_2' Malformed EoF u2
     'UTF8_3' u3 -> do
       p \<- 'contUtf8_3' (\\_ -> Malformed) EoF u3
       if 'isSurrogate' p
         then err Surrogate
         else pure $! fromUtf8 p

     'UTF8_4' u4 -> 'contUtf8_4' (\\_ -> Malformed) EoF u4
 @
 -}

module Parser.Lathe.Encoding.UTF8
  ( -- * Byte-order mark
    utf8BOM

    -- * UTF-8
  , UTF8Unit (..)
  , UTF8Point (..)
  , fromUtf8
  , isSurrogate

    -- ** First code unit
  , UTF8Branch (..)
  , UTF8Error_1 (..)
  , unitUtf8

    -- ** Second code unit
  , contUtf8_2
  , skipUtf8_2

    -- ** Third code unit
  , UTF8Error_3 (..)
  , contUtf8_3
  , skipUtf8_3

    -- ** Fourth code unit
  , UTF8Error_4 (..)
  , contUtf8_4
  , skipUtf8_4
  ) where

import           Parser.Lathe.Internal

import           Data.Bits
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Unsafe as B
import           Data.Word
import           GHC.Base (unsafeChr)
import           GHC.TypeNats (Nat)



{-# INLINE utf8BOM #-}
-- | Consume 3 bytes that represent a UTF-8 byte-order mark.
utf8BOM
  :: e           -- ^ Malformed.
  -> e           -- ^ Reached end.
  -> Parser e ()
utf8BOM malformed = unsafeRead 3 (convUTF8BOM malformed)

convUTF8BOM :: e -> ByteString -> (# Res e () #)
convUTF8BOM e = \b ->
  let w0 = B.unsafeIndex b 0
      w1 = B.unsafeIndex b 1
      w2 = B.unsafeIndex b 2

  in if w0 == 0xEF && w1 == 0xBB && w2 == 0xBF
       then (# Yes () #)
       else (# No e #)



-- | Errors that may be encountered when parsing the first UTF-8 code unit.
data UTF8Error_1 = -- | (byte 1 is @10xxxxxx@) Continuation code unit.
                   UTF8Continuation

                   -- | (byte 1 is @110__0000__x@) Overlong 2-unit code point.
                 | UTF8Overlong_2

                   -- | (byte 1 is @11110__1__xx@, @xx@ is not @00@)
                   --   Invalid 4-unit code point.
                 | UTF8Overflow_1

                   -- | (byte 1 is @11111xxx@)
                   --   Invalid first code unit.
                 | UTF8Invalid
                   deriving (Show, Eq)
 

-- | Errors that may be encountered when parsing the third UTF-8 code unit.
data UTF8Error_3 = -- | (byte 2 is not @10xxxxxx@) Non-continuation second code unit.
                   UTF8Incomplete_2_3

                   -- | (byte 1 is @1110__0000__@, byte 2 is @10__0__xxxxx@)
                   --   Overlong 3-unit code point.
                 | UTF8Overlong_3

                   -- | (byte 3 is not @10xxxxxx@) Non-continuation third code unit.
                 | UTF8Incomplete_3_3
                   deriving (Show, Eq)


-- | Errors that may be encountered when parsing the fourth UTF-8 code unit.
data UTF8Error_4 = -- | (byte 2 is not @10xxxxxx@) Non-continuation second code unit.
                   UTF8Incomplete_2_4

                 | -- | (byte 1 is @11110__000__@, byte 2 is @10__00__xxxx@)
                   --   Overlong 4-unit code point.
                   UTF8Overlong_4

                   -- | (byte 1 is @11110__100__@, byte 2 is not @10__00__xxxx@)
                   --   Invalid 4-unit code point.
                 | UTF8Overflow_2

                   -- | (byte 3 is not @10xxxxxx@) Non-continuation third code unit.
                 | UTF8Incomplete_3_4

                   -- | (byte 4 is not @10xxxxxx@) Non-continuation fourth code unit.
                 | UTF8Incomplete_4_4
                   deriving (Show, Eq)



-- | First UTF-8 code unit.
--   @n@ represents the total number of code units in this code point.
newtype UTF8Unit (n :: Nat) = UTF8Unit Word8

-- | A Unicode code point.
newtype UTF8Point (n :: Nat) = UTF8Point Word32

-- | Convert a code point into a 'Char'.
fromUtf8 :: UTF8Point n -> Char
fromUtf8 (UTF8Point w0) = unsafeChr $ fromIntegral w0

-- | Check whether a 3-unit code point lies in the surrogate range (@U+D800@ to @U+DFFF@).
isSurrogate :: UTF8Point 3 -> Bool
isSurrogate (UTF8Point w) = (w .&. 0xFFFFF800) == 0x0000D800



-- | UTF-8 branching based on the first code unit.
data UTF8Branch = UTF8_1 {-# UNPACK #-} !(UTF8Point 1)
                | UTF8_2 {-# UNPACK #-} !(UTF8Unit 2)
                | UTF8_3 {-# UNPACK #-} !(UTF8Unit 3)
                | UTF8_4 {-# UNPACK #-} !(UTF8Unit 4)

{-# INLINE unitUtf8 #-}
-- | Consume 1 byte that represents the first code unit of a UTF-8 code point.
unitUtf8
  :: (UTF8Error_1 -> e)  -- ^ Malformed
  -> e                   -- ^ Reached end.
  -> Parser e UTF8Branch
unitUtf8 malformed end = do
  w0 <- word8 end
  go w0
  where
    go w0
      | (w0 .&. 0x80) == 0 = pure $! UTF8_1 (UTF8Point (fromIntegral w0))
      | (w0 .&. 0x40) == 0 = err $ malformed UTF8Continuation
      | (w0 .&. 0x20) == 0 = if (w0 .&. 0x1F) < 0x02
                               then err $ malformed UTF8Overlong_2
                               else pure $! UTF8_2 (UTF8Unit w0)

      | (w0 .&. 0x10) == 0 = pure $! UTF8_3 (UTF8Unit w0)
      | (w0 .&. 0x08) == 0 = if (w0 .&. 0x07) > 0x04
                               then err $ malformed UTF8Overflow_1
                               else pure $! UTF8_4 (UTF8Unit w0)

      | otherwise          = err $ malformed UTF8Invalid



checkIncomplete :: Word8 -> Bool
checkIncomplete w1 = (w1 .&. 0xC0) /= 0x80

{-# INLINE contUtf8_2 #-}
-- | Consume 1 byte that represents the second code unit of a 2-unit UTF-8 code point
--   and convert the two units into a 'Char'.
contUtf8_2
  :: e             -- ^ (byte 2 is not @10xxxxxx@) Non-continuation second code unit.
  -> e             -- ^ Reached end.
  -> UTF8Unit 2
  -> Parser e Char
contUtf8_2 incomplete end (UTF8Unit w0) = do
  w1 <- word8 end
  if checkIncomplete w1
    then err incomplete
    else pure $! charUtf8_2 w0 w1

{-# INLINE skipUtf8_2 #-}
-- | Consume 1 byte that represents the second code unit of a 2-unit UTF-8 code point.
skipUtf8_2
  :: e           -- ^ (byte 2 is not @10xxxxxx@) Non-continuation second code unit.
  -> e           -- ^ Reached end.
  -> UTF8Unit 2
  -> Parser e ()
skipUtf8_2 incomplete end (UTF8Unit _) = do
  w1 <- word8 end
  if checkIncomplete w1
    then err incomplete
    else pure ()

charUtf8_2 :: Word8 -> Word8 -> Char
charUtf8_2 w0 w1 =
  unsafeChr $ unsafeShiftL (fromIntegral w0 .&. 0x1F) 6
            +              (fromIntegral w1 .&. 0x3F)



{-# INLINE contUtf8_3 #-}
-- | Consume 2 bytes that represent the second and third code units of
--   a 3-unit UTF-8 code point and convert the three units into a code point.
contUtf8_3
  :: (UTF8Error_3 -> e)     -- ^ Malformed.
  -> e                      -- ^ Reached end.
  -> UTF8Unit 3
  -> Parser e (UTF8Point 3)
contUtf8_3 e end (UTF8Unit w0) = unsafeRead 2 (convUTF8_3 e w0) end

{-# INLINE skipUtf8_3 #-}
-- | Consume 2 bytes that represent the second and third code units of
--   a 3-unit UTF-8 code point.
skipUtf8_3
  :: (UTF8Error_3 -> e) -- ^ Malformed.
  -> e                  -- ^ Reached end.
  -> UTF8Unit 3
  -> Parser e Bool      -- ^ 'True' if the skipped code point lies in
                        -- the surrogate range (@U+D800@ to @U+DFFF@).
skipUtf8_3 e end (UTF8Unit w0) = unsafeRead 2 (convUTF8_3_ e w0) end



checkOverlong3 :: Word8 -> Word8 -> Bool
checkOverlong3 w0 w1 = (w0 .&. 0x0F) == 0 && (w1 .&. 0x20) == 0

checkSurrogate :: Word8 -> Word8 -> Bool
checkSurrogate w0 w1 = (w0 .&. 0x0F) == 0x0D && (w1 .&. 0x20) /= 0

convUTF8_3
  :: (UTF8Error_3 -> e)
  -> Word8 -> ByteString -> (# Res e (UTF8Point 3) #)
convUTF8_3 malformed w0 = \b ->
  let w1 = B.unsafeIndex b 0
      w2 = B.unsafeIndex b 1

  in if checkIncomplete w1
       then (# No (malformed UTF8Incomplete_2_3) #)
       else
         if checkOverlong3 w0 w1
           then (# No (malformed UTF8Overlong_3) #)
           else
             if checkIncomplete w2
               then (# No (malformed UTF8Incomplete_3_3) #)
               else
                 let !r = charUtf8_3 w0 w1 w2
                 in (# Yes (UTF8Point r) #)

convUTF8_3_
  :: (UTF8Error_3 -> e)
  -> Word8 -> ByteString -> (# Res e Bool #)
convUTF8_3_ malformed w0 = \b ->
  let w1 = B.unsafeIndex b 0
      w2 = B.unsafeIndex b 1

  in if checkIncomplete w1
       then (# No (malformed UTF8Incomplete_2_3) #)
       else
         if checkOverlong3 w0 w1
           then (# No (malformed UTF8Overlong_3) #)
           else
             if checkIncomplete w2
               then (# No (malformed UTF8Incomplete_3_3) #)
               else
                 let !r = checkSurrogate w0 w1
                 in (# Yes r #)

charUtf8_3 :: Word8 -> Word8 -> Word8 -> Word32
charUtf8_3 w0 w1 w2 =
    unsafeShiftL (fromIntegral w0 .&. 0x0F) 12
  + unsafeShiftL (fromIntegral w1 .&. 0x3F) 6
  +              (fromIntegral w2 .&. 0x3F)



{-# INLINE contUtf8_4 #-}
-- | Consume 3 bytes that represent the second to fourth code units of
--   a 4-unit UTF-8 code point and convert the four units into a 'Char'.
contUtf8_4
  :: (UTF8Error_4 -> e) -- ^ Malformed.
  -> e                  -- ^ Reached end.
  -> UTF8Unit 4
  -> Parser e Char
contUtf8_4 e end (UTF8Unit w0) = unsafeRead 3 (convUTF8_4 e w0) end

{-# INLINE skipUtf8_4 #-}
-- | Consume 3 bytes that represent the second to fourth code units of
--   a 4-unit UTF-8 code point.
skipUtf8_4
  :: (UTF8Error_4 -> e) -- ^ Malformed.
  -> e                  -- ^ Reached end.
  -> UTF8Unit 4
  -> Parser e ()
skipUtf8_4 e end (UTF8Unit w0) = unsafeRead 3 (convUTF8_4_ e w0) end



checkOverlong4 :: Word8 -> Word8 -> Bool
checkOverlong4 w0 w1 = (w0 .&. 0x07) == 0x00 && (w1 .&. 0x30) == 0x00

checkOverflow4 :: Word8 -> Word8 -> Bool
checkOverflow4 w0 w1 = (w0 .&. 0x07) == 0x04 && (w1 .&. 0x30) /= 0x00

convUTF8_4
  :: (UTF8Error_4 -> e)
  -> Word8 -> ByteString -> (# Res e Char #)
convUTF8_4 malformed w0 = \b ->
  let w1 = B.unsafeIndex b 0
      w2 = B.unsafeIndex b 1
      w3 = B.unsafeIndex b 2

  in if checkIncomplete w1
       then (# No (malformed UTF8Incomplete_2_4) #)
       else
         if checkOverlong4 w0 w1
           then (# No (malformed UTF8Overlong_4) #)
           else
             if checkOverflow4 w0 w1
               then (# No (malformed UTF8Overflow_2) #)
               else
                 if checkIncomplete w2
                   then (# No (malformed UTF8Incomplete_3_4) #)
                   else
                     if checkIncomplete w3
                       then (# No (malformed UTF8Incomplete_4_4) #)
                       else
                         let !r = charUtf8_4 w0 w1 w2 w3
                         in (# Yes r #)

convUTF8_4_
  :: (UTF8Error_4 -> e)
  -> Word8 -> ByteString -> (# Res e () #)
convUTF8_4_ malformed w0 = \b ->
  let w1 = B.unsafeIndex b 0
      w2 = B.unsafeIndex b 1
      w3 = B.unsafeIndex b 2

  in if checkIncomplete w1
       then (# No (malformed UTF8Incomplete_2_4) #)
       else
         if checkOverlong4 w0 w1
           then (# No (malformed UTF8Overlong_4) #)
           else
             if checkOverflow4 w0 w1
               then (# No (malformed UTF8Overflow_2) #)
               else
                 if checkIncomplete w2
                   then (# No (malformed UTF8Incomplete_3_4) #)
                   else
                     if checkIncomplete w3
                       then (# No (malformed UTF8Incomplete_4_4) #)
                       else (# Yes () #)

charUtf8_4 :: Word8 -> Word8 -> Word8 -> Word8 -> Char
charUtf8_4 w0 w1 w2 w3 =
  unsafeChr $ unsafeShiftL (fromIntegral w0 .&. 0x07) 18
            + unsafeShiftL (fromIntegral w1 .&. 0x3F) 12
            + unsafeShiftL (fromIntegral w2 .&. 0x3F) 6
            +              (fromIntegral w3 .&. 0x3F)
