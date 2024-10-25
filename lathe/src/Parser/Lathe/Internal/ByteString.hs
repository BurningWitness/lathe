{-# LANGUAGE BangPatterns #-}

module Parser.Lathe.Internal.ByteString
  ( toStrictLen
  , toShortLen
  ) where

import           Control.Monad.ST
import           Control.Monad.ST.Unsafe
import           Data.ByteString.Internal as B (ByteString (..), unsafeCreate)
import qualified Data.ByteString.Unsafe as B (unsafeUseAsCStringLen)
import           Data.ByteString.Lazy.Internal as L (ByteString (..))
import           Data.ByteString.Short.Internal (ShortByteString (..))
import           Data.Primitive.ByteArray
import           Foreign.Marshal.Utils (copyBytes)
import           Foreign.Ptr



-- | Convert a lazy ByteString of known size to a strict one.
toStrictLen :: Int -> L.ByteString -> B.ByteString
toStrictLen total xs =
  B.unsafeCreate total $ \dest0 ->

    let go !dest bs =
          case bs of
            L.Chunk b cs -> do
              len <- B.unsafeUseAsCStringLen b $ \(src, len) -> do
                       copyBytes dest (castPtr src) len
                       pure len

              go (plusPtr dest len) cs

            L.Empty      -> pure ()

    in go dest0 xs



-- | Convert a lazy ByteString of known size to a short one.
toShortLen :: Int -> L.ByteString -> ShortByteString
toShortLen total xs =
  runST $ do
    marr <- newByteArray total

    let go !n bs =
          case bs of
            L.Chunk b cs -> do
              len <- unsafeIOToST .
                       B.unsafeUseAsCStringLen b $ \(src, len) -> do
                         unsafeSTToIO $ copyPtrToMutableByteArray marr n src len
                         pure len

              go (n + len) cs

            L.Empty      -> pure ()

    go 0 xs
    (\(ByteArray arr) -> SBS arr) <$> unsafeFreezeByteArray marr
