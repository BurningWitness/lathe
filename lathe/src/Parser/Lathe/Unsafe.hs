{-# OPTIONS_HADDOCK not-home #-}

{- | Parser internals, helper operations and unsafe functions.

     == Implementation

     Parser keeps a non-empty cons-list of known chunks
     (strict 'B.ByteString' plus a lazy 'L.ByteString') and a snoc-list
     of all chunks it may need when rolling back ( t'Rollback').

     Chunks themselves are never modified, instead a 'ChunkOffset' is used to
     keep track of position within the current chunk.
     'TotalOffset' is a sum of lengths of all chunks before the current one.

     A request for more input ('Resupply') only occurs when parser reaches the
     the end of the current chunk, no more chunks are known, v'More' input is
     available and more parsing is necessary. Current chunk is then replaced with the
     new one and both offsets are adjusted accordingly.

     A rollback may only occur if a function above requested so ('Policy').
     Any new chunks received while in the 'Keep' state are
     additionally added to the t'Rollback', which is then managed by the function above
     when it gains control again.
 -}

module Parser.Lathe.Unsafe
  ( -- * Itself
    TotalOffset
  , ChunkOffset
  , More (..)
  , Rollback (..)
  , Policy (..)

  , Core
  , Resupply (..)
  , Res (..)
  , Dec (..)
  , Parser (..)

  , Blank (..)

    -- * Run
    -- ** Immediate
  , UnexpectedPartial (..)

    -- * Parse
    -- *** No output
  , unsafeSkip

    -- | === Optional
  , unsafeSkipEndOr

    -- *** Strict ByteString
  , unsafeByteString
  , unsafeRead

    -- *** Short ByteString
  , unsafeShortByteString

    -- *** Lazy ByteString
  , unsafeLazyByteString
  ) where

import           Parser.Lathe.Internal
