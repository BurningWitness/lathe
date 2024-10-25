{- | Pure incremental byte parser.

     == Laziness

     Parser functions that return concrete types in this library fully evaluate
     their results before returning unless their documentation specifies otherwise.
 -}

module Parser.Lathe
  ( -- * Itself
    Parser

    -- * Run
    -- *** Immediate
  , ByteOffset
  , More (..)
  , Scrap (..)
  , parse

    -- *** Incremental
  , Blank
  , Result (..)
  , Resupply (..)
  , Partial (..)
  , prepare
  , draw
  , scrap

    -- * Manage
  , bytesRead
  , atEnd

    -- * Err
  , err
  , mapError
  , catch
  , match

    -- * Parse
    -- ** No output
  , skip1
  , skip
  , skipNul
  , skipUntil

    -- | === Optional
  , skipEndOr1
  , skipEndOr
  , skipUntilEndOr

    -- ** Strict ByteString
  , byteString
  , byteStringNul
  , byteStringUntil

    -- ** Short ByteString
  , shortByteString
  , shortByteStringNul
  , shortByteStringUntil

    -- ** Lazy ByteString
  , lazyByteString
  , lazyByteStringNul
  , lazyByteStringUntil
  , lazyByteStringRest
  ) where

import           Parser.Lathe.Internal
