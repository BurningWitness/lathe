{-# LANGUAGE CPP #-}

{-| Copy of @Data.ByteString.Builder.Prim.Internal.caseWordSize_32_64@.
 -}

module Parser.Lathe.Internal.Bitness
  ( caseWordSize_32_64
  ) where

#include "MachDeps.h"

-- | Select an implementation depending on bitness.
-- Throw a compile time error if bitness is neither 32 nor 64.
{-# INLINE caseWordSize_32_64 #-}
caseWordSize_32_64
  :: a -- Value for 32-bit architecture
  -> a -- Value for 64-bit architecture
  -> a
#if WORD_SIZE_IN_BITS == 32
caseWordSize_32_64 = const
#endif
#if WORD_SIZE_IN_BITS == 64
caseWordSize_32_64 = const id
#endif
