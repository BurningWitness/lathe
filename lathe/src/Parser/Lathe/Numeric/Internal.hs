{-# LANGUAGE BangPatterns #-}

module Parser.Lathe.Numeric.Internal
  ( Sign (..)
  ) where



-- | Whether the number is positive or negative.
data Sign = Plus
          | Minus
            deriving Show
