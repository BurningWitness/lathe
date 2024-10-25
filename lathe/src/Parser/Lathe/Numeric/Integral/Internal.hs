{-# LANGUAGE BangPatterns
           , UnboxedTuples #-}

module Parser.Lathe.Numeric.Integral.Internal
  ( u8
  , u16
  , u32
  , u64

  , i8
  , i16
  , i32
  , i64
  ) where



u8, u16, u32, u64 :: Num a => (Int, a, a)
u8  = ( 2,                  25, 5)
u16 = ( 4,                6553, 5)
u32 = ( 9,           429496729, 5)
u64 = (19, 1844674407370955161, 5)

i8, i16, i32, i64 :: Num a => (Int, a, a)
i8  = ( 2,                  12, 7)
i16 = ( 4,                3276, 7)
i32 = ( 9,           214748364, 7)
i64 = (18,  922337203685477580, 7)
