{-# LANGUAGE BangPatterns
           , NumericUnderscores
           , UnboxedTuples #-}

{-| Functions for parsing common time formats.
 -}

module Parser.Lathe.Time
  ( -- * Calendar
    year
  , quarter
    
    -- ** Month
  , month
  , monthDate

    -- ** Week
  , weekDate

    -- ** Ordinal
  , ordinalDate

    -- * Time of day
  , timeOfDay

    -- ** Local
  , localTime

    -- ** Zoned
  , timeZone
  , zonedTime
  ) where

import           Parser.Lathe
import           Parser.Lathe.Binary
import           Parser.Lathe.Numeric.Fractional
import           Parser.Lathe.Radix
import           Parser.Lathe.Unsafe

import           Data.Bits
import           Data.ByteString as B (ByteString)
import qualified Data.ByteString.Unsafe as B
import           Data.Fixed
import           Data.Time.Calendar
import           Data.Time.Calendar.Month
import           Data.Time.Calendar.OrdinalDate
import           Data.Time.Calendar.Quarter
import           Data.Time.Calendar.WeekDate
import           Data.Time.LocalTime



-- | Consume four decimal digits that represent an RFC 3339 full year.
year
  :: e             -- ^ Malformed.
  -> e             -- ^ Reached end.
  -> Parser e Year
year malformed end = unsafeRead 4 conv end
  where
    conv b
      | Just i0 <- dec (B.unsafeIndex b 0), let  y0 =           (fromIntegral i0)
      , Just i1 <- dec (B.unsafeIndex b 1), let !y1 = y0 * 10 + (fromIntegral i1)
      , Just i2 <- dec (B.unsafeIndex b 2), let !y2 = y1 * 10 + (fromIntegral i2)
      , Just i3 <- dec (B.unsafeIndex b 3), let !y3 = y2 * 10 + (fromIntegral i3)
          = let !yyyy = fromIntegral (y3 :: Int)
            in (# Yes yyyy #)

      | otherwise = (# No malformed #)



-- | Consume 7 bytes that represent a year quarter, in the @yyyy-Qq@ format.
--
--   Note that year quarters are not defined by ISO 8601, this is an informal convention.
quarter
  :: e                -- ^ Out of bounds.
  -> e                -- ^ Malformed.
  -> e                -- ^ Reached end.
  -> Parser e Quarter
quarter oob malformed end = unsafeRead 7 conv end
  where
    conv b
      | Just i0 <- dec (B.unsafeIndex b 0), let  y0 =            fromIntegral i0
      , Just i1 <- dec (B.unsafeIndex b 1), let !y1 = y0 * 10 + (fromIntegral i1)
      , Just i2 <- dec (B.unsafeIndex b 2), let !y2 = y1 * 10 + (fromIntegral i2)
      , Just i3 <- dec (B.unsafeIndex b 3), let !y3 = y2 * 10 + (fromIntegral i3)
      , 0x2D ==         B.unsafeIndex b 4
      , 0x51 ==         B.unsafeIndex b 5
      , Just q0 <- dec (B.unsafeIndex b 6)
          = let rq = case q0 of
                       1 -> Just Q1
                       2 -> Just Q2
                       3 -> Just Q3
                       4 -> Just Q4
                       _ -> Nothing

            in case rq of
                 Nothing -> (# No oob #)
                 Just q  -> let !yyyy = fromIntegral (y3 :: Int)
                            in (# Yes (YearQuarter yyyy q) #)

      | otherwise = (# No malformed #)



-- | Consume 7 bytes that represent an RFC 3339 month, in the @yyyy-mm@ format.
month
  :: e              -- ^ Out of bounds
  -> e              -- ^ Malformed.
  -> e              -- ^ Reached end.
  -> Parser e Month
month oob malformed end = unsafeRead 7 conv end
  where
    conv b
      | Just i0 <- dec (B.unsafeIndex b 0), let  y0 =            fromIntegral i0
      , Just i1 <- dec (B.unsafeIndex b 1), let !y1 = y0 * 10 + (fromIntegral i1)
      , Just i2 <- dec (B.unsafeIndex b 2), let !y2 = y1 * 10 + (fromIntegral i2)
      , Just i3 <- dec (B.unsafeIndex b 3), let !y3 = y2 * 10 + (fromIntegral i3)
      , 0x2D ==         B.unsafeIndex b 4
      , Just i5 <- dec (B.unsafeIndex b 5), let  m0 =            fromIntegral i5
      , Just i6 <- dec (B.unsafeIndex b 6), let !m1 = m0 * 10 + (fromIntegral i6)
          = case fromYearMonthValid (fromIntegral (y3 :: Int)) m1 of
              Nothing -> (# No oob #)
              Just ym -> (# Yes ym #)

      | otherwise = (# No malformed #)



-- | Consume 10 bytes that represent an RFC 3339 month date, in the @yyyy-mm-dd@ format.
monthDate
  :: e            -- ^ Out of bounds.
  -> e            -- ^ Malformed.
  -> e            -- ^ Reached end.
  -> Parser e Day
monthDate oob malformed end = unsafeRead 10 (convMonthDate oob malformed) end

convMonthDate :: e -> e -> B.ByteString -> (# Res e Day #)
convMonthDate oob malformed b
  | Just i0 <- dec (B.unsafeIndex b 0), let  y0 =            fromIntegral i0
  , Just i1 <- dec (B.unsafeIndex b 1), let !y1 = y0 * 10 + (fromIntegral i1)
  , Just i2 <- dec (B.unsafeIndex b 2), let !y2 = y1 * 10 + (fromIntegral i2)
  , Just i3 <- dec (B.unsafeIndex b 3), let !y3 = y2 * 10 + (fromIntegral i3)
  , 0x2D ==         B.unsafeIndex b 4
  , Just i5 <- dec (B.unsafeIndex b 5), let  m0 =            fromIntegral i5
  , Just i6 <- dec (B.unsafeIndex b 6), let !m1 = m0 * 10 + (fromIntegral i6)
  , 0x2D ==         B.unsafeIndex b 7
  , Just i8 <- dec (B.unsafeIndex b 8), let  d0 =            fromIntegral i8
  , Just i9 <- dec (B.unsafeIndex b 9), let !d1 = d0 * 10 + (fromIntegral i9)
      = case fromGregorianValid (fromIntegral (y3 :: Int)) m1 d1 of
          Nothing  -> (# No oob #)
          Just ymd -> (# Yes ymd #)

  | otherwise = (# No malformed #)



-- | Consume 10 bytes that represent an ISO 8601 week date,
--   in the extended @yyyy-Www-d@ format.
weekDate
  :: e            -- ^ Out of bounds.
  -> e            -- ^ Malformed.
  -> e            -- ^ Reached end.
  -> Parser e Day
weekDate oob malformed end = unsafeRead 10 conv end
  where
    conv b
      | Just i0 <- dec (B.unsafeIndex b 0), let  y0 =            fromIntegral i0
      , Just i1 <- dec (B.unsafeIndex b 1), let !y1 = y0 * 10 + (fromIntegral i1)
      , Just i2 <- dec (B.unsafeIndex b 2), let !y2 = y1 * 10 + (fromIntegral i2)
      , Just i3 <- dec (B.unsafeIndex b 3), let !y3 = y2 * 10 + (fromIntegral i3)
      , 0x2D ==         B.unsafeIndex b 4
      , 0x57 ==         B.unsafeIndex b 5
      , Just i6 <- dec (B.unsafeIndex b 6), let  w0 =            fromIntegral i6
      , Just i7 <- dec (B.unsafeIndex b 7), let !w1 = w0 * 10 + (fromIntegral i7)
      , 0x2D ==         B.unsafeIndex b 8
      , Just i9 <- dec (B.unsafeIndex b 9), let  d0 =            fromIntegral i9
          = case fromWeekDateValid (fromIntegral (y3 :: Int)) w1 d0 of
              Nothing  -> (# No oob #)
              Just ymd -> (# Yes ymd #)

      | otherwise = (# No malformed #)



-- | Consume 8 bytes that represent an ISO 8601 ordinal date, in the @yyyy-ddd@ format.
ordinalDate
  :: e            -- ^ Out of bounds.
  -> e            -- ^ Malformed.
  -> e            -- ^ Reached end.
  -> Parser e Day
ordinalDate oob malformed end = unsafeRead 8 conv end
  where
    conv b
      | Just i0 <- dec (B.unsafeIndex b 0), let  y0 =            fromIntegral i0
      , Just i1 <- dec (B.unsafeIndex b 1), let !y1 = y0 * 10 + (fromIntegral i1)
      , Just i2 <- dec (B.unsafeIndex b 2), let !y2 = y1 * 10 + (fromIntegral i2)
      , Just i3 <- dec (B.unsafeIndex b 3), let !y3 = y2 * 10 + (fromIntegral i3)
      , 0x2D ==         B.unsafeIndex b 4
      , Just i5 <- dec (B.unsafeIndex b 5), let  d0 =            fromIntegral i5
      , Just i6 <- dec (B.unsafeIndex b 6), let !d1 = d0 * 10 + (fromIntegral i6)
      , Just i7 <- dec (B.unsafeIndex b 7), let !d2 = d1 * 10 + (fromIntegral i7)
          = case fromOrdinalDateValid (fromIntegral (y3 :: Int)) d2 of
              Nothing  -> (# No oob #)
              Just ymd -> (# Yes ymd #)

      | otherwise = (# No malformed #)



fraction :: e -> Int -> Parser e Pico
fraction malformed ss = do
  hasDot <- ( do w <- word8 False
                 if w == 0x2E
                   then pure True
                   else err False
            )
              `catch` pure

  if not hasDot
    then pure $! MkFixed $ fromIntegral ss * 1_000_000_000_000
    else do
      o <- bytesRead
      frac <- fracFloat52Dec (FracFloat 0 0)
      o' <- bytesRead
      if o == o'
        then err malformed
        else pure $! MkFixed $ fromIntegral ss * 1_000_000_000_000
                                               + truncate (fracToDouble Plus frac 12)




-- | Consume at least 8 bytes that represent an RFC 3339 time of day,
--   in the @hh:mm:ss[.s…]@ format.
timeOfDay
  :: e                  -- ^ Out of bounds.
  -> e                  -- ^ Malformed.
  -> e                  -- ^ Reached end.
  -> Parser e TimeOfDay
timeOfDay oob malformed end = do
  (hh, mm, ss) <- unsafeRead 8 (convTimeOfDay malformed) end

  ssms <- fraction malformed ss

  case makeTimeOfDayValid hh mm ssms of
    Nothing  -> err oob
    Just tod -> pure tod

convTimeOfDay :: e -> B.ByteString -> (# Res e (Int, Int, Int) #)
convTimeOfDay malformed b
  | Just i0 <- dec (B.unsafeIndex b 0), let  h0 =            fromIntegral i0
  , Just i1 <- dec (B.unsafeIndex b 1), let !h1 = h0 * 10 + (fromIntegral i1)
  , 0x3A ==         B.unsafeIndex b 2
  , Just i3 <- dec (B.unsafeIndex b 3), let  m0 =            fromIntegral i3
  , Just i4 <- dec (B.unsafeIndex b 4), let !m1 = m0 * 10 + (fromIntegral i4)
  , 0x3A ==         B.unsafeIndex b 5
  , Just i6 <- dec (B.unsafeIndex b 6), let  s0 =            fromIntegral i6
  , Just i7 <- dec (B.unsafeIndex b 7), let !s1 = s0 * 10 + (fromIntegral i7)
      = (# Yes (h1, m1, s1) #)

  | otherwise = (# No malformed #)



-- | Consume at least 19 bytes that represent an RFC 3339 full time without time zone,
--   in the @yyyy-mm-ddThh:mm:ss[.s…]@ format.
--
--   Either @t@ or space is accepted instead of @T@.
localTime
  :: e                  -- ^ Out of bounds.
  -> e                  -- ^ Malformed.
  -> e                  -- ^ Reached end.
  -> Parser e LocalTime
localTime oob malformed end = do
  (ymd, hh, mm, ss) <- unsafeRead 19 conv end

  ssms <- fraction malformed ss

  case makeTimeOfDayValid hh mm ssms of
    Nothing  -> err oob
    Just tod -> pure $! LocalTime ymd tod
  where
    conv b =
      let !(# uymd #) = convMonthDate oob malformed b
      in case uymd of
           No e    -> (# No e #)
           Yes ymd ->

             let t = B.unsafeIndex b 10
             in if (t .&. 0xDF) == 0x54 || t == 0x20
                  then

                    let !(# uhns #) = convTimeOfDay malformed (B.unsafeDrop 11 b)
                    in case uhns of
                         No e             -> (# No e #)
                         Yes (hh, nn, ss) -> (# Yes (ymd, hh, nn, ss) #)

                  else (# No malformed #)



-- | Consume 1 or 6 bytes that represent an RFC 3339 time zone,
--   in the @Z@ and @±hh:mm@ formats respectively.
--
--   @z@ is accepted instead of @Z@.
timeZone
  :: e                 -- ^ Out of bounds.
  -> e                 -- ^ Malformed.
  -> e                 -- ^ Reached end.
  -> Parser e TimeZone
timeZone oob malformed end = do
  z <- word8 end
  if (z .&. 0xDF) == 0x5A
    then pure utc
    else if z == 0x2B
           then longZone False
           else if z == 0x2D
                  then longZone True
                  else err malformed
  where
    longZone m = unsafeRead 5 conv end
      where
        conv b
          | Just i0 <- dec (B.unsafeIndex b 0), let  h0 =            fromIntegral i0
          , Just i1 <- dec (B.unsafeIndex b 1), let !h1 = h0 * 10 + (fromIntegral i1)
          , 0x3A ==         B.unsafeIndex b 2
          , Just i3 <- dec (B.unsafeIndex b 3), let  m0 =            fromIntegral i3
          , Just i4 <- dec (B.unsafeIndex b 4), let !m1 = m0 * 10 + (fromIntegral i4)
              = if h1 >= 24 || m1 >= 60
                  then (# No oob #)
                  else let minus = if m
                                     then negate
                                     else id

                           !offset = minus $ (h1 * 60) + m1

                       in (# Yes (TimeZone offset False "") #)

          | otherwise = (# No malformed #)



-- | Consume at least 19 bytes that represent an RFC 3339 full time with a time zone.
--
--   Combines behaviors of both 'timeOfDay' and 'timeZone'.
zonedTime
  :: e                  -- ^ Out of bounds.
  -> e                  -- ^ Malformed.
  -> e                  -- ^ Reached end.
  -> Parser e ZonedTime
zonedTime oob malformed end = do
  local <- localTime oob malformed end
  zone  <- timeZone oob malformed end
  pure $! ZonedTime local zone
