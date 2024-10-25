{-# LANGUAGE NumericUnderscores #-}

{-| Functions for encoding data using common time formats.
 -}

module Builder.Lathe.Time
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
  , utcTime
  , timeZone
  , zonedTime
  ) where

import           Data.ByteString.Builder
import qualified Data.ByteString.Builder.Prim as Prim

import           Data.Bits
import           Data.Fixed
import           Data.Time.Calendar
import           Data.Time.Calendar.Month
import           Data.Time.Calendar.OrdinalDate
import           Data.Time.Calendar.Quarter
import           Data.Time.Calendar.WeekDate
import           Data.Time.Clock
import           Data.Time.LocalTime



{-# INLINE oneDec #-}
oneDec :: Prim.FixedPrim Int
oneDec =
  (\x -> fromIntegral x + 0x30
   )
     Prim.>$< Prim.word8

{-# INLINE twoDec #-}
twoDec :: Prim.FixedPrim Int
twoDec =
  (\xx -> let (x1, x0) = xx `quotRem` 10
          in (unsafeShiftL (fromIntegral x1) 8 + fromIntegral x0 + 0x3030)
   )
     Prim.>$< Prim.word16BE

{-# INLINE threeDec #-}
threeDec :: Prim.FixedPrim Int
threeDec =
  (`quotRem` 100)
     Prim.>$< oneDec
     Prim.>*< twoDec



trimmedSeconds :: Pico -> Builder
trimmedSeconds (MkFixed ssms) =
  let double = fromIntegral ssms / 1_000_000_000_000
      ss     = truncate double
      ms     = double - fromIntegral ss

  in if ms == 0
       then Prim.primFixed twoDec ss
       else ( if ss >= 10
                then mempty
                else word8 0x30
            )
         <> formatDouble standardDefaultPrecision double



{-# INLINE withYear #-}
withYear :: Year -> (Prim.FixedPrim Int -> Int -> Builder) -> Builder
withYear yyyy f =
  let (m, y, yyy) | yyyy >= 0 = let (x, xxx) = yyyy `quotRem` 1_000
                                in (False, x, xxx)
                  | otherwise = let (x, xxx) = negate yyyy `quotRem` 1_000
                                in (True, x, xxx)

  in    ( if m
            then word8 0x2D
            else mempty
        )
     <> integerDec y
     <> f threeDec (fromIntegral yyy)



-- | Encode a year in ISO 8601 extended @[-…y]yyyy@ format. At least 4 bytes wide.
--
--  Compatible with RFC 3339 if the year is in the \([0,10000)\) interval.
year :: Year -> Builder
year yyyy = withYear yyyy Prim.primFixed



-- | Encode a year quarter in @[-…y]yyyy-Qq@ format. At least 7 bytes wide.
--
--   Note that year quarters are not defined by ISO 8601, this is an informal convention.
quarter :: Quarter -> Builder
quarter (YearQuarter yyyy q) =
  let i = case q of
            Q1 -> 0x31
            Q2 -> 0x32
            Q3 -> 0x33
            Q4 -> 0x34

  in withYear yyyy $ \yearDec yyy ->
       Prim.primFixed
         ( id
             Prim.>$< yearDec
             Prim.>*< Prim.word16BE
             Prim.>*< Prim.word8
         )
         (yyy, (0x2D51, i))



-- | Encode a month in ISO 8601 extended @[-…y]yyyy-mm@ format. At least 7 bytes wide.
--
--   RFC 3339 compatibility matches that of the 'year' function.
month :: Month -> Builder
month (YearMonth yyyy mm) =
  withYear yyyy $ \yearDec yyy ->
    Prim.primFixed
      ( id
          Prim.>$< yearDec
          Prim.>*< Prim.word8
          Prim.>*< twoDec
      )
      (yyy, (0x2D, mm))



-- | Encode a month date in ISO 8601 extended @[-…y]yyyy-mm-dd@ format.
--   At least 10 bytes wide.
--
--   RFC 3339 compatibility matches that of the 'year' function.
monthDate :: Day -> Builder
monthDate (YearMonthDay yyyy mm dd) =
  withYear yyyy $ \yearDec yyy ->
    Prim.primFixed
      ( id
          Prim.>$< yearDec
          Prim.>*< Prim.word8
          Prim.>*< twoDec
          Prim.>*< Prim.word8
          Prim.>*< twoDec
      )
      (yyy, (0x2D, (mm, (0x2D, dd))))



-- | Encode an ordinal date in ISO 8601 extended @[-…y]yyyy-ddd@ format.
--   At least 8 bytes wide.
ordinalDate :: Day -> Builder
ordinalDate x =
  let (yyyy, ddd) = toOrdinalDate x
  in withYear yyyy $ \yearDec yyy ->
       Prim.primFixed
         ( id
             Prim.>$< yearDec
             Prim.>*< Prim.word8
             Prim.>*< threeDec
         )
         (yyy, (0x2D, ddd))



-- | Encode a week date in ISO 8601 extended @[-…y]yyyy-Www-d@ format.
--   At least 10 bytes wide.
weekDate :: Day -> Builder
weekDate x =
  let (yyyy, ww, d) = toWeekDate x
  in withYear yyyy $ \yearDec yyy ->
       Prim.primFixed
         ( id
             Prim.>$< yearDec
             Prim.>*< Prim.word16BE
             Prim.>*< twoDec
             Prim.>*< Prim.word8
             Prim.>*< oneDec
         )
         (yyy, (0x2D57, (ww, (0x2D, d))))



-- | Encode a time of day in RFC 3339 @hh:mm:ss[.s…]@ format.
--   8 to 21 bytes wide.
timeOfDay :: TimeOfDay -> Builder
timeOfDay (TimeOfDay hh mm ssms) =
     Prim.primFixed
       ( id
           Prim.>$< twoDec
           Prim.>*< Prim.word8
           Prim.>*< twoDec
           Prim.>*< Prim.word8
       )
       (hh, (0x3A, (mm, 0x3A)))

  <> trimmedSeconds ssms



-- | Encode a local time in ISO 8601 extended @[-…y]yyyy-mm-ddThh:mm:ss[.s…]@ format.
--   At least 19 bytes wide.
--
--   RFC 3339 compatibility matches that of the 'year' function.
localTime :: LocalTime -> Builder
localTime (LocalTime (YearMonthDay yyyy mm dd) (TimeOfDay hh nn ssms)) =
     withYear yyyy $ \yearDec yyy -> do
       Prim.primFixed
         ( id
             Prim.>$< yearDec
             Prim.>*< Prim.word8
             Prim.>*< twoDec
             Prim.>*< Prim.word8
             Prim.>*< twoDec
             Prim.>*< Prim.word8
             Prim.>*< twoDec
             Prim.>*< Prim.word8
             Prim.>*< twoDec
             Prim.>*< Prim.word8
         )
         (yyy, (0x2D, (mm, (0x2D, (dd, (0x54, (hh, (0x3A, (nn, 0x3A)))))))))

  <> trimmedSeconds ssms



-- | Encode a UTC time in ISO 8601 extended @[-…y]yyyy-mm-ddThh:mm:ss[.s…]Z@ format.
--   At least 20 bytes wide.
--
--   RFC 3339 compatibility matches that of the 'year' function.
utcTime :: UTCTime -> Builder
utcTime time = localTime (utcToLocalTime utc time) <> word8 0x5A



-- | Encode a time zone in ISO 8601 extended @±[…h]hh:mm@ format.
--   At least 6 bytes wide.
--
--   Compatible with RFC 3339 if the minute offset from UTC is
--   in the \((-24 \cdot 60, 24 \cdot 60)\) interval.
timeZone :: TimeZone -> Builder
timeZone zone =
  let hhmm = timeZoneMinutes zone

      (m, hh, mm) = if hhmm >= 0
                      then let (yy, zz) = hhmm `quotRem` 60
                           in (0x2B, yy, zz)

                      else let (yy, zz) = negate hhmm `quotRem` 60
                           in (0x2D, yy, zz)

  in Prim.primFixed
       ( id
           Prim.>$< Prim.word8
           Prim.>*< twoDec
           Prim.>*< Prim.word8
           Prim.>*< twoDec
       )
       (m, (hh, (0x3A, mm)))



-- | Encode a zoned time in ISO 8601 extended
--   @[-…y]yyyy-mm-ddThh:mm:ss[.s…]±[…h]hh:mm@ format.
--   At least 25 bytes wide.
--
--   RFC 3339 compatibility matches that of both the 'year' and 'timeZone' functions.
zonedTime :: ZonedTime -> Builder
zonedTime (ZonedTime local zone) = localTime local <> timeZone zone
