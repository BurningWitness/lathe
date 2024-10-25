{-# LANGUAGE OverloadedStrings
           , StandaloneDeriving #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Lathe.Time
  ( time
  ) where

import           Builder.Lathe.Time as I
import           Parser.Lathe.Time as O

import           Data.ByteString.Builder
import qualified Data.ByteString.Lazy as L
import           Data.Foldable
import           Data.Time.Calendar
import           Data.Time.Calendar.Month
import           Data.Time.Calendar.OrdinalDate
import           Data.Time.Calendar.Quarter
import           Data.Time.Calendar.WeekDate
import           Data.Time.Clock
import           Data.Time.LocalTime
import           Parser.Lathe
import           Test.Hspec



deriving instance Eq ZonedTime



data Error = OutOfBounds
           | Malformed
           | Error
             deriving (Show, Eq)



built :: (a -> Builder) -> a -> L.LazyByteString -> Spec
built build a ref =
  it "build" $
    toLazyByteString (build a) `shouldBe` ref

parsed :: (Eq a, Show a) => Parser Error a -> a -> L.ByteString -> Spec
parsed tear a ref =
  it "parse" $
    let (Scrap _ remaining more, ei) = parse tear ref
    in (remaining, more, ei) `shouldBe` ("", End, Right a)



pit :: (Eq a, Show a) => (a -> Builder) -> Parser Error a -> [(a, L.ByteString)] -> Spec
pit build tear as =
  for_ as $ \(a, ref) ->
    describe (show a) $ do
      built build a ref
      parsed tear a ref



time :: Spec
time = do
  describe "year" $ do
    pit I.year (O.year Malformed Error)
      [ (   0, "0000")
      , (1234, "1234")
      , ( 507, "0507")
      , (9999, "9999")
      ]

    describe "10000" $
      built I.year 10000 "10000"

    describe "-1" $
      built I.year (-1) "-0001"

    describe "-54321" $
      built I.year (-54321) "-54321"

  describe "quarter" $
    pit I.quarter (O.quarter OutOfBounds Malformed Error)
      [ (YearQuarter    0 Q1, "0000-Q1")
      , (YearQuarter 2468 Q2, "2468-Q2")
      , (YearQuarter  390 Q3, "0390-Q3")
      , (YearQuarter 9999 Q4, "9999-Q4")
      ]

  describe "month" $
    pit I.month (O.month OutOfBounds Malformed Error)
      [ (YearMonth    0  1, "0000-01")
      , (YearMonth 3692  5, "3692-05")
      , (YearMonth  407  8, "0407-08")
      , (YearMonth 9999 12, "9999-12")
      ]

  describe "monthDate" $
    pit I.monthDate (O.monthDate OutOfBounds Malformed Error)
      [ (YearMonthDay    0  1  1 , "0000-01-01")
      , (YearMonthDay 4826  5 27, "4826-05-27")
      , (YearMonthDay  690  8 14, "0690-08-14")
      , (YearMonthDay 9999 12 31, "9999-12-31")
      ]

  describe "weekDate" $
    pit I.weekDate (O.weekDate OutOfBounds Malformed Error)
      [ (YearWeekDay    0  1 Monday   , "0000-W01-1")
      , (YearWeekDay 5678 20 Wednesday, "5678-W20-3")
      , (YearWeekDay  505 53 Saturday , "0505-W53-6")
      , (YearWeekDay 9999 52 Sunday   , "9999-W52-7")
      ]

  describe "ordinalDate" $
    pit I.ordinalDate (O.ordinalDate OutOfBounds Malformed Error)
      [ (fromOrdinalDate 0    1  , "0000-001")
      , (fromOrdinalDate 6736 366, "6736-366")
      , (fromOrdinalDate 803  87 , "0803-087")
      , (fromOrdinalDate 9999 365, "9999-365")
      ]

  describe "timeOfDay" $
    pit I.timeOfDay (O.timeOfDay OutOfBounds Malformed Error)
      [ (TimeOfDay  0  0  0             , "00:00:00")
      , (TimeOfDay  1  2  0.000000000001, "01:02:00.000000000001")
      , (TimeOfDay 12 34 56.7890123     , "12:34:56.7890123")
      , (TimeOfDay 23 59 59.999999999999, "23:59:59.999999999999")
      , (TimeOfDay 23 59 60             , "23:59:60")
      ]

  describe "localTime" $ do
    pit I.localTime (O.localTime OutOfBounds Malformed Error)
      [ (LocalTime (YearMonthDay    0  1  1) (TimeOfDay  0  0  0)             , "0000-01-01T00:00:00")
      , (LocalTime (YearMonthDay 4826  5 27) (TimeOfDay  1  2  0.000000000027), "4826-05-27T01:02:00.000000000027")
      , (LocalTime (YearMonthDay  690  8 14) (TimeOfDay 12 34 56.7890123)     , "0690-08-14T12:34:56.7890123")
      , (LocalTime (YearMonthDay 9999 12 31) (TimeOfDay 23 59 59.999999999999), "9999-12-31T23:59:59.999999999999")
      , (LocalTime (YearMonthDay 9999 12 31) (TimeOfDay 23 59 60)             , "9999-12-31T23:59:60")
      ]

    describe "1234-12-13 14:15:16:17" $
      parsed (O.localTime OutOfBounds Malformed Error)
        (LocalTime (YearMonthDay 1234 12 13) (TimeOfDay 14 15 16.17))
        "1234-12-13 14:15:16.17"

    describe "4321-09-10t11:12:13:1415" $
      parsed (O.localTime OutOfBounds Malformed Error)
        (LocalTime (YearMonthDay 4321  9 10) (TimeOfDay 11 12 13.1415))
        "4321-09-10t11:12:13.1415"

  describe "utcTime" $
    describe "1234-12-13T14:15:16:17Z" $
      built I.utcTime
        (UTCTime (YearMonthDay 1234 12 13) (timeOfDayToTime (TimeOfDay 14 15 16.17)))
        "1234-12-13T14:15:16.17Z"

  describe "timeZone" $ do
    describe "Z" $
      parsed (O.timeZone OutOfBounds Malformed Error) utc "Z"

    describe "z" $
      parsed (O.timeZone OutOfBounds Malformed Error) utc "z"

    pit I.timeZone (O.timeZone OutOfBounds Malformed Error)
      [ (minutesToTimeZone      0 , "+00:00")
      , (minutesToTimeZone  (-300), "-05:00")
      , (minutesToTimeZone    540 , "+09:00")
      , (minutesToTimeZone    754 , "+12:34")
      , (minutesToTimeZone   1439 , "+23:59")
      , (minutesToTimeZone (-1439), "-23:59")
      ]

  describe "zonedTime" $
    pit I.zonedTime (O.zonedTime OutOfBounds Malformed Error)
      [ (ZonedTime (LocalTime (YearMonthDay    0  1  1) (TimeOfDay  0  0  0))              (minutesToTimeZone      0 ), "0000-01-01T00:00:00+00:00")
      , (ZonedTime (LocalTime (YearMonthDay 4826  5 27) (TimeOfDay  1  2  0.000000000027)) (minutesToTimeZone    540 ), "4826-05-27T01:02:00.000000000027+09:00")
      , (ZonedTime (LocalTime (YearMonthDay  690  8 14) (TimeOfDay 12 34 56.7890123))      (minutesToTimeZone  (-754)), "0690-08-14T12:34:56.7890123-12:34")
      , (ZonedTime (LocalTime (YearMonthDay 9999 12 31) (TimeOfDay 23 59 59.999999999999)) (minutesToTimeZone   1439 ), "9999-12-31T23:59:59.999999999999+23:59")
      , (ZonedTime (LocalTime (YearMonthDay 9999 12 31) (TimeOfDay 23 59 60))              (minutesToTimeZone (-1439)), "9999-12-31T23:59:60-23:59")
      ]
