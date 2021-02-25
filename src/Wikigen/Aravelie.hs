{-# LANGUAGE NoImplicitPrelude, NamedFieldPuns, OverloadedStrings #-}
-- | 

module Wikigen.Aravelie
       ( timeToAlternate
       , dateToArvelie
       , timeToNeralie
       ) where

import Universum
import Data.Time
import Data.Char (ord)

-- The Aravelie calendar date format.
data Arvelie = Arvelie
  { year :: Int
  , week :: Char
  , day :: Int }
  deriving (Eq, Ord, Show)

-- The Neralie time format.
data Neralie = Neralie
  { beat :: Int
  , pulse :: Int }
  deriving (Eq, Ord, Show)

data AlternateTime = AlternateTime
  { date :: Arvelie
  , time :: Neralie }
  deriving (Eq, Ord, Show)

-- convert a local time to its alternate format
timeToAlternate :: LocalTime -> AlternateTime
timeToAlternate dt = AlternateTime { date = dateToArvelie $ localDay dt, time = timeToNeralie $ localTimeOfDay dt }

-- convert a day to its corresponding arvelie date
-- assume initialization year of 2020
dateToArvelie :: Day -> Arvelie
dateToArvelie dt =
  -- get days of the year
  -- get years since beginning (2020)
  let (y, m, d) = toGregorian dt
      year = fromIntegral $ y - 2020
      -- computes the number of day
      daysSoFar = foldl' (\cum month -> (gregorianMonthLength y month) + cum) 0 [0..(m - 1)] + d
      -- add constant to start with letter A, divide by 14 days per week
      weekNum = floor $ (toRational daysSoFar) / 14 + 65 
      -- for months over 26, use '+' to denote the additional days.
      week = if weekNum > 25 then '+' else chr weekNum
      day = rem daysSoFar 14
  in
  Arvelie { year, week, day }

-- convert a time to its corresponding neralie time
timeToNeralie :: TimeOfDay -> Neralie
timeToNeralie t =
  let pulses = toRational $ ((todHour t * 60) + todMin t) * 60
      res = pulses / 86.4
      beat = floor res
      pulse = floor $ res * 1000
   in
   Neralie { beat, pulse }
