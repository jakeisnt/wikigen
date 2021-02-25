{-# LANGUAGE NoImplicitPrelude, NamedFieldPuns, OverloadedStrings #-}
-- | 

module Wikigen.Arvelie
       ( timeToAlternate
       , dateToArvelie
       , timeToNeralie
       , parseDay
       ) where

import Universum
import Data.Time
import Data.Time.Parse
import qualified Text.Show

-- parse a date string to a day
parseDay :: String -> Maybe Day 
parseDay s = strptime "%Y-%m-%d" s <&> (localDay . fst)

-- TODO: when this is complete enough, give it its own library!

-- ensure that the passed string has the correct length
-- if not, pad it with the necessary number of prefixes
ensurePrefixes :: Char -> Int -> String -> String
ensurePrefixes c num str = take ( num - length str) (repeat c) ++ str

-- The Aravelie calendar date format.
-- The default instance of show is fine here.
data Arvelie = Arvelie
  { year :: Int
  , week :: Char
  , day :: Int }
  deriving (Eq, Ord)

instance Show Arvelie where
  show a = ensurePrefixes '0' 2 (show (year a)) ++ (week a):ensurePrefixes '0' 2 (show (day a))

-- The Neralie time format.
data Neralie = Neralie
  { beat :: Int
  , pulse :: Int }
  deriving (Eq, Ord)

instance Show Neralie where
  show n = ensurePrefixes '0' 3 (show (beat n)) ++ ":" ++ ensurePrefixes '0' 3 (show (pulse n))

data AlternateTime = AlternateTime
  { date :: Arvelie
  , time :: Neralie }
  deriving (Eq, Ord)

instance Show AlternateTime where
  show a = show (date a) ++ "/" ++ show (time a)

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
      weekNum = floor $ (toRational daysSoFar) / 14
      -- for months over 26, use '+' to denote the additional days.
      week = if weekNum > 27 then '+' else chr (weekNum + 63)
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
