{-# LANGUAGE NoImplicitPrelude, NamedFieldPuns, OverloadedStrings #-}
-- | 

module Wikigen.Aravelie
       ( timeToAlternate
       , dateToArvelie
       , timeToNeralie
       ) where

import Universum
import Data.Time

-- The Aravelie calendar date format.
data Arvelie = Arvelie
  { week :: Text
  , day :: Integer }
  deriving (Eq, Ord, Show)

-- The Neralie time format.
data Neralie = Neralie
  { beat :: Integer
  , pulse :: Integer }
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
dateToArvelie d =
  let week = "1"
      day = 1
  in
  Arvelie { week, day }

-- convert a time to its corresponding neralie time
timeToNeralie :: TimeOfDay -> Neralie
timeToNeralie t =
  let pulses = ((todHour t * 60) + todMin t) * 60
      res = fromIntegral (pulses / 86.4) -- div (realToFrac $ toInteger $ pulses) 86.4
      beat = toInteger $ ceiling $ realToFrac res
      pulse = res - beat
   in
   Neralie { beat, pulse }
