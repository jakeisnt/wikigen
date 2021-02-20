{-#LANGUAGE DeriveGeneric, OverloadedStrings, DataKinds, TypeOperators, NoImplicitPrelude #-}

module Wikigen.Types
       ( Metadata(..), mapMetadata, mergeMetadata, mtMeta
       ) where

import Universum

data Metadata = Metadata { fileLinks :: [(Text, Text)] } deriving (Generic, Show)

mergeMetadata :: Metadata -> Metadata -> Metadata
mergeMetadata m1 m2 = Metadata { fileLinks = fileLinks m1 ++ fileLinks m2 }

mapMetadata :: [Metadata] -> Metadata
mapMetadata ms = foldl' mergeMetadata mtMeta ms

mtMeta :: Metadata
mtMeta = Metadata { fileLinks = [] }
