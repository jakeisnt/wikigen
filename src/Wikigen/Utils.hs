{-#LANGUAGE DeriveGeneric, OverloadedStrings, DataKinds, TypeOperators, NoImplicitPrelude #-}

module Wikigen.Utils
       ((|>), getTitle
       ) where

import Universum
import Text.Pandoc
import Wikigen.Types (Title(..))
import qualified Data.Text as T


(|>) :: a -> (a -> b) -> b
(|>) arg function = function arg

-- get the title of the file out of the metadata
getTitle :: Pandoc -> Title
getTitle (Pandoc m _) =
  let defaultTitle = "Jacob Chvatal's Wiki" in
    Title $ case nonEmpty $ docTitle m of
       Just v -> concatInlines $ map (\il -> case il of
                                   Str txt -> txt
                                   Space -> " "
                                   _ -> defaultTitle
                            ) v
       Nothing -> defaultTitle
       where
         -- lose style of inline, just keep the title text
         concatInlines :: NonEmpty Text -> Text
         concatInlines = foldl' T.append ""
