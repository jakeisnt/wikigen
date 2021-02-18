{-#LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
-- | 

module Wikigen.PandocParser
       (parseFile) where

import Wikigen.Ast
import qualified Data.Text                  as T
import Text.Pandoc
import Text.Pandoc.Highlighting (pygments)
import Text.Pandoc.Definition
import Universum
import Data.Default

parseFile :: Text -> Org
parseFile t =  error "bad"
