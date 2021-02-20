{-#LANGUAGE DeriveGeneric, OverloadedStrings, DataKinds, TypeOperators, NoImplicitPrelude #-}

module Wikigen.Transform
       ( modifyAst       ) where

import Text.Pandoc
import Wikigen.Types
import Universum

-- Transform the Pandoc document AST
modifyAst :: Metadata -> Pandoc -> Pandoc
modifyAst metadata ast = ast
