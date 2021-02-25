{-#LANGUAGE DeriveGeneric, OverloadedStrings, DataKinds, TypeOperators, NoImplicitPrelude, ScopedTypeVariables #-}

module Wikigen.Html
       ( unparseHtml
       ) where

import Universum
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Html.Renderer.String as H
import qualified Data.Text as T
import Text.Pandoc

newtype Title = Title String

-- write Html to a text buffer
unparseHtml :: Pandoc -> IO T.Text
unparseHtml ast = do
  html <- runIO (writeHtml5String def ast) >>= handleError
  return $ augmentHtml (Title "title") html

  where
    -- add additional information to the html exported
    augmentHtml :: Title -> Text -> Text
    augmentHtml tt = T.pack . H.renderHtml . (augmentBlaze tt) . H.preEscapedToHtml

    -- add some information to the header that blaze html neglected to
    augmentBlaze :: Title -> H.Html -> H.Html
    augmentBlaze (Title t) html =
      H.docTypeHtml $ do
      H.head $ do
        H.title $ H.toHtml t
        H.link H.! A.rel "stylesheet" H.! A.type_ "text/css" H.! A.href "https://jakeisnt.github.io/styles/main.css"
      H.body $ do
        H.title $ H.toHtml t
        html
