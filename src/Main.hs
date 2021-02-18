{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Wikigen.Ast
import Wikigen.IO

import Data.Char (isSpace)
import qualified Data.Text as T
import Lucid
import System.FilePath (takeBaseName, (-<.>))
import System.Environment
import Universum

main :: IO ()
main = do
  args <- getArgs
  progName <- getProgName          -- IO String
  _ <- mapM putStrLn args  
  putStrLn progName
  case (nonEmpty args) of
    Just a -> parseFile $ head a
    Nothing -> error "Provide a file name to parse!"

parseFile :: String -> IO ()
parseFile filePath = do
  (_, ast) <- readOrgFile filePath
  renderToFile (filePath -<.> ".html") (genPage ast)

genPage :: Org -> Html ()
genPage ast =
  let title = toHtml $ _orgTitle ast
      contents = toHtml $ genSemanticSection $ _orgStructuredText ast
      body = mapM_ genBody $ _orgSubtrees ast
   in html_ $ do
        head_ $ do
          title_ title
          meta_ [charset_ "utf-8"]
          link_ [rel_ "stylesheet", type_ "text/css", href_ "main.css"]
        body_
          ( do
              h1_ title
              toHtml contents
              toHtml body
          )
  where
    depthTitles = [h1_, h2_, h3_, h4_, h5_]
    getDepthTitle' :: Term a b => [a -> b] -> Natural -> (a -> b)
    getDepthTitle' depthList depth =
      let neList = nonEmpty depthList
       in case (depth, neList) of
            (_, Nothing) -> h5_
            (0, Just ls) -> head ls
            (_, Just ls) -> getDepthTitle' (tail ls) (depth - 1)
    getDepthTitle = getDepthTitle' depthTitles

    -- clickable header that hides text
    hideableComponent :: Html () -> Text -> Html () -> Html ()
    hideableComponent title titleStr body = do
      input_ [type_ "checkbox", id_ titleStr, style_ "display: none;"]
      div_ [id_ "hidden"] body
      label_ [Lucid.for_ titleStr] title

    genBody' :: Natural -> Org -> Html ()
    genBody' depth ast =
      let title = _orgTitle ast
          bodyText = genSemanticSection $ _orgStructuredText ast
          bodyRest = mapM_ (genBody' (depth + 1)) $ _orgSubtrees ast
          headerElement = getDepthTitle depth
          titleNoSpaces = T.pack $ filter isSpace $ T.unpack title
       in div_ $ do
            headerElement [id_ titleNoSpaces] $ toHtml title
            bodyText
            bodyRest

    genBody = genBody' 0

    genSemanticSection :: OrgSection -> Html ()
    genSemanticSection (OrgSection contents) = mapM_ genSemanticContent contents

    genSemanticContent :: OrgContent -> Html ()
    genSemanticContent section = case section of
      OrgUnorderedList ols -> ul_ $ mapM_ (\(OrgItem c) -> li_ $ mapM_ genSemanticContent c) ols
      OrgOrderedList ols -> ol_ $ mapM_ (\(OrgItem c) -> li_ $ mapM_ genSemanticContent c) ols
      OrgParagraph lm -> mapM_ genSemanticMarkup lm

    genSemanticMarkup :: Markup -> Html ()
    genSemanticMarkup m =
      case m of
        OrgPlain t -> toHtml t
        OrgLaTeX t -> toHtml t -- TODO: more formatting is needed here. (if i use latex in the future)
        OrgVerbatim t -> pre_ $ toHtml t
        OrgCode (Language l) (Output o) t -> code_ $ toHtml t
        OrgBold ms -> span_ [class_ "bold"] $ mapM_ genSemanticMarkup ms
        OrgItalic ms -> span_ [class_ "italic"] $ mapM_ genSemanticMarkup ms
        OrgUnderLine ms -> span_ [class_ "underline"] $ mapM_ genSemanticMarkup ms
        OrgStrikethrough ms -> span_ [class_ "strikethrough"] $ mapM_ genSemanticMarkup ms
        OrgHyperLink {link = l, description = d} ->
          -- default to using the link as the title if the description isn't available
          a_ [href_ l] $ toHtml $ fromMaybe l d
        OrgFileLink {filepath = fp, description = d} ->
          -- TODO: without a name this looks weird
          let htmlLink = T.pack $ "./" ++ fp -<.> ".html"
              linkTitle = fromMaybe (T.pack $ takeBaseName fp) d
           in a_ [href_ htmlLink, class_ "internal"] $ toHtml linkTitle
