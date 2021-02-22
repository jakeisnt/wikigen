{-#LANGUAGE DeriveGeneric, OverloadedStrings, DataKinds, TypeOperators, NoImplicitPrelude, ScopedTypeVariables #-}

module Main where

import Text.Pandoc

import qualified Data.Text as T
import Options.Generic -- use that harg library later!
import System.FilePath.Find as F
import Universum
import System.FilePath.GlobPattern (GlobPattern)
import Wikigen.Metadata (getMetadata)
import Wikigen.Transform (transformAst)
import Text.Pandoc.Options
import System.FilePath
import Text.Pandoc.Builder
import System.Directory
import Wikigen.File.Utils (addNDirectory, addDirectory)
import qualified Data.Map.Strict as Map
import qualified Text.DocTemplates.Internal
import qualified Data.Text.Internal.Lazy

import qualified Text.Blaze.Html5 as H
-- import Text.Blaze.Html.Renderer.String as H
import Text.Blaze.Html.Renderer.Text as H

-- cli options
data CliOpts = Generate { dirPath :: FilePath }
             | Analyze { } deriving (Generic, Show)

instance ParseRecord CliOpts

main :: IO ()
main = do
    cli <- getRecord "wiki"
    case cli of
      Generate _ -> generateWiki $ dirPath cli
      Analyze -> putStrLn ("analyzing beep beep boop" :: String)

-- finds files matching a pattern in a directory
search :: GlobPattern -> FilePath -> IO [String]
search pat = F.find always (fileName ~~? pat)

-- Ensure the argument is NonEmpty; throw an error if it is
ensureNonEmpty :: [a] -> NonEmpty a
ensureNonEmpty a = fromMaybe (error "it was empty : (") (nonEmpty a)

-- get the export path of a file
getExportPath :: FilePath -> FilePath
getExportPath fp = (addNDirectory 2 fp "public") -<.> ".html";

-- sort using a comparison function that operates under a monad
sortOnM :: (Ord a, Monad m) => (b -> m a) -> [b] -> m [b]
sortOnM getComp ls = do
  comparators <- mapM getComp ls
  let zipped = zip comparators ls;
  return $ map snd $ sortWith fst zipped

-- scan an entire wiki and output its html representation
-- https://rosettacode.org/wiki/Walk_a_directory/Recursively#Haskell
-- presumes this structure:
-- root (provided path)
--  | - Journals
--  | - WikiPages
generateWiki :: FilePath -> IO ()
generateWiki fp = do
  journalFiles <- search "*.org" $ addDirectory fp "journals"
  pageFiles <- search "*.org" $ addDirectory fp "pages"
  rfp <- canonicalizePath $ fp ++ "/public/index.html"
  mapM_ generateWikiFile (pageFiles ++ journalFiles)
  
  sortedPFiles <- sortOnM System.Directory.getModificationTime pageFiles
  writeHomePage rfp [("journals", reverse journalFiles), ("pages", sortedPFiles)]

writeHomePage :: FilePath -> [(Text, [FilePath])] -> IO ()
writeHomePage fp args = do
  html <- return $ unparseHtml $ generateHomePage args
  writeFile fp html
  
-- generate the home page for the wiki files
-- takes a list of tags and associated files with the tags!
generateHomePage :: [(Text, [FilePath])] -> Pandoc
generateHomePage args =
  let relFPs = map (\(t, fps) ->
                      (t,
                       (map
                        (\fp -> "./" ++ (T.unpack t) ++ "/" ++ (takeFileName fp) -<.> ".html"))
                       fps))
               args
  in
  setTitle "Jacob Chvatal's Wiki" $ doc $
  divWith nullAttr $ Text.Pandoc.Builder.fromList $ 
   concatMap Text.Pandoc.Builder.toList
   (map (\(txt, paths) ->
         para (str txt) <> bulletList
         (map (\path ->
                 let url = T.pack path
                     name = T.pack $ takeBaseName path
                 in
                   plain $ link url name (str name))
           paths)
      ) relFPs) 
  
-- Read a wiki file and output its html representation
generateWikiFile :: FilePath -> IO ()
generateWikiFile fp = do
  fileText <- readFile fp
  ast <- parseOrg fileText
  metadata <- return $ getMetadata [ast]
  modAst <- return $ transformAst metadata ast
  result <- return $ unparseHtml modAst
  let expPath = getExportPath fp;
  -- ensureDirsExist $ takeDirectory expPath
  writeFile expPath result

-- parse an Org file to its Pandoc representation
-- handle errors (currently in a bad way)
parseOrg :: T.Text -> IO Pandoc
parseOrg t = do
  maybeFile <- runIO $ readOrg def t
  return $ fromRight (error "bad") maybeFile

-- write Html to a text buffer
unparseHtml :: Pandoc -> Data.Text.Internal.Lazy.Text
-- T.Text
unparseHtml ast = H.renderHtml $ pandocToHtml ast

  where
    -- pandocToHtml :: a -> H.Html
    pandocToHtml ast = writeHtml5 def ast
                       -- { 
                   -- writerVariables =
                   --   Context $
                   --   Map.fromList (
                   --     [("css", toVal ("<style>body{color:#333;font-family:helvetica,arial,sans-serif;line-height:1.5;margin:0 auto;max-width:40em;padding:0 1em}h1,h2,h3,h4,h5,h6{margin:1em 0 .5em;line-height:1.2}a:link,a:visited{color:#03c;text-decoration:none}a:active,a:focus,a:hover{color:#06f;text-decoration:underline}h1 a:empty:before,h2 a:empty:before,h3 a:empty:before,h4 a:empty:before,h5 a:empty:before,h6 a:empty:before{content:#}h1 a:empty,h2 a:empty,h3 a:empty,h4 a:empty,h5 a:empty,h6 a:empty{visibility:hidden;padding-left:.25em}h1:hover a:empty,h2:hover a:empty,h3:hover a:empty,h4:hover a:empty,h5:hover a:empty,h6:hover a:empty{visibility:visible}img{max-width:100%}figure{margin:1em 0;text-align:center}figcaption{font-size:small}code,kbd,pre,samp{color:#009;font-family:monospace,monospace}pre kbd{color:#060}blockquote,pre{background:#eee;padding:.5em}pre{overflow:auto}blockquote{border-left:medium solid #ccc;margin:1em 0}blockquote :first-child{margin-top:0}blockquote :last-child{margin-bottom:0}table{border-collapse:collapse}td,th{border:thin solid #999;padding:.3em .4em;text-align:left}@media (prefers-color-scheme:dark){body{color:#bbb;background:#222}a:link,a:visited{color:#9bf}a:active,a:hover{color:#acf}code,kbd,pre,samp{color:#6cf}pre kbd{color:#9c9}blockquote,pre{background:#111}blockquote{border-color:#444}td,th{border-color:#666}}</style>" :: Text))])
                   -- }
                        -- ("css", "./main.css")
                         -- ast -- >>= handleError
                          
    -- add some information to the header that blaze html neglected to
    augmentBlaze :: H.Html -> H.Html
    augmentBlaze html =
      H.docTypeHtml $ do
      H.header $ do
        H.title "title"
      H.body $ html

-- initial css source: https://github.com/susam/spcss/blob/master/sp.min.css
