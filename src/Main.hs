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
import System.FilePath
import Text.Pandoc.Builder
import System.Directory
import Wikigen.File.Utils (addNDirectory, addDirectory)

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
  html <- unparseHtml $ generateHomePage args
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
  result <- unparseHtml modAst
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
unparseHtml :: Pandoc -> IO T.Text
unparseHtml ast = runIO (writeHtml5String def ast) >>= handleError
