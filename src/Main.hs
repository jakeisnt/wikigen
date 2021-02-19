{-#LANGUAGE DeriveGeneric, OverloadedStrings, DataKinds, TypeOperators, NoImplicitPrelude #-}

module Main where

import Text.Pandoc

import qualified Data.Text as T
import Options.Generic -- use that harg library later!
import System.FilePath.Find as F
import Universum
import System.FilePath.GlobPattern (GlobPattern)
import Wikigen.Metadata (getMetadata)
import Wikigen.Transform (modifyAst)
import System.FilePath
import Wikigen.File.Utils (swapDirectory, addNDirectory, addDirectory)
-- import Wikigen.Utils((|>))
import Control.Lens

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

-- scan an entire wiki and output its html representation
-- https://rosettacode.org/wiki/Walk_a_directory/Recursively#Haskell
-- presumes this structure:

-- root (provided path)
--  | - Journals
--  | - WikiPages
generateWiki :: FilePath -> IO ()
generateWiki s = do
  journalFiles <- search "*.org" $ addDirectory s "journals"
  pageFiles <- search "*.org" $ addDirectory s "pages"
  mapM_ generateWikiFile (pageFiles ++ journalFiles)

-- Read a wiki file and output its html representation
generateWikiFile :: FilePath -> IO ()
generateWikiFile fp = do
  fileText <- readFile fp
  ast <- parseOrg fileText
  metadata <- return $ getMetadata [ast]
  modAst <- return $ modifyAst metadata ast
  result <- unparseHtml modAst
  let
    outFilePath = (addNDirectory 1 fp "public") -<.> ".html";
  putStrLn fp
  putStrLn (addNDirectory 2 fp "public")
  writeFile outFilePath result

-- parse an Org file to its Pandoc representation
-- handle errors (currently in a bad way)
parseOrg :: T.Text -> IO Pandoc
parseOrg t = do
  maybeFile <- runIO $ readOrg def t
  return $ fromRight (error "bad") maybeFile

-- write Html to a text buffer
unparseHtml :: Pandoc -> IO T.Text
unparseHtml ast = runIO (writeHtml5String def ast) >>= handleError
