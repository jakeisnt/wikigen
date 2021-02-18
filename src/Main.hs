{-#LANGUAGE DeriveGeneric, OverloadedStrings, DataKinds, TypeOperators #-}

module Main where

import Text.Pandoc
import qualified Data.Text as T
import Data.Either (fromRight)
import Options.Generic -- use that harg library later!
import System.Directory
import System.FilePath.Find
import System.FilePath.GlobPattern (GlobPattern)

-- cli options
data CliOpts = Generate { dirPath :: FilePath }
          | Analyze { } deriving (Generic, Show)

instance ParseRecord CliOpts

main :: IO ()
main = do
    cli <- getRecord "wiki"
    case cli of
      Generate _ -> generateWiki $ dirPath cli
      Analyze -> putStrLn "analyzing beep beep boop"

-- metadata collected from teh wiki
data Metadata = Metadata { links :: Maybe (String, String) } deriving (Generic, Show)

-- make sure the directory exists
-- throw an error otherwise
-- ensureDirectoryExists :: FilePath -> ()
-- ensureDirectoryExists s = if not $ isDirectory s then error "directory does not exist" else ()

-- finds files matching a pattern in a directory
search :: GlobPattern -> FilePath -> IO [String]
search pat = find always (fileName ~~? pat)

-- scan an entire wiki and output its html representation
-- https://rosettacode.org/wiki/Walk_a_directory/Recursively#Haskell
generateWiki :: FilePath -> IO ()
generateWiki s = do
  files <- search "*.org" s
  mapM_ generateWikiFile files
  
-- Read a wiki file and output its html representation
generateWikiFile :: FilePath -> IO ()
generateWikiFile s = do
  ast <- parseOrg $ T.pack s
  metadata <- return $ getMetadata [ast]
  modAst <- return $ modifyAst metadata ast
  result <- unparseHtml modAst
  putStrLn $ T.unpack result

-- Extracts globally relevant metadata from pandoc objects
getMetadata :: [Pandoc] -> Metadata
getMetadata docs = Metadata {links = Just ("a", "b")}

-- Transform the Pandoc document AST
modifyAst :: Metadata -> Pandoc -> Pandoc
modifyAst metadata ast = ast

-- parse an Org file to its Pandoc representation
-- handle errors (currently in a bad way)
parseOrg :: T.Text -> IO Pandoc
parseOrg t = do
  maybeFile <- runIO $ readOrg def t
  return $ fromRight (error "bad") maybeFile

-- write Html to a text buffer
unparseHtml :: Pandoc -> IO T.Text
unparseHtml ast = runIO (writeHtml5String def ast) >>= handleError
