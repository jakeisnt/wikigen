{-#LANGUAGE DeriveGeneric, OverloadedStrings, DataKinds, TypeOperators, NoImplicitPrelude #-}

module Main where

import Text.Pandoc
import qualified Data.Text as T
import Options.Generic -- use that harg library later!
import System.FilePath.Find as F
import System.FilePath
import System.FilePath.GlobPattern (GlobPattern)
import Universum

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

data Metadata = Metadata { fileLinks :: [(Text, Text)] } deriving (Generic, Show)
mergeMetadata :: Metadata -> Metadata -> Metadata
mergeMetadata m1 m2 = Metadata { fileLinks = fileLinks m1 ++ fileLinks m2 }

mapMetadata :: [Metadata] -> Metadata
mapMetadata ms = foldl' mergeMetadata mtMeta ms 
  where mtMeta = Metadata { fileLinks = [] }

-- make sure the directory exists
-- throw an error otherwise
-- ensureDirectoryExists :: FilePath -> ()
-- ensureDirectoryExists s = if not $ isDirectory s then error "directory does not exist" else ()

-- finds files matching a pattern in a directory
search :: GlobPattern -> FilePath -> IO [String]
search pat = F.find always (fileName ~~? pat)

-- scan an entire wiki and output its html representation
-- https://rosettacode.org/wiki/Walk_a_directory/Recursively#Haskell
generateWiki :: FilePath -> IO ()
generateWiki s = do
  files <- search "*.org" s
  mapM_ generateWikiFile files

(|>) :: a -> (a -> b) -> b
(|>) arg function = function arg
  
-- Read a wiki file and output its html representation
generateWikiFile :: FilePath -> IO ()
generateWikiFile s = do
  fileText <- readFile s
  ast <- parseOrg fileText
  metadata <- return $ getMetadata [ast]
  modAst <- return $ modifyAst metadata ast
  result <- unparseHtml modAst
  let
    outFileName = s |> takeFileName |> (\a -> replaceExtension a ".html");
    outFilePath = s |> takeDirectory |> takeDirectory |> (\a -> a ++ "/public");
  writeFile (outFilePath </> outFileName) result

-- Extracts globally relevant metadata from pandoc objects
getMetadata :: [Pandoc] -> Metadata
getMetadata docs = mapMetadata $ map (\(Pandoc meta block) -> (mapMetadata $ map getBlockMetadata block)) docs
  where
    mtMeta = Metadata { fileLinks = [] }
    
    -- TODO: this format is highly dependent on the file format. see what Org needs!
    getMetaMetadata :: Meta -> Metadata
    getMetaMetadata metamap = mtMeta

    -- get metadata from a Block
    getBlockMetadata :: Block -> Metadata
    getBlockMetadata b = case b of
      Plain p -> mapMetadata $ map getInlineMetadata p
      Para p -> mapMetadata $ map getInlineMetadata p
      LineBlock p -> mapMetadata $ map (mapMetadata . map getInlineMetadata) p
      
      CodeBlock attr text -> mtMeta
      RawBlock fmt txt -> mtMeta
      BlockQuote blocks -> mapMetadata $ map getBlockMetadata blocks
      OrderedList lmeta blocks -> mapMetadata $ map (mapMetadata . (map getBlockMetadata)) blocks
      BulletList blocks -> mapMetadata $ map (mapMetadata . (map getBlockMetadata)) blocks
      DefinitionList inlineBlocks -> mtMeta -- get definitions, enable referencing them elsewhere in wiki? how is that difference from a quoteback?
      Header level attrs txts -> mtMeta
      Table attr cap cols thead tbodies tfoot -> mtMeta
      Div attr blocks -> mapMetadata $ map getBlockMetadata blocks
      
      HorizontalRule -> mtMeta -- not sure what this is!
      Null -> mtMeta

    -- get metadata from an Inline
    getInlineMetadata :: Inline -> Metadata
    getInlineMetadata inline = case inline of
      Str t -> mtMeta
      Emph inlines -> mapMetadata $ map getInlineMetadata inlines
      Underline inlines -> mapMetadata $ map getInlineMetadata inlines
      Strong inlines -> mapMetadata $ map getInlineMetadata inlines
      Strikeout inlines -> mapMetadata $ map getInlineMetadata inlines
      Superscript inlines -> mapMetadata $ map getInlineMetadata inlines
      Subscript inlines -> mapMetadata $ map getInlineMetadata inlines
      SmallCaps inlines -> mapMetadata $ map getInlineMetadata inlines
      Quoted qtype inlines -> mapMetadata $ map getInlineMetadata inlines
      Cite citations inlines -> mapMetadata $ map getInlineMetadata inlines
      Code attr txt -> mtMeta
      Math mtyp txt -> mtMeta -- tex math literal
      RawInline fmt txt -> mtMeta
      Link attr inlines tgt -> mapMetadata $ map getInlineMetadata inlines -- (alt text, list of inlines in text label, link target)
      Image attr inlines target -> mapMetadata $ map getInlineMetadata inlines -- same as link
      Note blocks -> mapMetadata $ map getBlockMetadata blocks
      Span attr inlines -> mapMetadata $ map getInlineMetadata inlines -- generic inline container; unused by org?

      Space -> mtMeta
      SoftBreak -> mtMeta
      LineBreak -> mtMeta

    -- get metadata from an attribute
    getAttrMetadata :: Attr -> Metadata
    getAttrMetadata (ident, classes, kvs) = mtMeta


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
