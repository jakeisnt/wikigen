{-#LANGUAGE DeriveGeneric, OverloadedStrings, DataKinds, TypeOperators #-}

module Main where

import Text.Pandoc
import qualified Data.Text as T
import Data.Either (fromRight)
import Options.Generic -- use that harg library later!

data Wiki = Generate { dirPath :: FilePath }
          | Analyze { } deriving (Generic, Show)

instance ParseRecord Wiki

main :: IO ()
main = do
    cli <- getRecord "wiki"
    case cli of
      Generate _ -> generateWikiFile $ dirPath cli
      Analyze -> putStrLn "analyzing beep beep boop"
      
-- Read a wiki file and output its html representation
generateWikiFile :: FilePath -> IO ()
generateWikiFile s = do
  ast <- parseOrg $ T.pack s
  modAst <- modifyAst ast
  result <- unparseHtml modAst
  putStrLn $ T.unpack result

-- Transform the Pandoc document AST
modifyAst :: Pandoc -> IO Pandoc
modifyAst ast = return ast

-- parse an Org file to its Pandoc representation
-- handle errors (currently in a bad way)
parseOrg :: T.Text -> IO Pandoc
parseOrg t = do
  maybeFile <- runIO $ readOrg def t
  return $ fromRight (error "bad") maybeFile

unparseHtml :: Pandoc -> IO T.Text
unparseHtml ast = runIO (writeHtml5String def ast) >>= handleError

-- -- Removes the either
-- fromRight :: a -> Either b a -> a
-- fromRight dflt mb = case mb of
--   Right answer -> answer
--   Left _ -> dflt
