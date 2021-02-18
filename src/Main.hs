{-#LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module Main where

import Text.Pandoc
import qualified Data.Text as T
import Universum

main :: IO ()
main = do
  ast <- runIO $ readOrg def (T.pack "[[https://google.com][url]]")
  properAst <- modifyAst ast
  result <- runIO (writeHtml5String def properAst) >>= handleError
  putStrLn result

modifyAst :: Either PandocError Pandoc -> IO Pandoc
modifyAst ast = return $ transformDoc $ fromRight (error "bad") ast

transformDoc :: Pandoc -> Pandoc
transformDoc a = a

-- TODO: create my own pandoc exporter for /semantic/ html (+ my css!) and with lucid.
-- for now, though, this is good.
