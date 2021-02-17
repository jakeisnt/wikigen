{-# LANGUAGE ScopedTypeVariables,TupleSections,OverloadedStrings, NoImplicitPrelude, DeriveAnyClass #-}

-- https://github.com/volhovm/orgstat/blob/master/src/OrgStat/IO.hs
module Wikigen.IO
  ( readOrgFile
  ) where

import           Universum

import qualified Data.Text                     as T
import qualified Data.Text.IO                  as TIO
import           System.Directory               ( doesFileExist )
import           System.FilePath                ( takeBaseName
                                                , takeExtension
                                                )

import           Wikigen.Ast                            ( Org )
import           Wikigen.Parser                         ( runParser )
import qualified Text.Show

data OrgIOException
    = OrgIOException Text
      -- ^ All exceptions related to reading files
    | ExternalException Text
      -- ^ Failed to run some external app (gpg)
    deriving (Typeable, Exception)

instance Text.Show.Show OrgIOException where
  show (OrgIOException    r) = "IOException: " <> T.unpack r
  show (ExternalException r) = "ExternalException: " <> T.unpack r

readOrgFile :: (MonadIO m, MonadCatch m) => FilePath -> m (Text, Org)
readOrgFile fp = do
  -- reading the org file
  -- logDebug $ "Reading org file" <> fpt
  -- ensure file exists
  unlessM (liftIO $ doesFileExist fp)
    $  throwM
    $  OrgIOException
    $  "Org file "
    <> fpt
    <> " doesn't exist"

  -- read file, encrypt or reject
  (content, fname) <- case takeExtension fp of
    ".org" -> (, fp) <$> liftIO (TIO.readFile fp)
    _ ->
      throwM
        $  OrgIOException
        $  "File "
        <> fpt
        <> " has unknown extension. Must be .org!"

  -- get filename
  let filename = T.pack $ takeBaseName fname
  -- first argument is all of the TODO strings for todo notes
  parsed <- runParser ["TODO"] content
  pure (filename, parsed)
  where fpt = T.pack fp
