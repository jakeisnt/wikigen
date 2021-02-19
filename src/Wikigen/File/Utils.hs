module Wikigen.File.Utils
       (swapDirectory, addNDirectory, addDirectory, ensureDirsExist) where

import Control.Lens
import System.FilePath
import System.Directory

-- replace dirname at depth n with the string
swapNDirectory :: Int -> FilePath -> String -> FilePath
swapNDirectory depth fp dirname =
  let dirpath = dirname ++ "/"
      pathArr = reverse $ splitPath fp
      path = joinPath $ reverse $
        -- replace the target of a lens with a constant value
        -- compose with it a function for converting a traversible to a lens
        -- apply the lens to the array
        (element depth .~ dirpath) pathArr
  in path --- </> filename

swapDirectory = swapNDirectory 0

-- add a directory at the nth index 
addNDirectory :: Int -> FilePath -> String -> FilePath
addNDirectory depth fp dirname =
  let dirpath = dirname ++ "/"
      pathArr = reverse $ splitPath fp
      -- move the lens to select the nth item of the list
      _drop 0 = id
      _drop n = _tail . _drop (n - 1)
      -- use lens to drop from list until reaching nth item
      -- run a function at the top of the list (concat the string to the list here)
      -- exit lens mode
      pr = pathArr & (_drop depth) %~ (dirpath:)
      path = joinPath $ reverse $ pr
  in path 

addDirectory :: FilePath -> String -> FilePath
addDirectory = addNDirectory 0

ensureDirExists :: FilePath -> IO ()
ensureDirExists fp = do
  exists <- doesDirectoryExist fp
  if   not exists
  then return ()
  else createDirectory fp
                    
-- ensure all of the paths 
ensureDirsExist :: FilePath -> IO ()
ensureDirsExist fp = do
  let splitFp = splitPath fp;
      -- [path, path two, path three four]
      enumFp = map (\n -> concat $ take n splitFp) [1..(length splitFp)];
  mapM_ ensureDirExists enumFp
