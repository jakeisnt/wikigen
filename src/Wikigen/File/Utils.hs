module Wikigen.File.Utils
       (swapDirectory, addNDirectory, addDirectory) where

import Control.Lens
import System.FilePath


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
  in path --- </> filename

addDirectory :: FilePath -> String -> FilePath
addDirectory = addNDirectory 0
