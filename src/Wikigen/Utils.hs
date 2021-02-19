module Wikigen.Utils
       ((|>)
       ) where

import Universum


(|>) :: a -> (a -> b) -> b
(|>) arg function = function arg
