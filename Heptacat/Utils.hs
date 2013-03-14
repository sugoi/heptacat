module Heptacat.Utils where

import Data.String.Utils (split)

gitUrl2Dir :: String -> FilePath
gitUrl2Dir url = last $ split "/" $ last $ split ":" url