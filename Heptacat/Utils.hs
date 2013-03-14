module Heptacat.Utils where

import Data.String.Utils (split)

gitUrl2Dir :: String -> FilePath
gitUrl2Dir url = 
  last $ split "/" $ last $ split ":" $
  reverse $ dropWhile (=='/') $ reverse $
  url

projectYamlFileName :: FilePath
projectYamlFileName = "project.yml"