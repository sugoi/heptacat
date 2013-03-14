module Heptacat.Utils where

import Data.String.Utils (split)

gitUrl2Dir :: String -> FilePath
gitUrl2Dir url = 
  removeDotGit $
  last $ split "/" $ last $ split ":" $
  reverse $ dropWhile (=='/') $ reverse $
  url
  where
    removeDotGit :: String -> String
    removeDotGit str = 
      reverse $
      (\x -> if take 4 x == "tig." then drop 4 x else x)$        
      reverse str


projectYamlFileName :: FilePath
projectYamlFileName = "project.yml"

