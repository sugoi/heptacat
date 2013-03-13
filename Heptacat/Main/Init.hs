module Heptacat.Main.Init where

import qualified Data.ByteString.Char8 as BS
import qualified Data.Yaml as Yaml
import           System.IO

import Heptacat.Type.IO

main :: IO ()
main = do
  proj <- getProject
  withFile "project.yml" WriteMode $ \h ->
    BS.hPutStrLn h $ Yaml.encode proj
  

