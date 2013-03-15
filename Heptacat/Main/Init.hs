module Heptacat.Main.Init where

import           Control.Lens ((^.))
import qualified Data.ByteString.Char8 as BS
import qualified Data.Yaml as Yaml
import           System.IO
import           System.Directory 
  (createDirectoryIfMissing)

import Heptacat.Project
import Heptacat.Project.IO
import Heptacat.Utils(defaultProjectFileName)

main :: IO ()
main = do
  proj <- getProject
  withFile defaultProjectFileName WriteMode $ \h ->
    BS.hPutStrLn h $ Yaml.encode proj
  let mkdirP = createDirectoryIfMissing True 
  mkdirP $ proj ^. recordRepo.taskListDir
  mkdirP $ proj ^. recordRepo.workerStateDir
  mkdirP $ proj ^. recordRepo.resultDir



