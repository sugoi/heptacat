module Heptacat.Main.Init where

import           Control.Lens ((^.))
import qualified Data.ByteString.Char8 as BS
import qualified Data.Yaml as Yaml
import           System.IO
import           System.FilePath ((</>))
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
  let prepareDir dirLens desc = do
        createDirectoryIfMissing True $ proj ^. recordRepo.dirLens
        writeFile ((proj ^. recordRepo.dirLens) </> "README") desc
  prepareDir taskListDir    "# task list are placed here."
  prepareDir progressDir    "# the progress of each task file will be put here."
  prepareDir workerStateDir "# the log of each worker will be here."
  prepareDir resultDir      "# the results will go here."



