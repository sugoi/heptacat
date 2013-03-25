module Heptacat.Task.IO where

import           Control.Lens ((^.))
import qualified Control.Lens as Lens
import           System.FilePath ((</>))

import Heptacat.Main.ProjectConfig
import Heptacat.Project
import Heptacat.Task
import Heptacat.Utils (gitUrl2Dir, pipeFromSuccess)
 


getTaskList :: IO [Task]
getTaskList = do
  let recoDir = gitUrl2Dir $ myProjectConfig ^. recordRepo.url
      tlDir   = myProjectConfig ^. recordRepo.taskListDir
  files <- pipeFromSuccess "ls" [recoDir </> tlDir]
  putStrLn files
  return []