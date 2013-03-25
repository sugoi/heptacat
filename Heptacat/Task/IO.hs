module Heptacat.Task.IO where

import           Control.Lens ((^.))
import qualified Control.Lens as Lens
import           Control.Monad (forM)
import           Data.Char (isSpace)
import qualified Data.Map as Map
import           System.FilePath ((</>))
import qualified System.IO.Strict as Strict

import Heptacat.Main.ProjectConfig
import Heptacat.Project
import Heptacat.Task
import Heptacat.Utils (gitUrl2Dir, pipeFromSuccess, nonCommentLines)
 


getTaskList :: IO [Task]
getTaskList = do
  let recoDir = gitUrl2Dir $ myProjectConfig ^. recordRepo.url
      tlDir   = recoDir </>  (myProjectConfig ^. recordRepo.taskListDir)
  files <- pipeFromSuccess "ls" [tlDir]
  tasks00 <- fmap concat $ forM (filter (/= "README") $ lines files) $ \taskFn -> do
    con <- fmap nonCommentLines $ Strict.readFile $ tlDir </> taskFn
    let parseTask :: String -> Task
        parseTask str = 
          let (_  ,rest1) = span isSpace         str
              (ref,rest2) = span (not . isSpace) rest1
              cmdarg      = dropWhile isSpace rest2
          in Task ref cmdarg Map.empty
        tasks0 = map parseTask con
    return tasks0
  print tasks00
  return []