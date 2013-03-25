module Heptacat.Task.IO where

import           Control.Lens ((^.))
import qualified Control.Lens as Lens
import           Control.Monad (forM)
import           Data.Char (isSpace)
import qualified Data.Map as Map
import           Data.Maybe
import           System.FilePath ((</>))
import qualified System.IO.Strict as Strict

import Heptacat.Main.ProjectConfig
import Heptacat.Project
import Heptacat.Task
import Heptacat.Utils 
  (gitUrl2Dir, pipeFromSuccess, nonCommentLines,
   md5, pipeFromFinish, wordsN)
 
data TaskPreference = 
    TaskPreference
      { globalProgress :: Event ,
        localProgress  :: Event ,
        taskMD5 :: String
      }
  deriving (Eq, Show)


getTaskList :: IO [Task]
getTaskList = do
  let recoDir = gitUrl2Dir $ myProjectConfig ^. recordRepo.url
      tlDir   = recoDir </>  (myProjectConfig ^. recordRepo.taskListDir)
      prDir   = recoDir </>  (myProjectConfig ^. recordRepo.progressDir)
  files <- pipeFromSuccess "ls" [tlDir]
  files2 <- pipeFromFinish "ls" [prDir]  
  print $ "progress files : " ++ files2
  progs00 <- fmap concat $ forM (filter (/= "README") $ lines files2) $
    \progFn -> do
      con <- fmap nonCommentLines $ Strict.readFile $ prDir </> progFn
      return $ mapMaybe decodeEvent con      

  print progs00      
  mapM_ print progs00      

  tasks00 <- fmap concat $ forM (filter (/= "README") $ lines files) $
    \taskFn -> do
      con <- fmap nonCommentLines $ Strict.readFile $ tlDir </> taskFn
      let parseTask :: String -> Task
          parseTask str = 
            let ((ref,cmdarg):_) = wordsN str
            in Task (TaskKey ref cmdarg) taskFn Map.empty
          tasks0 = map parseTask con
      return tasks0
  return tasks00