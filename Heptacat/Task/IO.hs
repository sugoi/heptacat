module Heptacat.Task.IO where

import           Control.Lens ((^.))
import           Control.Monad (forM)
import qualified Data.Map as Map
import           Data.Maybe
import           System.FilePath ((</>))
import qualified System.IO.Strict as Strict

import Heptacat.Main.ProjectConfig
import Heptacat.Project
import Heptacat.Task
import Heptacat.Utils 
  (gitUrl2Dir, pipeFromSuccess, nonCommentLines,
   pipeFromFinish, wordsN)
 
data TaskPreference = 
    TaskPreference
      { globalProgress :: Event ,
        localProgress  :: Event ,
        taskMD5 :: String
      }
  deriving (Eq, Show)


type TWEMap = Map.Map TaskKey (Map.Map WorkerName Event)

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

  let mapProgs :: TWEMap
      mapProgs = foldr go Map.empty progs00

      go :: (WorkerName, TaskKey, Event) -> TWEMap -> TWEMap
      go (wn, tk, ev) = Map.alter inner tk
        where
          inner Nothing = Just $ Map.singleton wn ev
          inner (Just map1) = Just $ Map.alter inner2 wn map1

          inner2 Nothing = Just ev
          inner2 (Just ev2) 
            | ev ^. workState > ev2 ^. workState = Just ev
            | otherwise                          = Just ev2


  tasks00 <- fmap concat $ forM (filter (/= "README") $ lines files) $
    \taskFn -> do
      con <- fmap nonCommentLines $ Strict.readFile $ tlDir </> taskFn
      let parseTask :: String -> Task
          parseTask str = 
            let ((ref,cmdarg):_) = wordsN str
                tk = TaskKey ref cmdarg
            in Task tk taskFn $
                maybe Map.empty id (Map.lookup tk mapProgs)
          tasks0 = map parseTask con
      return tasks0
  return tasks00