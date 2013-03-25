module Heptacat.Main.Worker where


import           Control.Applicative
import           Control.Lens ((^.))
import qualified Control.Lens as Lens
import           Control.Monad
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as BSL
import           Data.Function (on)
import qualified Data.List as List
import qualified Data.Map as Map
import           Data.Time (UTCTime, getCurrentTime)
import           System.Cmd (system, rawSystem)
import           System.Directory (doesDirectoryExist)
import           System.Exit
import           System.FilePath ((</>))
import           Text.Printf

import Heptacat.Main.ProjectConfig
import Heptacat.Options
import Heptacat.Project
import Heptacat.Task as Task
import Heptacat.Task.IO (getTaskList)
import Heptacat.Utils (gitUrl2Dir, md5, withWorkingDirectory, gitAtomically)

data Preference = 
     Preference { globalProgress :: WorkState ,
                  localProgress  :: WorkState ,
                  taskMD5 :: String
                }
  deriving (Eq, Ord, Show)

myWorkerName :: WorkerName
myWorkerName = myProjectConfig ^. workerNameInCharge

prefer :: Task -> (Preference, Task)
prefer t = (Preference gp lp ha, t)
  where
    ss = t ^. taskEvents
    gp = maximum $ (Intact:) $ map (^. workState) $ map snd $ Map.toList $ ss
    lp = maybe Intact id $ fmap (^. workState) $ Map.lookup myWorkerName $ ss
    ha = md5 $ (myWorkerName ++) $ show t


prepareCloneRepo :: String -> IO ()
prepareCloneRepo giturl = do
  let repoDir = gitUrl2Dir giturl
  rde <- doesDirectoryExist repoDir
  when (not rde) $ do
    _ <- rawSystem "git" ["clone", giturl]
    return ()
  _ <- system $ printf "cd %s; git pull" repoDir
  return ()

main :: IO ()
main = do        
  print myCmdLineOptions
  BSL.putStrLn $ Aeson.encode myProjectConfig
  when (null $ myWorkerName) $ do
    putStrLn "worker name cannot be obtained neither from the configure file nor command line options."        
    exitFailure

  prepareCloneRepo $ myProjectConfig ^. subjectRepo . url
  prepareCloneRepo $ myProjectConfig ^. recordRepo  . url

  let subjDir = gitUrl2Dir $ myProjectConfig ^. subjectRepo . url
      recoDir = gitUrl2Dir $ myProjectConfig ^. recordRepo . url
  withWorkingDirectory recoDir $ do      
    system $ printf "git config user.name %s" $ myWorkerName
    system "git config  merge.tool heptacat"
    system $ "git config  mergetool.heptacat.cmd "
          ++ "'heptacat merge \"$BASE\" \"$LOCAL\" \"$REMOTE\" \"$MERGED\"'"
    system "git config mergetool.trustExitCode false"
  ts <- getTaskList
  let sortedTaskList =
        map snd $ 
        List.sortBy (compare `on` fst) $ 
        map prefer $ ts
      (targetTask : _) = sortedTaskList ++ error "no task available."
  withWorkingDirectory recoDir $ do      
    gitAtomically $ do
      time <- getCurrentTime
      let progFn = 
            (myProjectConfig ^. recordRepo.progressDir) </> 
            (targetTask ^. taskFileName ++ ".by." ++ myWorkerName)
      appendFile progFn $ (++"\n") $
        encodeEvent myWorkerName (targetTask ^. taskKey )  (Event time Started)
    return ()

