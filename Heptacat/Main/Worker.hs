module Heptacat.Main.Worker where


import           Control.Applicative
import           Control.Lens ((^.))
import qualified Control.Lens as Lens
import           Control.Monad
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import           Data.Maybe
import qualified Data.Yaml as Yaml
import           System.Cmd (system, rawSystem)
import           System.Directory (doesDirectoryExist)
import           System.Exit
import           System.FilePath ((</>))
import           Text.Printf

import Heptacat.Main.ProjectConfig
import Heptacat.Options
import Heptacat.Project
import Heptacat.Task.IO (getTaskList)
import Heptacat.Utils


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
  when (null $ myProjectConfig ^. workerNameInCharge) $ do
    putStrLn "worker name cannot be obtained neither from the configure file nor command line options."        
    exitFailure

  prepareCloneRepo $ myProjectConfig ^. subjectRepo . url
  prepareCloneRepo $ myProjectConfig ^. recordRepo  . url

  let subjDir = gitUrl2Dir $ myProjectConfig ^. subjectRepo . url
      recoDir = gitUrl2Dir $ myProjectConfig ^. recordRepo . url
  withWorkingDirectory recoDir $ do      
    system $ printf "git config user.name %s" $ myProjectConfig ^. workerNameInCharge
    system "git config  merge.tool heptacat"
    system $ "git config  mergetool.heptacat.cmd "
          ++ "'heptacat merge \"$BASE\" \"$LOCAL\" \"$REMOTE\" \"$MERGED\"'"
    system "git config mergetool.trustExitCode false"
    gitAtomically $ do
      return ()
  ts <- getTaskList
  print ts
