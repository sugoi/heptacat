module Heptacat.Main.Worker where


import           Control.Applicative
import           Control.Lens ((^.))
import qualified Control.Lens as Lens
import           Control.Monad
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Yaml as Yaml
import           System.Cmd (system, rawSystem)
import           System.Directory (doesDirectoryExist)
import           System.Exit
import           System.FilePath ((</>))
import           Text.Printf

import Heptacat.Options
import Heptacat.Project
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
  print myOptions
  (Just projConfig0) <- Yaml.decode <$> BS.readFile (projectFileName myOptions)
  let newName = workerName myOptions
      projConfig 
        | newName /= "" 
           = Lens.set workerNameInCharge newName projConfig0 
        | otherwise = projConfig0
        
  BSL.putStrLn $ Aeson.encode projConfig
  when (null $ projConfig ^. workerNameInCharge) $ do
    putStrLn "worker name cannot be obtained neither from configure file nor command line options."        
    exitFailure

  prepareCloneRepo $ projConfig ^. subjectRepo . url
  prepareCloneRepo $ projConfig ^. recordRepo  . url
  let subjDir = gitUrl2Dir $ projConfig ^. subjectRepo . url
      recoDir = gitUrl2Dir $ projConfig ^. recordRepo . url
  withWorkingDirectory recoDir $ do      
    system $ printf "git config user.name %s" (projConfig ^. workerNameInCharge)
    system "git config  merge.tool heptacat"
    system $ "git config  mergetool.heptacat.cmd "
          ++ "'heptacat merge \"$BASE\" \"$LOCAL\" \"$REMOTE\" \"$MERGED\"'"
    system "git config mergetool.trustExitCode false"
    gitAtomically $ do
      return ()