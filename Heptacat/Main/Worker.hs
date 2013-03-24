module Heptacat.Main.Worker where


import           Control.Applicative
import           Control.Lens ((^.))
import qualified Control.Lens as Lens
import           Control.Monad
import qualified Data.ByteString.Char8 as BS
import           Data.Maybe
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
  parsePF <- Yaml.decode <$> BS.readFile (projectFileName myOptions)
  projConfig0 <- maybe (error $ projectFileName myOptions ++ " : no parse.") return parsePF

  let newName = workerName myOptions
      projConfig 
        | newName /= "" 
           = Lens.set workerNameInCharge newName projConfig0 
        | otherwise = projConfig0

  when (null $ projConfig ^. workerNameInCharge) $ do
    putStrLn "worker name cannot be obtained neither from configure file nor command line options."        
    exitFailure

  prepareCloneRepo $ projConfig ^. subjectRepo . url
  prepareCloneRepo $ projConfig ^. recordRepo  . url
  let subjDir = gitUrl2Dir $ projConfig ^. subjectRepo . url
      recoDir = gitUrl2Dir $ projConfig ^. recordRepo . url
  withWorkingDirectory recoDir $ do      
    gitAtomically $ do
      return ()