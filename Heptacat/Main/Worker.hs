module Heptacat.Main.Worker where


import           Control.Applicative
import           Control.Lens ((^.))
import           Control.Monad
import qualified Data.ByteString.Char8 as BS
import qualified Data.Yaml as Yaml
import           System.Cmd (system, rawSystem)
import           System.Directory (doesDirectoryExist)
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
  (Just projConfig) <- Yaml.decode <$> BS.readFile (projectFileName myOptions)
  prepareCloneRepo $ projConfig ^. subjectRepo . url
  prepareCloneRepo $ projConfig ^. recordRepo  . url
