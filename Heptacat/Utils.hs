module Heptacat.Utils where

import Data.String.Utils (split)
import System.Posix.Directory (changeWorkingDirectory, getWorkingDirectory)
import System.Process (runInteractiveProcess)
import System.IO

gitUrl2Dir :: String -> FilePath
gitUrl2Dir url =
  removeDotGit $
  last $ split "/" $ last $ split ":" $
  reverse $ dropWhile (=='/') $ reverse $
  url
  where
    removeDotGit :: String -> String
    removeDotGit str =
      reverse $
      (\x -> if take 4 x == "tig." then drop 4 x else x)$
      reverse str


defaultProjectFileName :: FilePath
defaultProjectFileName = "project.yml"


withWorkingDirectory :: FilePath -> IO a -> IO a
withWorkingDirectory dir io = do
  oldwd <- getWorkingDirectory
  changeWorkingDirectory dir
  ret <- io
  changeWorkingDirectory oldwd
  return ret

gitCommitId :: IO String
gitCommitId = do
  (_,hOut,_,hProc) <- runInteractiveCommand "git log | head -n 1"
  str <- hGetContents hOut
  waitForProcess hProc
  retturn $ words str !! 1

gitAtomically :: IO a -> IO a
gitAtomically io = do
  oldCommitId <- gitCommitId
