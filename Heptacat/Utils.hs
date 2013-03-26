{-# LANGUAGE OverloadedStrings #-}

module Heptacat.Utils where

import           Control.Monad
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as BSL
import           Data.Char (isSpace)
import qualified Data.Hash.MD5 as MD5
import           Data.Monoid ((<>))
import           Data.String.Utils (split)
import           Safe (headMay)
import           System.Cmd.Utils (pipeFrom, forceSuccess)
import           System.Posix.Directory (changeWorkingDirectory, getWorkingDirectory)
import           System.Process
import           System.IO


-- | decode a singleton value (which is, strictly speaking,
--   not allowed at the top level) out of JSON.

decodeA1 :: Aeson.FromJSON a => BSL.ByteString -> Maybe a
decodeA1 str = (headMay =<<) $ Aeson.decode $ "[" <> str <> "]"

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

gitReflog :: IO String
gitReflog = do
  (_,hOut,_,hProc) <- runInteractiveCommand "git log | head -n 1"
  str <- hGetContents hOut
  waitForProcess hProc
  return $ words str !! 1

gitAtomically :: IO a -> IO a
gitAtomically doSomething = do
  _ <- system "git pull origin master"            
  oldReflog <- gitReflog
  putStrLn oldReflog
  ret <- doSomething
  _ <- system "git push origin master"            
  return ret
-- git reset --hard reflog


pipeFromSuccess :: FilePath -> [String] -> IO String
pipeFromSuccess path argv = do
  (ph, ret)<- pipeFrom path argv
  forceSuccess ph
  return ret

pipeFromFinish :: FilePath -> [String] -> IO String
pipeFromFinish path argv = do
  (_, ret)<- pipeFrom path argv
  return ret


nonCommentLines :: String -> [String]
nonCommentLines = filter (not . isComment) . lines
  where
    isComment str = let xs = dropWhile isSpace str in
      case xs of
        ""      -> True
        ('#':_) -> True
        _       -> False

md5 :: Show a => a -> String
md5 = MD5.md5s . MD5.Str . show

-- obtain non-whitespace segments and the rest
wordsN :: String -> [(String, String)]
wordsN = f . dropWhile isSpace
  where
    f [] = repeat ("","")
    f str = (hd, rest3) : f rest3
      where
        (hd, rest2) = span (not.isSpace) str
        (_ , rest3) = span isSpace rest2
