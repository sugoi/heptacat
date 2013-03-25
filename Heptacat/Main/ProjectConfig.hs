module Heptacat.Main.ProjectConfig where

import           Control.Applicative
import           Control.Lens ((^.))
import qualified Control.Lens as Lens
import           Control.Monad
import qualified Data.ByteString.Char8 as BS
import           Data.Char (isSpace)
import qualified Data.Yaml as Yaml
import           System.IO.Unsafe
import           Text.Printf

import Heptacat.Options
import Heptacat.Project
import Heptacat.Utils

{-# NOINLINE myProjectConfig #-}
myProjectConfig :: Project
myProjectConfig = unsafePerformIO $ do
  let projFn = projectFileName myCmdLineOptions               
  parsePF <- Yaml.decode <$> BS.readFile projFn
  projConfig0 <- maybe (fail $ projFn ++ " : no parse.") return parsePF
  let newName = workerName myCmdLineOptions
      projConfig 
        | newName /= "" 
           = Lens.set workerNameInCharge newName projConfig0 
        | otherwise = projConfig0
  when (any isSpace $ projConfig ^. workerNameInCharge ) $
    fail $ printf "Worker name %s must not contain spaces." $ projConfig ^. workerNameInCharge
  return projConfig