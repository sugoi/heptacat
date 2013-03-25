module Heptacat.Main.WorkerConfig where

import           Control.Applicative
import qualified Control.Lens as Lens
import qualified Data.ByteString.Char8 as BS
import qualified Data.Yaml as Yaml
import           System.IO.Unsafe


import Heptacat.Options
import Heptacat.Project
import Heptacat.Utils

{-# NOINLINE myWorkerConfig #-}
myWorkerConfig :: Project
myWorkerConfig = unsafePerformIO $ do
  let projFn = projectFileName myCmdLineOptions               
  parsePF <- Yaml.decode <$> BS.readFile projFn
  projConfig0 <- maybe (error $ projFn ++ " : no parse.") return parsePF
  let newName = workerName myCmdLineOptions
      projConfig 
        | newName /= "" 
           = Lens.set workerNameInCharge newName projConfig0 
        | otherwise = projConfig0
  return projConfig