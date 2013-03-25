{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RankNTypes #-}

module Heptacat.Project.IO where


import qualified Control.Lens as L
import           Control.Monad (when)
import qualified Control.Monad.State as S
import           Control.Monad.IO.Class (liftIO)
import           Data.Default
import           Heptacat.Project
import           System.IO
import           System.Console.CmdArgs.Text
import           Text.Printf



type ProjIO = S.StateT Project IO



getProject :: IO Project
getProject = S.execStateT ioProj def

askStr :: [String] -> String -> L.Lens' Project String -> ProjIO ()
askStr detail msg lens = do
  defVal <- L.use lens
  let msg' = if | defVal /= ""  -> printf "%s [default: %s] " msg defVal
                | otherwise     -> msg ++ " "

  input <- liftIO $ do
    when (detail /= []) $
      putStr $ showText defaultWrap [Line $ unwords detail]
    putStr msg'
    hFlush stdout
    getLine

  if | input  /= "" -> L.assign lens input
     | defVal /= "" -> L.assign lens defVal
     | otherwise    -> askStr [] msg lens

ioProj :: ProjIO Project
ioProj = do
  askStr ["I will now set up the contents of a heptacat record repository.",
          "A record repository contains the experiment project information",
          "as well as all the experimental results.",
          "The repository URL must be in the form that follows ``git clone'';",
          "e. g. git@server.addr:subject/repository/url ."]
    "record repository url?" $ recordRepo . url

  askStr ["Tasklist directory contains multiple files" ,
          "that each contains a list of tasks."]
    "tasklist directory?" $ recordRepo . taskListDir

  askStr ["Task progress directory contains files" ,
          "that corresponds to task files" ]
    "progress directory?" $ recordRepo . taskProgressDir

  askStr ["The worker-state directory will contain" ,
          "the history of the workers."]
    "worker state directory?" $ recordRepo . workerStateDir

  askStr ["The heptacat workers will collect the experimental results",
          "into the result directory of the record repository."]
    "result directory?" $ recordRepo . resultDir


  askStr ["A subject repository is the repository that contains the codes",
          "for the heptacat workers to perform experiments.",
          "The repository URL must be in the form that follows ``git clone'';",
          "e. g. git@server.addr:subject/repository/url ."]
    "subject repository url?" $ subjectRepo . url

  askStr ["The subject repository must contain a startup script." ,
          "All the workers execute the startup script," ,
          "with some command-line arguments if specified." ]
    "startup script filename?" $ subjectRepo . startUpScript

  askStr ["The startup script may produce a bunch of files." ,
          "The contents of the output directory" ,
          "will be collected as experimental results."]
    "output directory?" $ subjectRepo . outputDir

  S.get
