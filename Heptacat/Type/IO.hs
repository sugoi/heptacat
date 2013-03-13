{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RankNTypes #-}

module Heptacat.Type.IO where


import qualified Control.Lens as L
import qualified Control.Monad.State as S
import           Control.Monad.IO.Class (liftIO)
import           Data.Default
import           Heptacat.Type
import           System.IO
import           Text.Printf

type LaboIO = S.StateT Laboratory IO

getLaboratory :: IO Laboratory
getLaboratory = S.execStateT ioLabo def 

askStr :: String -> L.Lens' Laboratory String -> LaboIO ()
askStr msg lens = do
  defVal <- L.use lens
  let msg' = if | defVal /= ""  -> printf "%s [default: %s] " msg defVal
                | otherwise     -> msg ++ " "

  input <- liftIO $ do
    putStr msg'
    hFlush stdout
    getLine

  if | input  /= "" -> L.assign lens input
     | defVal /= "" -> L.assign lens defVal
     | otherwise    -> askStr msg lens

ioLabo :: LaboIO Laboratory
ioLabo = do
  askStr "subject repository url?" $ subjectRepo . url
  askStr "output directory?" $ subjectRepo . outputDir
  askStr "startup script filename?" $ subjectRepo . startUpScript
  askStr "record repository url?" $ recordRepo . url
  askStr "tasklist directory?" $ recordRepo . taskListDir
  askStr "worker state directory?" $ recordRepo . workerStateDir
  askStr "result directory?" $ recordRepo . resultDir
  S.get
