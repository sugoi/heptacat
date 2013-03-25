module Heptacat.Main where

import           Control.Monad

import           Heptacat.Options 
import qualified Heptacat.Main.Init as Init
import qualified Heptacat.Main.Worker as Worker
import qualified Heptacat.Main.Merge as Merge

main :: IO ()
main = 
  case myCmdLineOptions of
    Init{} -> Init.main
    Worker{} -> Worker.main
    Diff  argv -> mapM_ putStrLn $ "heptacat invoked with diff mode.": argv
    Merge argv -> Merge.main
    _ -> putStrLn $ "unknown cmdline option : " ++ show myCmdLineOptions
