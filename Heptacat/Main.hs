module Heptacat.Main where

import           Control.Monad
import Heptacat.Options 
import qualified Heptacat.Main.Init as Init
import qualified Heptacat.Main.Worker as Worker

main :: IO ()
main = 
  case myOptions of
    Init{} -> Init.main
    Worker{} -> Worker.main
    Diff  argv -> mapM_ putStrLn argv
    Merge argv -> mapM_ putStrLn argv 
    _ -> putStrLn "aieee!"
