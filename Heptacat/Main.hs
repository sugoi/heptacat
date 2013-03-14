module Heptacat.Main where


import Heptacat.Options 

import qualified Heptacat.Main.Init as Init
import qualified Heptacat.Main.Worker as Worker

main :: IO ()
main = 
  case myOptions of
    Init{} -> Init.main
    Worker{} -> Worker.main
    _ -> putStrLn "aieee!"
