module Heptacat.Main where


import Heptacat.Options 
import Heptacat.Type.IO

import qualified Heptacat.Main.Init as Init

main :: IO ()
main = 
  case myOptions of
    Init{} -> Init.main
    Worker{} -> putStrLn "hashi-re!"
    _ -> putStrLn "aieee!"
