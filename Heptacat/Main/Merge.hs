module Heptacat.Main.Merge where

import qualified Heptacat.Options as Opt

main :: IO ()
main = do
  let (Opt.Merge argv) = Opt.myCmdLineOptions
  mapM_ putStrLn $ "heptacat invoked with merge mode.": argv 
