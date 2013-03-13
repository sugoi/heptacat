import qualified Data.Yaml as Yaml
import qualified Data.ByteString.Char8 as BS

import Heptacat.Options 
import Heptacat.Type.IO


main :: IO ()
main = do
  print $ myOptions
  lab <- getLaboratory
  print lab
  BS.putStrLn $ Yaml.encode lab
  

