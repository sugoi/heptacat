import Heptacat.Options 
import Heptacat.Type.IO

main :: IO ()
main = do
  print $ myOptions
  lab <- getLaboratory
  print lab

