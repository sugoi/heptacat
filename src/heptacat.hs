import Heptacat.Options 
import Heptacat.Type.IO

main = do
  print $ myOptions
  lab <- getLaboratory
  print lab

