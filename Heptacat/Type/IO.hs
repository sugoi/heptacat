module Heptacat.Type.IO where


import qualified Control.Lens as L
import qualified Control.Monad.State as S
import qualified Control.Monad.State.Class as S
import           Data.Default
import           Heptacat.Type


type LaboIO = S.StateT Laboratory IO

getLaboratory :: IO Laboratory
getLaboratory = S.execStateT ioLabo def 

ioLabo :: LaboIO Laboratory
ioLabo = do
  L.assign (subjectRepo . url) "git@github.com/jk/test"
  S.get