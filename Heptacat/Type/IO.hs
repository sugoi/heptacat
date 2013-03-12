module Heptacat.Type.IO where

import qualified Control.Lens as L
import qualified Control.Monad.State as S
import           Heptacat.Type



getLaboratory :: IO Laboratory
getLaboratory = return undefined