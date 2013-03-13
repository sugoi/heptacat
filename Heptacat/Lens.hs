module Heptacat.Lens where

import Control.Lens
import Control.Lens.TH

myLensRules :: LensRules
myLensRules =   lensRules 
  & lensField .~ (Just . (++"L"))