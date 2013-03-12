{-# LANGUAGE TemplateHaskell #-}

module Heptacat.Repository where

import Control.Lens.TH

data Repository = Repository 
  { recordRepositoryUrl :: FilePath ,
    subjectRepositoryUrl :: FilePath        
  }

$(makeLenses ''Repository)