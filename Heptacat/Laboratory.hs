{-# LANGUAGE TemplateHaskell #-}

module Heptacat.Laboratory where

import Control.Lens.TH

data Laboratory = Laboratory
  { _recordRepoUrl :: String,
    _subjectRepoUrl :: String,
    _taskListDir :: FilePath,
    _workerStateDir :: FilePath,
    _resultDir :: FilePath
  }

$(makeLenses ''Laboratory)