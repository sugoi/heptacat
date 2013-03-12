{-# LANGUAGE TemplateHaskell #-}

module Heptacat.Type where

import Control.Lens
import Control.Lens.TH

class HasUrl t where
  url :: Simple Lens t String

data SubjectRepo = SubjectRepo
  { _subjectRepoUrl :: String,
    _outputDir :: FilePath ,
    _startUpScript :: FilePath}

$(makeLenses ''SubjectRepo)

instance HasUrl SubjectRepo where
  url = subjectRepoUrl

data RecordRepo = RecordRepo
  {    
    _recordRepoUrl :: String,
    _taskListDir :: FilePath,
    _workerStateDir :: FilePath,
    _resultDir :: FilePath 
  }

$(makeLenses ''RecordRepo)

instance HasUrl RecordRepo where
  url = recordRepoUrl

data Laboratory = Laboratory
  {
    _subjectRepo :: SubjectRepo, 
    _recordRepo :: RecordRepo
  }

$(makeLenses ''Laboratory)
