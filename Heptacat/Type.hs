{-# LANGUAGE TemplateHaskell #-}

module Heptacat.Type where

import Control.Lens
import Control.Lens.TH
import Data.Default

class HasUrl t where
  url :: Simple Lens t String

data SubjectRepo = SubjectRepo
  { _subjectRepoUrl :: String,
    _outputDir :: FilePath ,
    _startUpScript :: FilePath}
    deriving (Eq, Show)

$(makeLenses ''SubjectRepo)

instance HasUrl SubjectRepo where
  url = subjectRepoUrl

instance Default SubjectRepo where
  def = SubjectRepo "" "output" "start"

data RecordRepo = RecordRepo
  {    
    _recordRepoUrl :: String,
    _taskListDir :: FilePath,
    _workerStateDir :: FilePath,
    _resultDir :: FilePath 
  }
    deriving (Eq, Show)

$(makeLenses ''RecordRepo)

instance HasUrl RecordRepo where
  url = recordRepoUrl

instance Default RecordRepo where
  def = RecordRepo "" "task" "woker_state" "result"

data Laboratory = Laboratory
  {
    _subjectRepo :: SubjectRepo, 
    _recordRepo :: RecordRepo
  }
    deriving (Eq, Show)

instance Default Laboratory where
  def = Laboratory def def

$(makeLenses ''Laboratory)
