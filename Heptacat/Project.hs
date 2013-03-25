{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Heptacat.Project where

import Control.Lens
import Data.Aeson.TH
import Data.Data
import Data.Default
import GHC.Generics

class HasUrl t where
  url :: Simple Lens t String

data SubjectRepo = SubjectRepo
  { _subjectRepoUrl :: String,
    _outputDir :: FilePath ,
    _startUpScript :: FilePath}
    deriving (Eq, Show, Data, Typeable, Generic)

$(makeLenses ''SubjectRepo)
$(deriveJSON (drop 1) ''SubjectRepo)

instance HasUrl SubjectRepo where
  url = subjectRepoUrl

instance Default SubjectRepo where
  def = SubjectRepo "" "output" "start"


data RecordRepo = RecordRepo
  {
    _recordRepoUrl :: String,
    _taskListDir :: FilePath,
    _progressDir :: FilePath,
    _workerStateDir :: FilePath,
    _resultDir :: FilePath
  }
    deriving (Eq, Show, Data, Typeable, Generic)

$(makeLenses ''RecordRepo)
$(deriveJSON (drop 1) ''RecordRepo)

instance HasUrl RecordRepo where
  url = recordRepoUrl

instance Default RecordRepo where
  def = RecordRepo "" "task" "progress" "worker" "result"

type WorkerName = String

data Project = Project
  {
    _workerNameInCharge :: WorkerName,
    _subjectRepo :: SubjectRepo,
    _recordRepo :: RecordRepo
  }
    deriving (Eq, Show, Data, Typeable, Generic)

instance Default Project where
  def = Project "" def def

$(makeLenses ''Project)
$(deriveJSON (drop 1) ''Project)

