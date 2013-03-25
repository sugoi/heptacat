{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Heptacat.Task where

import           Control.Applicative ((<|>))
import           Control.Lens
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Data
import qualified Data.Map as Map
import           Data.Time (UTCTime, getCurrentTime)
import           GHC.Generics
import           System.IO.Unsafe

import           Heptacat.Project

data Event = Start | Timeout | Failure | Success
    deriving (Eq, Show, Data, Typeable, Generic)
$(makeLenses ''Event)
$(deriveJSON id ''Event)


data State
  = State { _event :: Event,  _timeStamp :: UTCTime } 
    deriving (Eq, Show, Data, Typeable, Generic)  
$(makeLenses ''State)
$(deriveJSON (drop 1) ''State)

  
data Task = Task
  {
    _reflog  :: String,
    _cmdLineArgs :: String,
    _taskState :: Map.Map WorkerName State
  }
    deriving (Eq, Show, Data, Typeable, Generic)

$(makeLenses ''Task)
$(deriveJSON (drop 1) ''Task)


testTasks :: [Task]
testTasks = 
  [ Task "acbd" "1 10" $ Map.fromList [("", State Start t)] ]
  where
    t = unsafePerformIO $ getCurrentTime



