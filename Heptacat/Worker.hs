module Heptacat.Worker where

data WorkerEvent = 
data WorkerState
  = WorkerState { _eventFlag :: WorkerEvent,  _timeStamp :: UTCTime } 
  
  
data Task = Task
  {
    _taskCmd :: String,
    _workerStates :: [WorkerState]
  }
    deriving (Eq, Show, Data, Typeable, Generic)