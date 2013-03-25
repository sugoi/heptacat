{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Heptacat.Task where

import           Control.Applicative ((<|>))
import           Control.Lens
import           Data.Aeson as Aeson
import           Data.Aeson.TH
import qualified Data.ByteString.Lazy.Char8 as BSL
import           Data.Data
import qualified Data.Map as Map
import           Data.Time (UTCTime, getCurrentTime)
import           GHC.Generics
import           System.IO.Unsafe

import           Heptacat.Project

data Event = Intact | Start | Timeout | Failure | Success
    deriving (Eq, Ord, Show, Data, Typeable, Generic)
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
    _taskFileName :: FilePath, 
    _taskState :: Map.Map WorkerName State
  }
    deriving (Eq, Show, Data, Typeable, Generic)

$(makeLenses ''Task)
$(deriveJSON (drop 1) ''Task)

encodeState :: WorkerName -> Task -> State -> String
encodeState wn ta st = unwords 
  [read $ BSL.unpack $ Aeson.encode $ st ^. timeStamp, 
   show $ st ^. event, 
   wn, 
   ta ^. reflog, 
   ta ^. cmdLineArgs]




testTasks :: [Task]
testTasks = 
  [ Task "acbd" "1 10" "list-01.txt" $ Map.fromList [("", State Start t)] ]
  where
    t = unsafePerformIO $ getCurrentTime



