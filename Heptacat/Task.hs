{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}


module Heptacat.Task where

import           Control.Applicative 
import           Control.Lens
import           Data.Aeson as Aeson
import           Data.Aeson.TH
import qualified Data.ByteString.Lazy.Char8 as BSL
import           Data.Data
import qualified Data.Map as Map
import           Data.Time (UTCTime, getCurrentTime)
import           GHC.Generics
import           Safe (readMay)
import           System.IO.Unsafe

import           Heptacat.Utils (decodeA1, wordsN)
import           Heptacat.Project

data WorkState = Intact | Started | Timeout | Failure | Success
    deriving (Eq, Ord, Show, Read, Data, Typeable, Generic)
$(makeLenses ''WorkState)
$(deriveJSON id ''WorkState)


data Event
  = Event { _timeStamp :: UTCTime, _workState :: WorkState } 
    deriving (Eq, Show, Data, Typeable, Generic)  
$(makeLenses ''Event)
$(deriveJSON (drop 1) ''Event)


data TaskKey = TaskKey
  {
    _reflog  :: String,
    _cmdLineArgs :: String
  }
    deriving (Eq, Show, Data, Typeable, Generic)

$(makeLenses ''TaskKey)
$(deriveJSON (drop 1) ''TaskKey)

  
data Task = Task
  {
    _taskKey :: TaskKey,
    _taskFileName :: FilePath, 
    _taskEvents :: Map.Map WorkerName Event
  }
    deriving (Eq, Show, Data, Typeable, Generic)

$(makeLenses ''Task)
$(deriveJSON (drop 1) ''Task)

encodeEvent :: WorkerName -> TaskKey -> Event -> String
encodeEvent wn tk ev = unwords 
  [read $ BSL.unpack $ Aeson.encode $ ev ^. timeStamp, 
   take 7 $ (++ repeat ' ') $ show $ ev ^. workState, 
   wn, 
   tk ^. reflog, 
   tk ^. cmdLineArgs]

decodeEvent :: String -> Maybe (WorkerName, TaskKey, Event)      
decodeEvent str =
  let ((strTime,_):
       (strWS,_):
       (wn,_):
       (strRef,args):_) = wordsN str
      tk = TaskKey strRef args
      maybeTime = decodeA1 $ BSL.pack $ show strTime
      maybeWS   = readMay $ strWS
      maybeEvt  = Event <$> maybeTime <*> maybeWS
  in (wn, tk, ) <$> maybeEvt
 
testTasks :: [Task]
testTasks = 
  [ Task 
      (TaskKey "acbd" "1 10") 
      "list-01.txt" $ 
      Map.fromList [("", Event t Started)] ]
  where
    t = unsafePerformIO $ getCurrentTime
