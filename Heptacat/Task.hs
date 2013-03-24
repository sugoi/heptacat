{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Heptacat.Task where

import           Control.Applicative ((<|>))
import           Control.Lens
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Data
import           Data.Time (UTCTime, getCurrentTime)
import           GHC.Generics
import           System.IO.Unsafe

data Event = Start | Success | Timeout | Failure
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
    _cmdLine :: String,
    _workerStates :: [State]
  }
    deriving (Eq, Show, Data, Typeable, Generic)

$(makeLenses ''Task)
$(deriveToJSON (drop 1) ''Task)

instance FromJSON Task where
  parseJSON val =
    let tryFromStr = fmap (\str -> Task str []) . parseJSON
    in tryFromStr val  <|> $(mkParseJSON (drop 1) ''Task) val

testTasks :: [Task]
testTasks = 
  [ Task "acbd 1 10" [State Start t] ]
  where
    t = unsafePerformIO $ getCurrentTime



