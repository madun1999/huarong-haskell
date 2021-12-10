{-# LANGUAGE TemplateHaskell #-}

module Network where

import Lens.Micro.TH (makeLenses)
import Lens.Micro ((^.), (&), (.~))

data Connection = Connection 
    {   _serverIP :: String
      , _tableID :: String
      , _connection :: ()
      , _localPlayerID :: Int
      , _connectionStatus :: ConnectionStatus
    } deriving (Show)
data ConnectionStatus = Good | Disconnected deriving (Show)


-- Convenient Lenses
makeLenses ''Connection

-- For testing
dummyConnection =
  Connection "" "" () 0 Good