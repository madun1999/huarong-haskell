{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Client where

import Control.Concurrent
  ( Chan,
    MVar,
    dupChan,
    forkIO,
    killThread,
    newChan,
    newMVar,
    putMVar,
    readChan,
    readMVar,
    takeMVar,
    writeChan,
  )
import qualified Control.Concurrent as GHC.Conc.Sync
import Control.Exception (handle)
import Control.Monad.Fix
import Data.Aeson (FromJSON, ToJSON, decode, encode)
import Data.Bits ()
import qualified Data.ByteString as B
import Data.ByteString.Char8 as BC (hPutStrLn, pack, unpack)
import Data.ByteString.Lazy (fromStrict, putStrLn, toStrict)
import qualified Data.ByteString.Lazy as Data.ByteString.Lazy.Internal
import Data.List ()
import qualified Data.List
import Data.Maybe
import qualified Data.String
import GHC.Base
import GHC.Generics (Generic)
import qualified GHC.IO.Handle.Types
import Lens.Micro ((&), (.~), (^.))
import Lens.Micro.TH (makeLenses)
import Network.BSD (defaultProtocol)
import Network.Socket
  ( AddrInfo (addrAddress, addrFamily),
    SocketOption (KeepAlive),
    SocketType (Stream),
    connect,
    getAddrInfo,
    setSocketOption,
    socket,
    socketToHandle,
  )
import qualified Network.Socket as Network.Socket.Info
import System.IO
  ( BufferMode (BlockBuffering, NoBuffering),
    IOMode (ReadWriteMode, WriteMode),
    hClose,
    hGetLine,
    hPutStr,
    hPutStrLn,
    hSetBuffering,
  )

-- | request
data Request = Request
  { reqService :: String,
    reqPayload :: String
  }
  deriving (Show, Generic)

instance FromJSON Request

instance ToJSON Request

-- | response
data Response = Response
  { resService :: String,
    resPayload :: String
  }
  deriving (Show, Generic)

instance FromJSON Response

instance ToJSON Response

-- | Info
data Info = Info
  { detail :: String,
    status :: Bool
  }
  deriving (Show, Generic)

instance FromJSON Info

instance ToJSON Info

data Connection = Connection
  { _serverIP :: String,
    _tableID :: String,
    _connection :: (),
    _localPlayerID :: Int,
    _connectionStatus :: ConnectionStatus
  }
  deriving (Show)

data ConnectionStatus = Good | Disconnected deriving (Show)

-- Convenient Lenses
makeLenses ''Connection

-- For testing
dummyConnection =
  Connection "" "" () 0 Good

initConnection :: Network.Socket.Info.HostName -> Network.Socket.Info.ServiceName -> IO GHC.IO.Handle.Types.Handle
initConnection hostname port = do
  addrinfos <- getAddrInfo Nothing (Just hostname) (Just port)
  let serveraddr = Prelude.head addrinfos

  sock <- socket (addrFamily serveraddr) Stream defaultProtocol

  setSocketOption sock KeepAlive 1

  connect sock (addrAddress serveraddr)

  handle <- socketToHandle sock ReadWriteMode

  hSetBuffering handle NoBuffering

  return handle

sendMovementMsg :: GHC.IO.Handle.Types.Handle -> String -> IO ()
sendMovementMsg handle msg = do
  BC.hPutStrLn handle $ pack $ convertJSONString $ Request "move" msg
  return ()

sendJoinRoomMsg :: GHC.IO.Handle.Types.Handle -> String -> IO ()
sendJoinRoomMsg handle msg = do
  BC.hPutStrLn handle $ pack $ convertJSONString $ Request "joinRoom" msg
  return ()

sendCreateRoomMsg :: GHC.IO.Handle.Types.Handle -> IO ()
sendCreateRoomMsg handle = do
  BC.hPutStrLn handle $ pack $ convertJSONString $ Request "createRoom" ""
  return ()

-- networkCallback msg = do
--   return ()

startClient :: ([Char] -> IO ()) -> (String -> IO ()) -> IO (String -> IO (), String -> IO (), IO (), IO ())
startClient networkCallback roomIdCallback = do
  handle <- initConnection "localhost" "80"
  reader <- forkIO $
    fix $ \loop -> do
      line <- hGetLine handle
      let res = fromMaybe (Response {resService = "default", resPayload = ""}) (decode $ packString line :: Maybe Response)
      case resService res of
        "move" -> networkCallback line
        "createRoom" -> do
          let info = fromMaybe (Info {status = False, detail = ""}) (decode $ packString (resPayload res) :: Maybe Info)
          roomIdCallback $ detail info
        _ -> do return ()
      loop

  return (sendMovementMsg handle, sendJoinRoomMsg handle, sendCreateRoomMsg handle, closeClient handle reader)

closeClient :: GHC.IO.Handle.Types.Handle -> GHC.Conc.Sync.ThreadId -> IO ()
closeClient handle reader = do
  hClose handle
  killThread reader

convertJSONString :: ToJSON a => a -> [Char]
convertJSONString a = unpackByteString $ encode a

unpackByteString :: Data.ByteString.Lazy.Internal.ByteString -> [Char]
unpackByteString = unpack . toStrict

packString :: String -> Data.ByteString.Lazy.Internal.ByteString
packString = fromStrict . pack
