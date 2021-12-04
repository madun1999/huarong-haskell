{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}

module Lib
  ( serveLog,
    plainHandler,
  )
where

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
import Control.Exception (SomeException (SomeException), handle)
import Control.Monad (when)
import qualified Control.Monad as Data.Foldable
import Control.Monad.Fix (fix)
import Control.Monad.State
import Data.Aeson (FromJSON, ToJSON, decode, encode)
import qualified Data.ByteString as B
import Data.ByteString.Char8 as BC (pack, unpack)
import Data.ByteString.Lazy (fromStrict, putStrLn, toStrict)
import qualified Data.ByteString.Lazy as Data.ByteString.Lazy.Internal
import qualified Data.List
import Data.Maybe (fromJust, fromMaybe, isJust)
import Data.UUID (UUID, toString)
import Data.UUID.V4 (nextRandom)
import GHC.Base
import GHC.Generics (Generic)
import Network.BSD (defaultProtocol)
import Network.Socket
  ( AddrInfo (addrAddress, addrFamily, addrFlags),
    AddrInfoFlag (AI_PASSIVE),
    ServiceName,
    SockAddr,
    Socket,
    SocketOption (ReuseAddr),
    SocketType (Stream),
    accept,
    bind,
    defaultHints,
    defaultProtocol,
    getAddrInfo,
    listen,
    setSocketOption,
    socket,
    socketToHandle,
    withSocketsDo,
  )
import Network.Socket.ByteString ()
import System.Environment (getArgs)
import System.IO
  ( BufferMode (NoBuffering),
    Handle,
    IOMode (ReadWriteMode),
    hClose,
    hGetLine,
    hPutStrLn,
    hSetBuffering,
    putStrLn,
  )

-- | game room
data Room = Room
  { roomID :: String,
    channel :: Chan Data.ByteString.Lazy.Internal.ByteString
  }

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

-- |
-- start service
-- Port number or name; 514 is default
-- Function to handle incoming messages
serveLog :: ServiceName -> (SockAddr -> [Char] -> IO ()) -> IO a
serveLog port handlerfunc = withSocketsDo $ do
  addrinfos <-
    getAddrInfo
      (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
      Nothing
      (Just port)
  let serveraddr = head addrinfos

  sock <- socket (addrFamily serveraddr) Stream defaultProtocol
  setSocketOption sock ReuseAddr 1
  bind sock (addrAddress serveraddr)
  listen sock 6
  chanList <- newMVar ([] :: [Room]) :: IO (MVar [Room])

  mainLoop sock chanList handlerfunc

-- |
-- handle connection
mainLoop :: Socket -> MVar [Room] -> (SockAddr -> [Char] -> IO ()) -> IO b
mainLoop sock chanList handlerfunc = do
  connection@(_, clientaddr) <- accept sock
  handlerfunc clientaddr "<server>: client connnected"
  forkIO (procRequest connection chanList handlerfunc)
  mainLoop sock chanList handlerfunc

procRequest :: (Socket, t) -> MVar [Room] -> (t -> [Char] -> IO ()) -> IO ()
procRequest (connsock, clientaddr) chanList handlerfunc = do
  connhdl <- socketToHandle connsock ReadWriteMode
  hSetBuffering connhdl NoBuffering
  let wrapLogfunc = handlerfunc clientaddr

  handle (\(SomeException _) -> return ()) $
    flip fix (Nothing, Nothing, chanList) $ \loop (room, reader, list) -> do
      line <- fmap init (hGetLine connhdl)
      wrapLogfunc line

      case line of
        -- If an exception is caught, send a message and break the loop
        "quit" -> do
          case reader of
            Nothing -> wrapLogfunc "<server>: client disconnected"
            Just r -> killThread r >> wrapLogfunc "<server>: client disconnected"
          let rm = fromJust room
          when (isJust room) (writeChan (channel rm) $ packString $ convertJSONString (wrapInfoResponse "Info" ("Leave " ++ roomID rm) True))
        -- else, continue looping.
        _ -> do
          res <- procMethod (packString line) connhdl (room, reader, list) -- broadcast move message
          loop res

  hClose connhdl -- close the handle

plainHandler :: Show a => a -> [Char] -> IO ()
plainHandler addr msg =
  System.IO.putStrLn $ "From " ++ show addr ++ ": " ++ msg

procMethod :: Data.ByteString.Lazy.Internal.ByteString -> Handle -> (Maybe Room, Maybe GHC.Conc.Sync.ThreadId, MVar [Room]) -> IO (Maybe Room, Maybe GHC.Conc.Sync.ThreadId, MVar [Room])
procMethod message connhdl (currRoom, currReader, chanList) = do
  let req = (decode message :: Maybe Request)
  handleMessage (fromMaybe (Request {reqService = "default", reqPayload = ""}) req)
  where
    leaveRoom = when
      (isJust currRoom)
      do
        let r = fromJust currRoom
        writeChan (channel r) $ packString $ convertJSONString (wrapInfoResponse "Info" ("Leave " ++ roomID r) True)
        Data.Foldable.forM_ currReader killThread

    handleMessage (Request reqService reqPayload) =
      case reqService of
        -- "createRoom"
        "createRoom" -> do
          chan <- newChan
          uuid <- nextRandom
          let room = Room (Data.UUID.toString uuid) chan
          -- fork channel for reading broadcast
          reader <- forkIO $
            fix $ \loop -> do
              line <- readChan chan
              hPutStrLn connhdl $ unpackByteString line -- pass move message to client
              loop
          hPutStrLn connhdl $ convertJSONString (wrapInfoResponse "createRoom" (Data.UUID.toString uuid) True)
          case currReader of
            Nothing -> do
              currChanList <- takeMVar chanList
              putMVar chanList $! (room : currChanList)
            Just r -> do
              leaveRoom
          return (Just room, Just reader, chanList)
        "joinRoom" -> do
          currChanList <- readMVar chanList
          let room = Data.List.find (\(Room roomID channel) -> roomID == reqPayload) currChanList
          case room of
            Nothing -> return (currRoom, currReader, chanList)
            Just (Room roomID channel) -> do
              -- fork channel for reading broadcast
              commonChan <- dupChan channel
              reader <- forkIO $
                fix $ \loop -> do
                  line <- readChan commonChan
                  hPutStrLn connhdl $ unpackByteString line -- pass move message to client
                  loop
              Data.Foldable.forM_ currReader killThread
              writeChan channel $ packString $ convertJSONString (wrapInfoResponse "Info" ("Join " ++ roomID) True)
              return (room, Just reader, chanList)
        "move" -> do
          when (isJust currRoom) (writeChan (channel $ fromJust currRoom) (packString $ convertJSONString Response {resService = reqService, resPayload = reqPayload}))
          return (currRoom, currReader, chanList)
        "leaveRoom" -> leaveRoom >> return (Nothing, Nothing, chanList)
        _ -> hPutStrLn connhdl (convertJSONString (wrapInfoResponse "Info" "unknow" True)) >> return (currRoom, currReader, chanList)

wrapInfoResponse :: String -> String -> Bool -> Response
wrapInfoResponse service detail status = Response {resService = service, resPayload = convertJSONString (Info {detail = detail, status = status})}

convertJSONString :: ToJSON a => a -> [Char]
convertJSONString a = unpackByteString $ encode a

unpackByteString :: Data.ByteString.Lazy.Internal.ByteString -> [Char]
unpackByteString = unpack . toStrict

packString :: String -> Data.ByteString.Lazy.Internal.ByteString
packString = fromStrict . pack

-- test case
-- {"reqService":"createRoom", "reqPayload":""}
-- {"reqService":"leaveRoom", "reqPayload":""}
-- {"reqService":"move", "reqPayload":"from 1"} {"reqService":"move", "reqPayload":"from 2"}
-- {"reqService":"joinRoom", "reqPayload":"31f0faa0-1047-4353-8682-6ffaa0feb8ea"}