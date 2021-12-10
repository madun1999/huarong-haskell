module Main where

import Brick
import Brick.BChan (newBChan, writeBChan, BChan)
import Client
    ( startClient,
      dummyConnection,
      ConnectionStatus(Good),
      localPlayerID,
      tableID, ConnectionDetails )
import Control.Arrow (ArrowLoop (loop))
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever, liftM, void, when)
import Control.Monad.Fix
import Data.Maybe (fromMaybe)
import Graphics.Vty (defaultConfig, mkVty, outputIface, supportsMode, Mode (Mouse), Output (setMode))
import Lib
import Logic (Direction (..))
import TUI.GameApp
    ( GameAppEvent (..), Move (Move), gameApp
    , initialGameAppState, gameMessageEvent
    )
import TUI.Menu (MenuEvent (..), MenuState, app, initialMenuState, menuMessageEvent, connectionDetails, level, connectionInfoForm)
import qualified TUI.Menu as M (tableID)
import Lens.Micro ((^.), (&), (.~))
import qualified Data.Text as T
import Brick.Forms
import Data.Text (unpack)
import Control.Monad.IO.Class (liftIO)

-- main :: IO ()
-- main = do
--   -- finalState <- defaultMain gameApp (initialGameAppState dummyConnection "simple")
--   (sendMovementMsg, sendJoinRoomMsg, sendCreateRoomMsg, closeClient) <- startClient putStrLn putStrLn
--   sendCreateRoomMsg
--   sendMovementMsg "move 1->2"

--   fix
--     ( \loop -> do
--         a <- getLine
--         loop
--     )

--   -- finalState <- defaultMain app initialMenuState
--   return ()

main :: IO ()
main = do
  menuChannel <- newBChan 10
  gameAppChannel <- newBChan 10
  conn <- startClient (gameMessageDispatch gameAppChannel) (roomMessageDispatch menuChannel)
  let builder = mkVty defaultConfig
  initialVty <- builder
  let output = outputIface initialVty
  when (supportsMode output Mouse) $ liftIO $ setMode output Mouse True  -- enable Mouse
  finalMenuState <- customMain initialVty builder (Just menuChannel) app (initialMenuState conn)
  case menuToConnection finalMenuState of
    Nothing         -> return ()
    Just connection -> do
                    let builder = mkVty defaultConfig
                    initialVty <- builder
                    let output = outputIface initialVty
                    when (supportsMode output Mouse) $ liftIO $ setMode output Mouse True -- enable Mouse
                    let levelStr = unpack $ formState (finalMenuState ^. connectionInfoForm) ^. level 
                    finalGameState <-
                        customMain initialVty builder (Just gameAppChannel) gameApp (initialGameAppState connection levelStr)
                    return ()
                    

menuToConnection :: MenuState -> Maybe ConnectionDetails
menuToConnection ms
    | null $ c ^. tableID = Nothing
    | otherwise           = Just c
    where c = ms ^. connectionDetails

gameMessageDispatch :: BChan GameAppEvent  -> String -> IO ()
gameMessageDispatch gameChannel s = do
      writeBChan gameChannel $ gameMessageEvent s
      threadDelay 100000

roomMessageDispatch :: BChan MenuEvent -> String -> IO ()
roomMessageDispatch menuChannel s = do
      writeBChan menuChannel $ menuMessageEvent s
      threadDelay 100000