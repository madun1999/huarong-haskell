module Main where

import Brick
import Brick.BChan (newBChan, writeBChan)
import Client (startClient)
import Control.Arrow (ArrowLoop (loop))
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever, liftM, void)
import Control.Monad.Fix
import Data.Maybe (fromMaybe)
import Graphics.Vty (defaultConfig, mkVty)
import Lib
import Logic (Direction (..))
import Network (Connection, dummyConnection)
import TUI.GameApp (GameAppEvent (OpponentMove), Move (Move), gameApp, initialGameAppState)
import TUI.Menu (MenuEvent (ConnectionFailed), MenuState, app, initialMenuState)

-- main :: IO ()
-- main = do
--   -- finalState <- defaultMain gameApp (initialGameAppState dummyConnection)
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
  void . forkIO $
    forever $ do
      writeBChan menuChannel $ ConnectionFailed "1"
      threadDelay 100000
  let builder = mkVty defaultConfig
  initialVty <- builder
  finalMenuState <- customMain initialVty builder (Just menuChannel) app initialMenuState

  case menuToConnection finalMenuState of
    Nothing -> return ()
    Just connection -> do
      gameAppChannel <- newBChan 10
      void . forkIO $
        forever $ do
          writeBChan gameAppChannel $ OpponentMove (Move 0 0 Logic.Up) -- TODO
          threadDelay 100000
      let builder = mkVty defaultConfig
      initialVty <- builder
      finalGameState <-
        customMain
          initialVty
          builder
          (Just gameAppChannel)
          gameApp
          (initialGameAppState connection)
      return ()

menuToConnection :: MenuState -> Maybe Connection
menuToConnection ms = Just dummyConnection
