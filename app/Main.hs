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
import Control.Monad (forever, liftM, void)
import Control.Monad.Fix
import Data.Maybe (fromMaybe)
import Graphics.Vty (defaultConfig, mkVty)
import Lib
import Logic (Direction (..))
import TUI.GameApp
    ( GameAppEvent (..), Move (Move), gameApp
    , initialGameAppState, initialSimpleGameState, gameMessageEvent
    )
import TUI.Menu (MenuEvent (..), MenuState, app, initialMenuState, menuMessageEvent, connectionDetails)
import qualified TUI.Menu as M (tableID)
import Lens.Micro ((^.), (&), (.~))
import qualified Data.Text as T

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
  gameAppChannel <- newBChan 10
  conn <- startClient (gameMessageDispatch gameAppChannel) (roomMessageDispatch menuChannel)
  let builder = mkVty defaultConfig
  initialVty <- builder
  finalMenuState <- customMain initialVty builder (Just menuChannel) app (initialMenuState conn)
  case menuToConnection finalMenuState of
    Nothing         -> return ()
    Just connection -> do
                    let builder = mkVty defaultConfig
                    initialVty <- builder
                    finalGameState <-
                        customMain initialVty builder (Just gameAppChannel) gameApp (initialGameAppState connection)
                    return ()

menuToConnection :: MenuState -> Maybe ConnectionDetails
menuToConnection ms
    | null $ c ^. tableID = Nothing
    | otherwise           = Just c
    where c = ms ^. connectionDetails
-- main :: IO ()
-- main = do 
--     defaultMain gameApp (initialSimpleGameState dummyConnection)
--     return ()

gameMessageDispatch :: BChan GameAppEvent  -> String -> IO ()
gameMessageDispatch gameChannel s = do
    --   event <- gameMessageEvent s
    --   writeBChan gameChannel event
      writeBChan gameChannel $ gameMessageEvent s
      threadDelay 100000

roomMessageDispatch :: BChan MenuEvent -> String -> IO ()
roomMessageDispatch menuChannel s = do
      writeBChan menuChannel $ menuMessageEvent s
      threadDelay 100000