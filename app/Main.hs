module Main where

import Brick
import Client (startClient)
import Control.Arrow (ArrowLoop (loop))
import Control.Monad.Fix
import Lib
import TUI.GameApp (gameApp, initialGameAppState)
import TUI.Menu (app, initialMenuState)

main :: IO ()
main = do
  -- finalState <- defaultMain gameApp (initialGameAppState dummyConnection)
  (sendMovementMsg, sendJoinRoomMsg, sendCreateRoomMsg, closeClient) <- startClient putStrLn putStrLn
  sendCreateRoomMsg
  sendMovementMsg "move 1->2"

  fix
    ( \loop -> do
        a <- getLine
        loop
    )

  -- finalState <- defaultMain app initialMenuState
  return ()
