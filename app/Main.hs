module Main where

import Lib
import Brick 
import TUI.Menu (app, initialMenuState, MenuState, MenuEvent (ConnectionFailed))
import TUI.GameApp (gameApp, initialGameAppState, GameAppEvent (OpponentMove), Move (Move))
import Network (dummyConnection, Connection)
import Brick.BChan (newBChan, writeBChan)
import Control.Monad (void, forever, liftM)
import Control.Concurrent (forkIO, threadDelay)
import Graphics.Vty (mkVty, defaultConfig)
import Data.Maybe (fromMaybe)
import Logic (Direction(..))

main :: IO ()
main = do 
    menuChannel <- newBChan 10
    void . forkIO $ forever $ do
        writeBChan menuChannel $ ConnectionFailed "1"
        threadDelay 100000
    let builder = mkVty defaultConfig
    initialVty <- builder
    finalMenuState <- customMain initialVty builder (Just menuChannel) app initialMenuState

    case menuToConnection finalMenuState of 
        Nothing -> return ()
        Just connection -> do
            gameAppChannel <- newBChan 10
            void . forkIO $ forever $ do
                writeBChan gameAppChannel $ OpponentMove (Move 0 0 Logic.Up) -- TODO
                threadDelay 100000
            let builder = mkVty defaultConfig
            initialVty <- builder
            finalGameState <- customMain initialVty builder (Just gameAppChannel) 
                gameApp (initialGameAppState connection)
            return ()

menuToConnection :: MenuState -> Maybe Connection
menuToConnection ms = Just dummyConnection