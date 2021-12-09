module Main where

import Lib
import Brick 
import TUI.Menu (app, initialMenuState)
import TUI.GameApp (gameApp, initialGameAppState)
import Network (dummyConnection)

main :: IO ()
main = do 
    finalState <- defaultMain gameApp (initialGameAppState dummyConnection)
    -- finalState <- defaultMain app initialMenuState
    return ()
