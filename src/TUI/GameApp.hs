{-# LANGUAGE TemplateHaskell #-}

module TUI.GameApp where

import Logic (Game(..), initialPosition, numRows, numCols, Role (..))

import Brick
  ( App(..), AttrMap, BrickEvent(..), EventM, Next, Widget
  , customMain, neverShowCursor
  , continue, halt
  , hLimit, vLimit, vBox, hBox
  , padRight, padLeft, padTop, padAll, Padding(..)
  , withBorderStyle
  , str
  , attrMap, withAttr, emptyWidget, AttrName, on, fg
  , (<+>), attrName, CursorLocation (cursorLocation)
  )

import Lens.Micro.TH (makeLenses)
import Lens.Micro ((^.), (&), (.~))
import Brick.BChan (newBChan, writeBChan)
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import qualified Brick.Util as U
import qualified Graphics.Vty as V
import Client (Connection, connectionStatus, ConnectionStatus (..))
import Graphics.Vty
  ( black, cyan, red, blue, yellow, magenta
  , green, brightBlue, brightCyan, brightGreen, brightYellow, brightMagenta
  )

-------------- Menu Types ----------------

-- States
data GameAppState = GameAppState {
    _connection :: Connection
  , _cursorPosition :: CursorPosition
  , _localGame :: Game
  , _opponentGame :: Game
  , _duelStatus :: DuelStatus
}

data CursorPosition = CursorPosition 
  {
    _cursorX :: Int
  , _cursorY :: Int 
  }

initialCursorPosition :: CursorPosition
initialCursorPosition = CursorPosition 0 0

data DuelStatus = InProgress | LocalWin | LocalLose | Tie

type Move = ()

-- Event

data GameAppEvent = OpponentMove Move

-- Resource
data GameAppResourceName =
  MenuServerIPField | MenuTableIDField
  deriving (Eq, Ord, Show)

-- Convenient Lenses
makeLenses ''GameAppState

---------- Initial State for the GameApp --------------

initialGameAppState :: Connection -> GameAppState
initialGameAppState c = GameAppState c initialCursorPosition initialPosition initialPosition InProgress


---- Brick App ----

gameApp :: App GameAppState GameAppEvent GameAppResourceName
gameApp = App { appDraw = drawGameApp
          , appChooseCursor = neverShowCursor
          , appHandleEvent = handleGameAppEvent
          , appStartEvent = return
          , appAttrMap = const boardAttrMap
          }






---- Brick draw App ----
drawGameApp :: GameAppState -> [Widget GameAppResourceName]
drawGameApp s = [hBox [
  drawLocalBoard s,
  drawStats s,
  drawOpponentBoard s
  ]]


drawLocalBoard :: GameAppState -> Widget GameAppResourceName
drawLocalBoard s = B.borderWithLabel (str "Your Game") $ drawGrid (s ^. localGame) $ Just (s ^. cursorPosition)


drawOpponentBoard :: GameAppState -> Widget GameAppResourceName
drawOpponentBoard s = B.borderWithLabel (str "Opponent's Game") $ drawGrid (s ^. opponentGame) Nothing

drawGrid :: Game -> Maybe CursorPosition -> Widget GameAppResourceName
drawGrid game cp = vBox [hBox [ drawCell game x y | y <-[0..numCols -1]] | x <- [0..numRows -1]]
  where
    drawCell game x y = tileColor (grid game) x y $  C.hCenter $   drawCursorBorder cp x y $ tileStr (grid game) x y



drawCursorBorder :: Maybe CursorPosition -> Int -> Int -> Widget GameAppResourceName -> Widget GameAppResourceName
drawCursorBorder (Just (CursorPosition cx cy)) x y 
    | (cx == x) && (cy == y) = B.border 
    | otherwise              = padAll 1
drawCursorBorder Nothing x y = padAll 1

-- drawInfo :: GameAppState -> Widget GameAppResourceName
-- drawInfo s = vBox
--   [ drawConnectionInfo $ s ^. connection
--   , drawWinLoss $ s ^. duelStatus
--   ]
--   where drawConnectionInfo c = str $ show $ c ^. connectionStatus
--         drawWinLoss ds = str $ case ds of
--                                   InProgress -> ""
--                                   LocalWin -> "You win!"
--                                   LocalLose -> "You lost."
--                                   Tie -> "Tie."

tileStr :: [[Maybe Role]] -> Int -> Int -> Widget n
tileStr grid x y =
  if    ((x /= 0) && ((grid !! (x-1) !! y) == (grid !! x !! y)))
    || ((y /= 0) && ((grid !! x !! (y-1)) == (grid !! x !! y)))
  then
    str " "
  else
    str $ case grid !! x !! y of
      Just Zhangfei -> "张飞"
      Just Caocao -> "曹操"
      Just Machao -> "马超"
      Just Zhaoyun -> "赵云"
      Just Guanyu -> "关羽"
      Just Huangzhong -> "黄忠"
      Just Zu1 -> "卒"
      Just Zu2 -> "卒"
      Just Zu3 -> "卒"
      Just Zu4 -> "卒"
      Nothing -> " "

tileColor :: [[Maybe Role]] -> Int -> Int -> Widget n -> Widget n
tileColor grid x y w = case grid !! x !! y of
  Just Zhangfei -> withAttr zhangfeiAttr $ w
  Just Caocao -> withAttr caocaoAttr $ w
  Just Machao -> withAttr machaoAttr $ w
  Just Zhaoyun -> withAttr zhaoyunAttr $ w
  Just Guanyu -> withAttr guanyuAttr $ w
  Just Huangzhong -> withAttr huangzhongAttr $ w
  Just Zu1 -> withAttr zu1Attr $ w
  Just Zu2 -> withAttr zu2Attr $ w
  Just Zu3 -> withAttr zu3Attr $ w
  Just Zu4 -> withAttr zu4Attr $ w
  Nothing -> w

drawStats :: GameAppState -> Widget GameAppResourceName
drawStats s = hLimit 40
  $ vBox [ drawConnectionStatus (s ^. connection)
         , drawLocalStep (step $ s ^. localGame) 
         , drawOpponentStep (step $ s ^. opponentGame)
         , padTop (Pad 2) $ drawGameOver (s ^. duelStatus)
         , drawInstructions
         ]

drawInstructions :: Widget GameAppResourceName
drawInstructions = padTop (Pad 2) $ vBox 
  [
    str "arrow: move cursor"
  , str "enter: select/deselect/move"
  , str "q: quit"
  ]

drawConnectionStatus :: Connection -> Widget GameAppResourceName
drawConnectionStatus c = withBorderStyle BS.unicodeBold
                         $ B.borderWithLabel (str "Connection:")
                         $ C.center 
                         $ case c ^. connectionStatus of
                            Good -> C.hCenter $ str "Good" 
                            Disconnected -> C.hCenter $ str "Disconnected" 

drawLocalStep :: Int -> Widget GameAppResourceName
drawLocalStep n = drawStep n "Your Steps:"

drawOpponentStep :: Int -> Widget GameAppResourceName
drawOpponentStep n = drawStep n "Opponent Steps:"

drawStep :: Int -> String -> Widget GameAppResourceName
drawStep n s = withBorderStyle BS.unicodeBold
  $ B.borderWithLabel (str s)
  $ C.hCenter
  $ padAll 1
  $ str $ show n

drawGameOver :: DuelStatus -> Widget GameAppResourceName
drawGameOver InProgress = emptyWidget 
drawGameOver LocalWin = C.hCenter $ str "You Win!" 
drawGameOver LocalLose = C.hCenter $ str "You Lose.." 
drawGameOver Tie = C.hCenter $ str "Tie!"


---------- Handle Brick Events ------------

handleGameAppEvent :: GameAppState -> BrickEvent GameAppResourceName GameAppEvent -> EventM GameAppResourceName (Next GameAppState)

-- handleEvent s (AppEvent (ConnectionSuccess c)) = error "Not implemented"
-- handleEvent s (AppEvent (ConnectionFailed e)) = continue (s & connectionState .~ Error e)

handleGameAppEvent s (VtyEvent (V.EvKey V.KEnter [])) = connectToTable s
handleGameAppEvent s (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt s
handleGameAppEvent s ev = continue s
  -- do
  -- newForm <- handleFormEvent ev (s ^. connectionInfoForm)
  -- continue (s & connectionInfoForm .~ newForm)


connectToTable :: GameAppState -> EventM GameAppResourceName (Next GameAppState)
connectToTable s = continue s
  -- liftIO (requestConnectToTable (s ^. connectionInfo)) >> continue (s & connectionState .~ Connecting)



requestConnectToTable :: connectionInfo -> IO ()
-- TODO
requestConnectToTable ci = return ()


---------- Brick Attribute Map ----------


zhangfeiAttr, caocaoAttr, machaoAttr, zhaoyunAttr, guanyuAttr, huangzhongAttr:: AttrName
zhangfeiAttr = attrName "ZhangfeiAttr"
caocaoAttr = attrName "Caocao"
machaoAttr = attrName "Machao"
zhaoyunAttr = attrName "Zhaoyun"
guanyuAttr = attrName "Guanyu"
huangzhongAttr = attrName "Huangzhong"

zu1Attr, zu2Attr, zu3Attr, zu4Attr :: AttrName
zu1Attr = attrName "Zu1"
zu2Attr = attrName "Zu2"
zu3Attr = attrName "Zu3"
zu4Attr = attrName "Zu4"

boardAttrMap :: AttrMap
boardAttrMap = attrMap V.defAttr
  [
    (caocaoAttr, black `on` red)
  , (zhangfeiAttr, black `on` blue)
  , (machaoAttr, black `on` cyan)
  , (zhaoyunAttr, black `on` yellow)
  , (guanyuAttr, black `on` magenta)
  , (huangzhongAttr, black `on` green)
  , (zu1Attr, black `on` brightMagenta)
  , (zu2Attr, black `on` brightCyan)
  , (zu3Attr, black `on` brightGreen)
  , (zu4Attr, black `on` brightYellow)
  ]