{-# LANGUAGE TemplateHaskell #-}

module TUI.GameApp where

import Logic
  ( Game(..), numRows, numCols, Role (..)
  , Grid, Tile, move, Direction (..), selectLevel
  )

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
import Lens.Micro ((^.), (&), (.~), (?~))
import Brick.BChan (newBChan, writeBChan)
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import qualified Brick.Util as U
import qualified Graphics.Vty as V
import Client (ConnectionDetails, connectionStatus, ConnectionStatus (..), tableID, localPlayerID, connectionCallbacks, sendMovementMsgCallBack)
import Graphics.Vty
  ( black, cyan, red, blue, yellow, magenta
  , green, brightBlue, brightCyan, brightGreen, brightYellow, brightMagenta
  )
import Control.Monad.IO.Class (MonadIO(liftIO))


-------------- Menu Types ----------------

-- States
data GameAppState = GameAppState {
    _connectionDetails :: ConnectionDetails
  , _cursorPosition :: CursorPosition
  , _localGame :: Game
  , _opponentGame :: Game
  , _duelStatus :: DuelStatus
}

data CursorPosition = CursorPosition
  {
    _cursorX :: Int
  , _cursorY :: Int
  , _selected :: Maybe (Int, Int)
  }

initialCursorPosition :: CursorPosition
initialCursorPosition = CursorPosition 0 0 Nothing

data DuelStatus = InProgress | LocalWin | LocalLose | Tie deriving (Eq, Show)

data Move = Move
  { _x:: Int
  , _y :: Int
  , _direction :: Logic.Direction
  } deriving (Eq, Show, Read)

-- Event

type PlayerID = Int

data GameAppEvent =
  MoveEvent Move PlayerID
  deriving (Eq, Show, Read)

-- Resource
data GameAppResourceName =
  MenuServerIPField | MenuTableIDField
  deriving (Eq, Ord, Show)

-- Convenient Lenses
makeLenses ''GameAppState
makeLenses ''CursorPosition

---------- Initial State for the GameApp --------------

initialGameAppState :: ConnectionDetails -> String -> GameAppState
initialGameAppState c levelStr = GameAppState c initialCursorPosition g g InProgress
                      where g = selectLevel levelStr

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
drawGameApp s = [vBox [ str $ "Table ID: " ++ (s ^. connectionDetails . tableID) ,
  hBox [
  drawLocalBoard s,
  drawStats s,
  drawOpponentBoard s
  ]]]


drawLocalBoard :: GameAppState -> Widget GameAppResourceName
drawLocalBoard s = B.borderWithLabel (str "Your Game") $ drawGrid (s ^. localGame) $ Just (s ^. cursorPosition)


drawOpponentBoard :: GameAppState -> Widget GameAppResourceName
drawOpponentBoard s = B.borderWithLabel (str "Opponent's Game") $ drawGrid (s ^. opponentGame) Nothing

drawGrid :: Game -> Maybe CursorPosition -> Widget GameAppResourceName
drawGrid game cp = vBox [hBox [ drawCell game x y cp | x <-[0..numCols -1]] | y <- [0..numRows -1]]


drawCell :: Game -> Int -> Int -> Maybe CursorPosition -> Widget GameAppResourceName
drawCell game x y cp = tileColor (grid game) x y $ drawCursorBorder cp x y $ C.center $  tileStr (grid game) x y True



drawCursorBorder :: Maybe CursorPosition -> Int -> Int -> Widget GameAppResourceName -> Widget GameAppResourceName
drawCursorBorder (Just cp) x y
    | cp ^. cursorX == x && cp ^. cursorY == y = B.border
    | otherwise              = padAll 1
drawCursorBorder Nothing x y = padAll 1


tileStr :: Grid -> Int -> Int -> Bool -> Widget n
tileStr grid x y hide =
  if  hide && ((y /= 0) && ((grid !! (y-1) !! x) == (grid !! y !! x))
    || (x /= 0) && ((grid !! y !! (x-1)) == (grid !! y !! x)))
  then
    str " "
  else
    str $ case grid !! y !! x of
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

tileColor :: Grid -> Int -> Int -> Widget n -> Widget n
tileColor grid x y w = case grid !! y !! x of
  Just Zhangfei -> withAttr zhangfeiAttr w
  Just Caocao -> withAttr caocaoAttr w
  Just Machao -> withAttr machaoAttr w
  Just Zhaoyun -> withAttr zhaoyunAttr w
  Just Guanyu -> withAttr guanyuAttr w
  Just Huangzhong -> withAttr huangzhongAttr w
  Just Zu1 -> withAttr zu1Attr w
  Just Zu2 -> withAttr zu2Attr w
  Just Zu3 -> withAttr zu3Attr w
  Just Zu4 -> withAttr zu4Attr w
  Nothing -> w

drawStats :: GameAppState -> Widget GameAppResourceName
drawStats s = hLimit 40
  $ vBox [ 
          --  drawConnectionStatus (s ^. connectionDetails), 
           hBox [drawLocalStep (s ^. localGame), drawOpponentStep (s ^. opponentGame)]
         , drawSelected s
         , padTop (Pad 2) $ drawGameOver (s ^. duelStatus)
         , drawInstructions
         ]

drawSelected :: GameAppState -> Widget GameAppResourceName
drawSelected s = case s ^. cursorPosition . selected of
  Just (cx, cy) -> withBorderStyle BS.unicodeBold
                      $ B.borderWithLabel (str "Selected:")
                      $ C.center
                      $ tileColor (grid game) cx cy $  C.center $ drawCursorBorder Nothing cx cy $ tileStr (grid game) cx cy False
                      where game = s ^. localGame

  Nothing       -> withBorderStyle BS.unicodeBold
                         $ B.borderWithLabel (str "Selected:")
                         $ C.center
                         $ padAll 1
                         $ str " "



drawInstructions :: Widget GameAppResourceName
drawInstructions = padTop (Pad 2) $ vBox
  [
    str "arrow: move cursor"
  , str "enter: select/deselect/move"
  , str "q: quit"
  ]

drawConnectionStatus :: ConnectionDetails -> Widget GameAppResourceName
drawConnectionStatus c = withBorderStyle BS.unicodeBold
                         $ B.borderWithLabel (str "Connection:")
                         $ C.center
                         $ case c ^. connectionStatus of
                            Good -> C.hCenter $ str "Good"
                            Disconnected -> C.hCenter $ str "Disconnected"

drawLocalStep :: Game -> Widget GameAppResourceName
drawLocalStep g = drawStep n finished "Your Steps:"
  where n        = step g
        finished = status g 

drawOpponentStep :: Game -> Widget GameAppResourceName
drawOpponentStep g = drawStep n finished "Opponent's Steps:"
  where n        = step g
        finished = status g 

drawStep :: Int -> Bool -> String -> Widget GameAppResourceName
drawStep n finished s = withBorderStyle BS.unicodeBold
  $ B.borderWithLabel (str s)
  $ C.hCenter
  $ padAll 1
  $ vBox [C.hCenter $ str $ show n, C.hCenter $ str (if finished then "Finished!" else " ")]

drawGameOver :: DuelStatus -> Widget GameAppResourceName
drawGameOver InProgress = C.hCenter $ str "In Progress"
drawGameOver LocalWin = C.hCenter $ str "You Win!"
drawGameOver LocalLose = C.hCenter $ str "You Lose.."
drawGameOver Tie = C.hCenter $ str "Tie!"


---------- Create Events from Server Message ----------

gameMessageEvent :: String -> GameAppEvent
gameMessageEvent s = read s :: GameAppEvent
-- gameMessageEvent :: String -> IO GameAppEvent
-- gameMessageEvent s = print s >> return (MoveEvent (Move 1 2 Logic.Left) 1)


---------- Handle Brick Events ------------

handleGameAppEvent :: GameAppState -> BrickEvent GameAppResourceName GameAppEvent -> EventM GameAppResourceName (Next GameAppState)

handleGameAppEvent s (AppEvent (MoveEvent move id))
  | id == s ^. connectionDetails . localPlayerID = continue s
  | otherwise = tryOpponentMove s move


handleGameAppEvent s (VtyEvent (V.EvKey V.KUp [])) =
  continue $
    case s ^. cursorPosition . cursorY of
      0         -> s
      a -> s & cursorPosition . cursorY .~ a-1
handleGameAppEvent s (VtyEvent (V.EvKey V.KDown [])) =
  continue $
    case s ^. cursorPosition . cursorY of
      4         -> s
      a -> s & cursorPosition . cursorY .~ a+1
handleGameAppEvent s (VtyEvent (V.EvKey V.KLeft [])) =
  continue $
    case s ^. cursorPosition . cursorX of
      0         -> s
      a -> s & cursorPosition . cursorX .~ a-1
handleGameAppEvent s (VtyEvent (V.EvKey V.KRight [])) =
  continue $
    case s ^. cursorPosition . cursorX of
      3         -> s
      a -> s & cursorPosition . cursorX .~ a+1
handleGameAppEvent s (VtyEvent (V.EvKey V.KEnter [])) =
    case currentCell s of
      Just r -> continue $ s & ((cursorPosition . selected) ?~ (cp ^. cursorX, cp ^. cursorY))
      Nothing ->
        case s ^. cursorPosition . selected of
          Just (x, y) -> tryLocalMove s x y
          Nothing -> continue s
      where cp = s ^. cursorPosition

handleGameAppEvent s (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt s
handleGameAppEvent s ev = continue s


tryLocalMove :: GameAppState -> Int -> Int -> EventM n (Next GameAppState)
tryLocalMove s sx sy =
  if status $ s ^. localGame then continue s else
    let cx  = s ^. cursorPosition . cursorX
        cy   = s ^. cursorPosition . cursorY
        gr        = grid $ s ^. localGame
        sCell     = gr !! sy !! sx
        direction
          | cx > 0 && gr !! cy !! (cx - 1) == sCell = Just Logic.Right
          | cy > 0 && gr !! (cy - 1) !! cx == sCell = Just Logic.Down
          | cx < 3 && gr !! cy !! (cx + 1) == sCell = Just Logic.Left
          | cy < 4 && gr !! (cy + 1) !! cx == sCell = Just Logic.Up
          | otherwise = Nothing
    in case direction of
      Just a -> let movement = Move sx sy a 
                    event    = MoveEvent movement (s ^. connectionDetails . localPlayerID) in
        do
        x <- continue $ checkWinner $ s & localGame .~ move (s ^. localGame) sx sy a & cursorPosition . selected .~ Nothing
        liftIO (sendMovementMsg s (show event))
        return x
      Nothing -> continue s

sendMovementMsg :: GameAppState -> String -> IO ()
sendMovementMsg s str = do
  -- print str     
  (s ^. connectionDetails . connectionCallbacks . sendMovementMsgCallBack) str 


tryOpponentMove :: GameAppState -> Move -> EventM n (Next GameAppState)
tryOpponentMove s m@(Move x y direction) =
  if status $ s ^. opponentGame then continue s else
    let gr        = grid $ s ^. opponentGame in do
      -- liftIO (print $ show m) 
      continue $ checkWinner $ s & opponentGame .~ move (s ^. opponentGame) x y direction

checkWinner :: GameAppState -> GameAppState
checkWinner s
  | s ^. duelStatus /= InProgress || not (status $ s ^. localGame) || not (status $ s ^. opponentGame)
    = s
  | step (s ^. localGame) == step (s ^. opponentGame) = s & duelStatus .~ Tie
  | step (s ^. localGame) <= step (s ^. opponentGame) = s & duelStatus .~ LocalWin
  | step (s ^. localGame) >= step (s ^. opponentGame) = s & duelStatus .~ LocalLose
  | otherwise = s



currentCell :: GameAppState -> Tile
currentCell s = gr !! y !! x
  where gr = grid (s ^. localGame)
        x  = s ^. cursorPosition . cursorX
        y  = s ^. cursorPosition . cursorY



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