module TUI.Game where

import Logic (Game(..))

import Brick
  ( App(..), AttrMap, BrickEvent(..), EventM, Next, Widget
  , customMain, neverShowCursor
  , continue, halt
  , hLimit, vLimit, vBox, hBox
  , padRight, padLeft, padTop, padAll, Padding(..)
  , withBorderStyle
  , str
  , attrMap, withAttr, emptyWidget, AttrName, on, fg
  , (<+>)
  )

import Brick.BChan (newBChan, writeBChan)
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import qualified Brick.Util as U
import qualified Graphics.Vty as V

---- Brick App Resource Name ----

type Name = ()

---- Brick App Events ----

data BoardEvents = BoardEvent

---- Brick App ----

app :: App Game BoardEvents Name
app = App { appDraw = drawApp
          , appChooseCursor = neverShowCursor
          , appHandleEvent = handleEvent
          , appStartEvent = return
          , appAttrMap = const boardAttrMap
          }




---- Brick draw App ----

drawApp game = 


drawBoard :: Game -> [Widget Name]
drawBoard game = [C.center $ padRight (Pad 2) (drawStats game) <+> drawGrid game]

drawGrid :: Game -> Widget Name
drawGrid game = vBox rows
  where
    rows = [hBox $ tilesInRow r | r <- grid game]
    tilesInRow row = 
        [hLimit 9 $ withBorderStyle BS.unicodeBold $ 
            B.border $ C.hCenter $ padAll 1 $ colorTile $ 
                printTile tile | tile <- row]

colorTile val = case val of
  "2" -> withAttr blueBg $ str val
  "4" -> withAttr brblBg $ str val
  "8" -> withAttr cyanBg $ str val
  "16" -> withAttr bcyanBg $ str val
  "32" -> withAttr magBg $ str val
  "64" -> withAttr bmagBg $ str val
  "128" -> withAttr yellowBg $ str val
  "256" -> withAttr byellowBg $ str val
  "512" -> withAttr greenBg $ str val
  "1024" -> withAttr bgreenBg $ str val
  "2048" -> withAttr whiteBg $ str val
  _ -> str val

drawStats :: Game -> Widget Name
drawStats game = hLimit 11
  $ vBox [ drawStep (step game)
         , padTop (Pad 2) $ drawGameOver (status game)
         ]

drawStep :: Int -> Widget Name
drawStep n = withBorderStyle BS.unicodeBold
  $ B.borderWithLabel (str "Score")
  $ C.hCenter
  $ padAll 1
  $ str $ show n

drawGameOver :: Bool -> Widget Name
drawGameOver status = emptyWidget
--   if status
--      then withAttr gameOverAttr $ C.hCenter $ str "GAME OVER"
--      else emptyWidget

-- gameOverAttr :: AttrName
-- gameOverAttr = "gameOver"

---- Brick handle Board Event ----

handleEvent :: Game -> BrickEvent Name BoardEvents -> EventM Name (Next Game)
handleEvent = error "not implemented"


---- Brick Board attribute Map ---

boardAttrMap :: AttrMap
boardAttrMap = attrMap V.defAttr
  [ 
--       (snakeAttr, V.blue `on` V.blue)
--   , (foodAttr, V.red `on` V.red)
--   , (gameOverAttr, fg V.red `V.withStyle` V.bold)
  ]


