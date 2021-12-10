{-# LANGUAGE TemplateHaskell #-}

module TUI.Menu where

import Brick
    ( attrMap,
      continue,
      neverShowCursor,
      AttrMap,
      App(..),
      EventM,
      BrickEvent (VtyEvent, AppEvent),
      Next,
      Widget, on, halt, str, (<+>), showFirstCursor, vBox, vLimit, padTop, Padding (Pad) )

import qualified Data.Text as T
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import qualified Graphics.Vty as V
import Brick.Forms (newForm, Form, editTextField, renderForm, invalidFormInputAttr, focusedFormInputAttr, handleFormEvent, (@@=))
import Lens.Micro.TH (makeLenses)
import Lens.Micro ((^.), (&), (.~))
import Graphics.Vty (black, yellow, white, red)
import Data.Text (empty)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Client (Connection)

-------------- Menu Types ----------------

-- States
data ConnectionState =
  Idle | Connecting | Error String | Success
  deriving (Show)

data ConnectionInfo = ConnectionInfo {
    _serverIP :: T.Text
  , _tableID :: T.Text
} deriving (Show)

data MenuState = MenuState {
    _connectionInfo :: ConnectionInfo
  , _connectionState :: ConnectionState
  , _connectionInfoForm :: Form ConnectionInfo MenuEvent MenuResourceName
}

-- Event


data MenuEvent = ConnectionSuccess Connection | ConnectionFailed String

-- Resource
data MenuResourceName =
  MenuServerIPField | MenuTableIDField
  deriving (Eq, Ord, Show)

-- Convenient Lenses
makeLenses ''ConnectionInfo
makeLenses ''MenuState

---------- Initial State for the menu --------------

initialMenuState :: MenuState
initialMenuState = MenuState ci Idle (makeConnectionForm ci)
                    where ci = ConnectionInfo empty empty


---------- Brick App ------------

app :: App MenuState MenuEvent MenuResourceName
app = App { appDraw = drawMenu
          , appChooseCursor = showFirstCursor
          , appHandleEvent = handleEvent
          , appStartEvent = return
          , appAttrMap = const menuAttrMap
          }

---------- Brick Attribute Map ----------

menuAttrMap :: AttrMap
menuAttrMap = attrMap V.defAttr
  [
    (focusedFormInputAttr, black `on` yellow)
  , (invalidFormInputAttr, white `on` red)
  ]

---------- Handle Brick Events ------------

handleEvent :: MenuState -> BrickEvent MenuResourceName MenuEvent -> EventM MenuResourceName (Next MenuState)

handleEvent s (AppEvent (ConnectionSuccess c)) = error "Not implemented"
handleEvent s (AppEvent (ConnectionFailed e)) = continue (s & connectionState .~ Error e)

handleEvent s (VtyEvent (V.EvKey V.KEnter [])) = connectToTable s
handleEvent s (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt s
handleEvent s ev = do
  newForm <- handleFormEvent ev (s ^. connectionInfoForm)
  continue (s & connectionInfoForm .~ newForm)


connectToTable :: MenuState -> EventM MenuResourceName (Next MenuState)
connectToTable s = liftIO (requestConnectToTable (s ^. connectionInfo)) >> continue (s & connectionState .~ Connecting)



requestConnectToTable :: connectionInfo -> IO ()
requestConnectToTable ci = return () -- TODO

---------- Brick Drawing --------------

drawMenu :: MenuState -> [Widget MenuResourceName]
drawMenu s = [vBox
              [
                C.hCenter $ str "Connect to Table"
              , vLimit 1 $ drawStatus s
              , padTop (Pad 2) $ C.center $ renderForm $ s ^. connectionInfoForm
              , str "Press Tab to switch fields"
              , str "Press enter to confirm"
              , str "Press q to exit"
              ]
            ]

drawStatus :: MenuState -> Widget n
drawStatus s = case s ^. connectionState of
  Idle -> str ""
  Connecting -> str "Connecting.... "
  Error s -> str ("Error: " ++ s)
  Success -> str "Success fully connected to table"

------- Brick Input form creation -----------
makeConnectionForm :: ConnectionInfo -> Form ConnectionInfo MenuEvent MenuResourceName
makeConnectionForm =
    newForm [
          (str "Server IP: " <+>) @@= editTextField serverIP MenuServerIPField (Just 1)
        , (str "Table ID: " <+>) @@= editTextField tableID MenuTableIDField (Just 1)
    ]
