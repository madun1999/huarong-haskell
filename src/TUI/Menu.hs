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
import Brick.Forms (newForm, Form (formState), editTextField, renderForm, invalidFormInputAttr, focusedFormInputAttr, handleFormEvent, (@@=))
import Lens.Micro.TH (makeLenses)
import Lens.Micro ((^.), (&), (.~))
import Graphics.Vty (black, yellow, white, red)
import Data.Text (empty, null)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Client (ConnectionDetails, tableID, connectionCallbacks, sendCreateRoomMsgCallBack, sendJoinRoomMsgCallBack, localPlayerID)

-------------- Menu Types ----------------

-- States
data ConnectionState =
  Idle | Connecting | Error String | Success
  deriving (Show)

data ConnectionInfoText = ConnectionInfoText {
    _serverIP :: T.Text
  , _tableID :: T.Text
} deriving (Show)

data MenuState = MenuState {
    _connectionState :: ConnectionState
  , _connectionInfoForm :: Form ConnectionInfoText MenuEvent MenuResourceName
  , _connectionDetails :: ConnectionDetails
}

-- Event


data MenuEvent = ConnectionEvent String
  deriving (Show, Read)

-- Resource
data MenuResourceName =
  MenuServerIPField | MenuTableIDField
  deriving (Eq, Ord, Show)

-- Convenient Lenses
makeLenses ''ConnectionInfoText
makeLenses ''MenuState

---------- Initial State for the menu --------------

initialMenuState :: ConnectionDetails -> MenuState
initialMenuState cd = MenuState Idle (makeConnectionForm ci) cd
                    where ci = ConnectionInfoText empty empty


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

---------- Create Events from Server Message ----------

menuMessageEvent :: String -> MenuEvent
menuMessageEvent s = ConnectionEvent s

---------- Handle Brick Events ------------

handleEvent :: MenuState -> BrickEvent MenuResourceName MenuEvent -> EventM MenuResourceName (Next MenuState)

handleEvent s (AppEvent (ConnectionEvent str))
  | Prelude.null str = continue (s & connectionState .~ Error "1")
  | otherwise = halt (s & connectionDetails . Client.tableID .~ str)


handleEvent s (VtyEvent (V.EvKey V.KEnter [])) = connectToTable s
handleEvent s (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt s
handleEvent s ev = do
  newForm <- handleFormEvent ev (s ^. connectionInfoForm)
  continue (s & connectionInfoForm .~ newForm)


connectToTable :: MenuState -> EventM MenuResourceName (Next MenuState)
connectToTable s = do 
  s1 <- liftIO (requestConnectToTable s) 
  continue (s1 & connectionState .~ Connecting)



requestConnectToTable :: MenuState -> IO MenuState
requestConnectToTable s
  | Data.Text.null tID = ccb ^. sendCreateRoomMsgCallBack >> return (s & connectionDetails . localPlayerID .~ 0)
  | otherwise  = (ccb ^. sendJoinRoomMsgCallBack) (T.unpack tID) >> return (s & connectionDetails . localPlayerID .~ 1)
  where tID = formState (s ^. connectionInfoForm) ^. TUI.Menu.tableID
        cd  = s ^. connectionDetails
        ccb = cd ^. connectionCallbacks

---------- Brick Drawing --------------

drawMenu :: MenuState -> [Widget MenuResourceName]
drawMenu s = [vBox
              [
                C.hCenter $ str "Connect to a Table"
              , vLimit 1 $ drawStatus s
              , padTop (Pad 2) $ C.center $ renderForm $ s ^. connectionInfoForm
              , str "Press Tab to switch fields"
              , str "Press enter to confirm"
              , str "Leave Table ID blank to create a new room"
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
makeConnectionForm :: ConnectionInfoText -> Form ConnectionInfoText MenuEvent MenuResourceName
makeConnectionForm =
    newForm [
          (str "Server IP: " <+>) @@= editTextField serverIP MenuServerIPField (Just 1)
        , (str "Table ID: " <+>) @@= editTextField TUI.Menu.tableID MenuTableIDField (Just 1)
    ]
