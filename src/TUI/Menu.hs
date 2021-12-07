{-# LANGUAGE TemplateHaskell #-}

module TUI.Menu where

import Brick

import qualified Data.Text as T
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import qualified Graphics.Vty as V
import Brick.Forms (newForm, Form, editTextField, renderForm, invalidFormInputAttr)
import Lens.Micro.TH (makeLenses)

type ServerIP = String

type TableID = String 

data ConnectionState = 
  Idle | Connecting | Error String | Success
  deriving (Show)

data ConnectionInfo = ConnectionInfo {
    _serverIP :: T.Text
  , _tableID :: T.Text
} deriving (Show)

data MenuEvent = MenuEvent
data MenuResourceName = 
  MenuServerIPField | MenuTableIDField
  deriving (Eq, Ord, Show)

data MenuState = MenuState {
    _connectionInfo :: ConnectionInfo
  , _connectionState :: ConnectionState
} deriving (Show)

makeLenses ''ConnectionInfo
makeLenses ''MenuState



app :: App MenuState MenuEvent MenuResourceName
app = App { appDraw = drawMenu 
          , appChooseCursor = neverShowCursor
          , appHandleEvent = handleEvent
          , appStartEvent = return
          , appAttrMap = const boardAttrMap
          }

boardAttrMap :: AttrMap
boardAttrMap = attrMap V.defAttr 
  [
    (focusedFormInputAttr, black `on` yellow)
  , (invalidFormInputAttr, white `on` red)  
  ]

handleEvent :: MenuState -> BrickEvent MenuResourceName MenuEvent -> EventM MenuResourceName (Next MenuState)
handleEvent g _ = continue g

drawMenu :: MenuState -> [Widget MenuResourceName]
drawMenu = error "Not implemented"

renderForm (Form s e n)

-- https://github.com/jtdaugherty/brick/blob/master/docs/guide.rst#input-forms

makeConnectionForm :: ConnectionInfo -> Form ConnectionInfo MenuEvent MenuResourceName
makeConnectionForm = 
    newForm [
          editTextField serverIP MenuServerIPField (Just 0)
        , editTextField tableID MenuTableIDField (Just 1)
    ]
