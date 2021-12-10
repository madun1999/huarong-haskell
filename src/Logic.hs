{-# LANGUAGE TemplateHaskell #-}

module Logic where
import Lens.Micro ((^.), (&), (.~))
import Data.Maybe
import Prelude hiding (Left, Right)
import Control.Lens (element)

-- Tiles represent different roles
type Tile = Maybe Role
-- use a 5x4 matrix to represent the grid
type Grid = [[Tile]]

numRows :: Int
numRows = 5
numCols :: Int
numCols = 4

-- status : Caocao already escape or not
-- name : current player's name
data Game = Game
  { grid  :: Grid
  , step :: Int
  , status  :: Bool
  , name :: String
  } deriving (Eq, Show)


data Direction
    = Up | Left | Right | Down
    deriving (Eq, Show, Read)

-- different roles will take different numbers of tiles
data Role
    = Zhangfei
    | Caocao
    | Machao
    | Zhaoyun
    | Guanyu
    | Huangzhong
    | Zu1
    | Zu2
    | Zu3
    | Zu4
    deriving (Eq, Show)


-- grid[x][y] stands for the top left tile of current character
-- if movement is valid :
--    update(step, grid)
--    if caocao already escape :
--      update(status)      
move :: Game -> Int -> Int -> Direction-> Game
move game x y direction 
  | x > 0 && gr !! y !! (x - 1) == gr !! y !! x = move game (x-1) y direction
  | y > 0 && gr !! (y - 1) !! x == gr !! y !! x = move game x (y-1) direction
  | otherwise = 
          let newGrid = moveGrid (grid game) y x direction
              st  = isWin newGrid
          in  Game {grid = newGrid, step = step game + 1, status = st, name = name game}
  where gr = grid game

--the initial board
initialPosition :: Game
initialPosition = Game {
  grid = [[Just Zhangfei,Just Caocao,Just Caocao,Just Machao],
          [Just Zhangfei,Just Caocao,Just Caocao,Just Machao],
          [Just Zhaoyun,Just Guanyu,Just Guanyu,Just Huangzhong],
          [Just Zhaoyun,Just Zu2,Just Zu3,Just Huangzhong],
          [Just Zu1,Nothing,Nothing,Just Zu4]]
  , step = 0
  , status = False
  , name = "player"
}

-- a simple initial board 
initialPositionSimple :: Game
initialPositionSimple = Game {
  grid = [[Nothing,Just Caocao,Just Caocao,Nothing],
          [Nothing,Just Caocao,Just Caocao,Nothing],
          [Nothing,Nothing,Nothing,Nothing],
          [Nothing,Nothing,Nothing,Nothing],
          [Nothing,Nothing,Nothing,Nothing]]
  , step = 0
  , status = False
  , name = "player"
}

-- >>> move initialPosition 0 1 Down
-- Game {grid = [[Zhangfei,Caocao,Caocao,Machao],[Zhangfei,Caocao,Caocao,Machao],[Zhaoyun,Guanyu,Guanyu,Huangzhong],[Zhaoyun,Zu2,Zu3,Huangzhong],[Zu1,Bai,Bai,Zu4]], step = 1, status = False, name = "player"}
-- Game {grid = [[Zhangfei,Caocao,Caocao,Machao],[Zhangfei,Caocao,Caocao,Machao],[Zhaoyun,Guanyu,Guanyu,Huangzhong],[Zhaoyun,Zu2,Zu3,Huangzhong],[Zu1,Bai,Bai,Zu4]], step = 1, status = False, name = "player"}
--

-- >>> grid initialPosition
-- [[Zhangfei,Caocao,Caocao,Machao],[Zhangfei,Caocao,Caocao,Machao],[Zhaoyun,Guanyu,Guanyu,Huangzhong],[Zhaoyun,Zu2,Zu3,Huangzhong],[Zu1,Bai,Bai,Zu4]]
--

setName:: Game -> String -> Game
setName game s = Game{grid = grid game, step = step game, status = status game, name = s}




isWin :: Grid -> Bool
isWin gr = gr !! 4 !! 1 == Just Caocao && gr !! 4 !! 2 == Just Caocao

moveGrid :: Grid -> Int -> Int -> Direction -> Grid
moveGrid curGrid x y direction = do
                            let curRole = curGrid !! x !! y
                            if curRole == Just Caocao then moveCaocao curGrid x y direction
                            else if curRole == Just Guanyu then moveGuan curGrid x y direction
                            else if curRole == Just Zu1 || curRole == Just Zu2 || curRole == Just Zu3 || curRole == Just Zu4 then moveZu curGrid x y direction
                            else if isNothing curRole then curGrid
                            else moveFour curGrid x y direction


moveCaocaoDown :: Grid -> Int -> Int ->  Grid
moveCaocaoDown curGrid x y  = do
                                let tmp1 = curGrid & element (x + 2) . element y .~ Just Caocao
                                let tmp2 = tmp1 & element (x + 2). element (y + 1) .~ Just Caocao
                                let tmp3 = tmp2 & element x. element y .~ Nothing
                                let tmp4 = tmp3 & element x. element (y + 1) .~ Nothing
                                tmp4

moveCaocaoLeft :: Grid -> Int -> Int ->  Grid
moveCaocaoLeft curGrid x y  = do
                                    let tmp1 = curGrid & element x . element (y - 1) .~ Just Caocao
                                    let tmp2 = tmp1 & element (x + 1). element (y - 1) .~ Just Caocao
                                    let tmp3 = tmp2 & element x. element (y + 1) .~ Nothing
                                    let tmp4 = tmp3 & element (x + 1). element (y + 1) .~ Nothing
                                    tmp4

moveCaocaoRight :: Grid -> Int -> Int ->  Grid
moveCaocaoRight curGrid x y  = do
                                    let tmp1 = curGrid & element x . element (y + 2) .~ Just Caocao
                                    let tmp2 = tmp1 & element (x + 1). element (y + 2) .~ Just Caocao
                                    let tmp3 = tmp2 & element x. element (y) .~ Nothing
                                    let tmp4 = tmp3 & element (x + 1). element (y) .~ Nothing
                                    tmp4

moveCaocaoUp :: Grid -> Int -> Int ->  Grid
moveCaocaoUp curGrid x y  = do
                              let tmp1 = curGrid & element (x - 1) . element y .~ Just Caocao
                              let tmp2 = tmp1 & element (x - 1). element (y + 1) .~ Just Caocao
                              let tmp3 = tmp2 & element (x + 1). element y .~ Nothing
                              let tmp4 = tmp3 & element (x + 1). element (y + 1) .~ Nothing
                              tmp4

moveCaocao :: Grid -> Int -> Int -> Direction -> Grid
moveCaocao curGrid x y direction =  do
                                    if curGrid !! x !! y == Just Caocao then
                                        case direction of 
                                          Up ->  if x - 1 >= 0 && y + 1 < 4 && curGrid !! (x - 1) !!y == Nothing && curGrid !! (x - 1) !!(y + 1) == Nothing then moveCaocaoUp curGrid x y else curGrid
                                          Left -> if x + 1 < 5 && y - 1 >= 0 && curGrid !! x !!(y - 1) == Nothing && curGrid !! (x + 1) !!(y - 1) == Nothing then moveCaocaoLeft curGrid x y else curGrid
                                          Right -> if x + 1 < 5 && y + 2 <4 && curGrid !! x !!(y + 2) == Nothing && curGrid !! (x + 1) !!(y + 2) == Nothing then moveCaocaoRight curGrid x y else curGrid
                                          Down -> if x + 2 < 5 && y + 1 <4 && curGrid !!(x + 2) !!(y + 1) == Nothing && curGrid !! (x + 2) !!y == Nothing then moveCaocaoDown curGrid x y else curGrid
                                    else curGrid       


-- >>> moveCaocao (grid initialPosition) 0 0 RightSide
-- [[Zhangfei,Caocao,Caocao,Machao],[Zhangfei,Caocao,Caocao,Machao],[Zhaoyun,Guanyu,Guanyu,Huangzhong],[Zhaoyun,Zu2,Zu3,Huangzhong],[Zu1,Bai,Bai,Zu4]]
--


-- >>>  (grid initialPosition) & element 0. element 2 .~ Bai
-- [[Zhangfei,Caocao,Bai,Machao],[Zhangfei,Caocao,Caocao,Machao],[Zhaoyun,Guanyu,Guanyu,Huangzhong],[Zhaoyun,Zu2,Zu3,Huangzhong],[Zu1,Bai,Bai,Zu4]]
--


moveFourDown :: Grid -> Int -> Int -> Tile -> Grid
moveFourDown curGrid x y curRole   = do
                                    let tmp1 = curGrid & element (x + 2) . element y .~ curRole
                                    let tmp2 = tmp1 & element x. element y .~ Nothing
                                    tmp2

moveFourLeft :: Grid -> Int -> Int -> Tile -> Grid
moveFourLeft curGrid x y curRole   = do
                                    let tmp1 = curGrid & element x . element (y - 1) .~ curRole
                                    let tmp2 = tmp1 & element (x + 1). element (y - 1) .~ curRole
                                    let tmp3 = tmp2 & element x. element y .~ Nothing
                                    let tmp4 = tmp3 & element (x + 1). element y .~ Nothing
                                    tmp4

moveFourRight :: Grid -> Int -> Int -> Tile -> Grid
moveFourRight curGrid x y curRole   = do
                                    let tmp1 = curGrid & element x . element (y + 1) .~ curRole
                                    let tmp2 = tmp1 & element (x + 1). element (y + 1) .~ curRole
                                    let tmp3 = tmp2 & element x. element y .~ Nothing
                                    let tmp4 = tmp3 & element (x + 1). element y .~ Nothing
                                    tmp4

moveFourUp :: Grid -> Int -> Int -> Tile -> Grid
moveFourUp curGrid x y curRole = do
                                    let tmp1 = curGrid & element (x + 1) . element y .~ Nothing
                                    let tmp2 = tmp1 & element (x - 1). element y .~ curRole
                                    tmp2

moveFour :: Grid -> Int -> Int -> Direction -> Grid
moveFour curGrid x y direction =  do
                                    let curRole = getRole curGrid x y
                                    if curRole == Just Huangzhong || curRole == Just Zhaoyun || curRole == Just Machao || curRole == Just Zhangfei then
                                        case direction of 
                                          Up -> if x -1 >= 0 && curGrid !! (x-1) !!y == Nothing  then moveFourUp curGrid x y curRole else curGrid
                                          Left -> if x + 1 < 5 && y - 1 >= 0 && curGrid !! x !!(y - 1) == Nothing && curGrid !! (x + 1) !!(y - 1) == Nothing then moveFourLeft curGrid x y curRole else curGrid
                                          Right -> if x + 1 < 5 && y + 1 < 4 && curGrid !! x !!(y + 1) == Nothing && curGrid !! (x + 1) !!(y + 1) == Nothing then moveFourRight curGrid x y curRole else curGrid
                                          Down -> if x + 2 <5 && curGrid !! (x+2) !!y == Nothing  then moveFourDown curGrid x y curRole else curGrid
                                    else curGrid        

-- >>> moveFour (grid initialPosition) 0 0 RightSide
-- [[Zhangfei,Caocao,Caocao,Machao],[Zhangfei,Caocao,Caocao,Machao],[Zhaoyun,Guanyu,Guanyu,Huangzhong],[Zhaoyun,Zu2,Zu3,Huangzhong],[Zu1,Bai,Bai,Zu4]]
--


getRole :: Grid -> Int -> Int -> Tile
getRole curGrid x y = curGrid!!x!!y




moveZuDown :: Grid -> Int -> Int -> Tile -> Grid
moveZuDown curGrid x y curRole   = do
                                    let tmp1 = curGrid & element (x + 1) . element y .~ curRole
                                    let tmp2 = tmp1 & element x. element y .~ Nothing
                                    tmp2

moveZuLeft :: Grid -> Int -> Int -> Tile -> Grid
moveZuLeft curGrid x y curRole   = do
                                    let tmp1 = curGrid & element x . element (y - 1) .~ curRole
                                    let tmp2 = tmp1 & element x. element y .~ Nothing
                                    tmp2

moveZuRight :: Grid -> Int -> Int -> Tile -> Grid
moveZuRight curGrid x y curRole   = do
                                    let tmp1 = curGrid & element x . element (y + 1) .~ curRole
                                    let tmp2 = tmp1 & element x. element y .~ Nothing
                                    tmp2

moveZuUp :: Grid -> Int -> Int -> Tile -> Grid
moveZuUp curGrid x y curRole = do
                                    let tmp1 = curGrid & element (x - 1) . element y .~ curRole
                                    let tmp2 = tmp1 & element x. element y .~ Nothing
                                    tmp2

moveZu :: Grid -> Int -> Int -> Direction -> Grid
moveZu curGrid x y direction =  do
                                    let curRole = getRole curGrid x y
                                    if curRole == Just Zu1 || curRole == Just Zu2 || curRole == Just Zu3 || curRole == Just Zu4 then
                                        case direction of 
                                          Up -> if x - 1 >= 0 && curGrid !! (x - 1) !! y == Nothing  then moveZuUp curGrid x y curRole else curGrid
                                          Left -> if y - 1 >= 0 && curGrid !! x !!(y - 1) == Nothing  then moveZuLeft curGrid x y curRole else curGrid
                                          Right -> if y + 1 < 4 && curGrid !! x !!(y + 1) == Nothing  then moveZuRight curGrid x y curRole else curGrid
                                          Down -> if x + 1 < 5 && curGrid !! (x + 1) !! y == Nothing  then moveZuDown curGrid x y curRole else curGrid
                                    else curGrid     

-- >>> moveZu (grid initialPosition) 4 0 LeftSide
-- [[Zhangfei,Caocao,Caocao,Machao],[Zhangfei,Caocao,Caocao,Machao],[Zhaoyun,Guanyu,Guanyu,Huangzhong],[Zhaoyun,Zu2,Zu3,Huangzhong],[Zu1,Bai,Bai,Zu4]]
--

moveGuanDown :: Grid -> Int -> Int  -> Grid
moveGuanDown curGrid x y = do
                                    let tmp1 = curGrid & element (x + 1) . element y .~ Just Guanyu
                                    let tmp2 = tmp1 & element (x + 1) . element (y+1) .~ Just Guanyu
                                    let tmp3 = tmp2 & element x. element y .~ Nothing
                                    let tmp4 = tmp3 & element x. element (y+1) .~ Nothing
                                    tmp4

moveGuanLeft :: Grid -> Int -> Int  -> Grid
moveGuanLeft curGrid x y = do
                                    let tmp1 = curGrid & element x . element (y - 1) .~ Just Guanyu
                                    let tmp2 = tmp1 & element x. element (y + 1) .~ Nothing
                                    tmp2

moveGuanRight :: Grid -> Int -> Int  -> Grid
moveGuanRight curGrid x y = do
                                    let tmp1 = curGrid & element x . element (y + 2) .~ Just Guanyu
                                    let tmp2 = tmp1 & element x. element y  .~ Nothing
                                    tmp2

moveGuanUp :: Grid -> Int -> Int  -> Grid
moveGuanUp curGrid x y  = do
                                    let tmp1 = curGrid & element (x-1) . element y .~ Just Guanyu
                                    let tmp2 = tmp1 & element (x-1) . element (y+1) .~ Just Guanyu
                                    let tmp3 = tmp2 & element x. element y .~ Nothing
                                    let tmp4 = tmp3 & element x. element (y+1) .~ Nothing
                                    tmp4

moveGuan :: Grid -> Int -> Int -> Direction -> Grid
moveGuan curGrid x y direction =  do
                                    let curRole = getRole curGrid x y
                                    if curRole == Just Guanyu then
                                        case direction of 
                                          Up ->  if x - 1 >= 0 && y + 1 < 4 && curGrid !! (x - 1) !!y == Nothing && curGrid !! (x - 1) !!(y + 1) == Nothing then moveGuanUp curGrid x y else curGrid
                                          Left -> if y - 1 >= 0 && curGrid !! x !!(y - 1) == Nothing  then moveGuanLeft curGrid x y else curGrid
                                          Right -> if y + 2 <4 && curGrid !! x !!(y + 2) == Nothing then moveGuanRight curGrid x y else curGrid
                                          Down -> if x + 1 < 5 && y + 1 <4 && curGrid !!(x + 1) !!(y + 1) == Nothing && curGrid !! (x + 1) !!y == Nothing then moveGuanDown curGrid x y else curGrid
                                    else curGrid

-- >>> moveGuan (grid initialPosition) 2 1 Up
-- [[Zhangfei,Caocao,Caocao,Machao],[Zhangfei,Caocao,Caocao,Machao],[Zhaoyun,Guanyu,Guanyu,Huangzhong],[Zhaoyun,Zu2,Zu3,Huangzhong],[Zu1,Bai,Bai,Zu4]]
--

