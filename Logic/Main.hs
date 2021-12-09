module Main where

import Lib
import Control.Lens
import Data.Maybe




-- >>> [[1,2,3],[4,5,6]] & element 0 . element 1 .~ 9
-- [[1,9,3],[4,5,6]]
--

main :: IO ()
main = someFunc


-- use a 5x4 matrix to represent the grid
type Grid = [[Role]]


-- status : Caocao already escape or not
-- name : current player's name
data Game = Game
  { grid  :: Grid
  , step :: Int
  , status  :: Bool
  , name :: String
  } deriving (Eq, Show)


data Direction 
    = Up | LeftSide | RightSide | Down
    deriving (Eq, Show)

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
    | Bai
    deriving (Eq, Show)

-- grid[x][y] stands for the top left tile of current character
-- if movement is valid :
--    update(step, grid)
--    if caocao already escape :
--      update(status)      
-- move :: Game -> Int -> Int -> Direction -> Game
-- move game x y direction = game 

--the initial board
initialPosition :: Game
initialPosition = Game {
 grid = [[Zhangfei,Caocao,Caocao,Machao],
        [Zhangfei,Caocao,Caocao,Machao], 
        [Zhaoyun,Guanyu,Guanyu,Huangzhong], 
        [Zhaoyun,Zu2,Zu3,Huangzhong], 
        [Zu1,Bai,Bai,Zu4]]
 ,step = 0
 ,status = False
 ,name = "player"
}

setName:: Game -> String -> Game
setName game s = Game{grid = grid game, step = step game + 1, status = status game, name = s}


isWin :: Game -> Bool
isWin game = grid game !!4 !! 1 == Caocao

-- The move Function
move :: Game -> Int -> Int -> Direction-> Game
move game x y direction = 
              Game{grid = moveGrid (grid game) x y direction, step = step game + 1, status = status game, name = name game}
              
              
-- >>> move initialPosition 0 1 Down
-- Game {grid = [[Zhangfei,Caocao,Caocao,Machao],[Zhangfei,Caocao,Caocao,Machao],[Zhaoyun,Guanyu,Guanyu,Huangzhong],[Zhaoyun,Zu2,Zu3,Huangzhong],[Zu1,Bai,Bai,Zu4]], step = 1, status = False, name = "player"}
--


-- >>> grid initialPosition
-- [[Zhangfei,Caocao,Caocao,Machao],[Zhangfei,Caocao,Caocao,Machao],[Zhaoyun,Guanyu,Guanyu,Huangzhong],[Zhaoyun,Zu2,Zu3,Huangzhong],[Zu1,Bai,Bai,Zu4]]
--

moveGrid :: Grid -> Int -> Int -> Direction -> Grid
moveGrid curGrid x y direction = do
                            let curRole = curGrid !! x !! y
                            if curRole == Caocao then moveCaocao curGrid x y direction      
                            else if curRole == Guanyu then moveGuan curGrid x y direction     
                            else if curRole == Zu1 || curRole == Zu2 || curRole == Zu3 || curRole == Zu4 then moveZu curGrid x y direction 
                            else if curRole == Bai then curGrid
                            else moveFour curGrid x y direction 
                          


moveCaocaoDown :: Grid -> Int -> Int ->  Grid
moveCaocaoDown curGrid x y  = do
                                let tmp1 = curGrid & element (x + 2) . element y .~ Caocao
                                let tmp2 = tmp1 & element (x + 2). element (y + 1) .~ Caocao
                                let tmp3 = tmp2 & element x. element y .~ Bai
                                let tmp4 = tmp3 & element x. element (y + 1) .~ Bai
                                tmp4

moveCaocaoLeft :: Grid -> Int -> Int ->  Grid
moveCaocaoLeft curGrid x y  = do
                                    let tmp1 = curGrid & element x . element (y - 1) .~ Caocao
                                    let tmp2 = tmp1 & element (x + 1). element (y - 1) .~ Caocao
                                    let tmp3 = tmp2 & element x. element (y + 1) .~ Bai
                                    let tmp4 = tmp3 & element (x + 1). element (y + 1) .~ Bai
                                    tmp4

moveCaocaoRight :: Grid -> Int -> Int ->  Grid
moveCaocaoRight curGrid x y  = do
                                    let tmp1 = curGrid & element x . element (y + 1) .~ Caocao
                                    let tmp2 = tmp1 & element (x + 1). element (y + 1) .~ Caocao
                                    let tmp3 = tmp2 & element x. element (y - 1) .~ Bai
                                    let tmp4 = tmp3 & element (x + 1). element (y - 1) .~ Bai
                                    tmp4

moveCaocaoUp :: Grid -> Int -> Int ->  Grid
moveCaocaoUp curGrid x y  = do
                                    let tmp1 = curGrid & element (x - 1) . element y .~ Caocao
                                    let tmp2 = tmp1 & element (x - 1). element (y + 1) .~ Caocao
                                    let tmp3 = tmp2 & element (x + 1). element y .~ Bai
                                    let tmp4 = tmp3 & element (x + 1). element (y + 1) .~ Bai
                                    tmp4

moveCaocao :: Grid -> Int -> Int -> Direction -> Grid
moveCaocao curGrid x y direction =  do
                                    if curGrid !! x !! y == Caocao then
                                        case direction of 
                                          Up ->  if x - 1 >= 0 && y + 1 < 4 && curGrid !! (x - 1) !!y == Bai && curGrid !! (x - 1) !!(y + 1) == Bai then moveCaocaoUp curGrid x y else curGrid
                                          LeftSide -> if x + 1 < 5 && y - 1 >= 0 && curGrid !! x !!(y - 1) == Bai && curGrid !! (x + 1) !!(y - 1) == Bai then moveCaocaoLeft curGrid x y else curGrid
                                          RightSide -> if x + 1 < 5 && y + 2 <4 && curGrid !! x !!(y + 2) == Bai && curGrid !! (x + 1) !!(y + 2) == Bai then moveCaocaoRight curGrid x y else curGrid
                                          Down -> if x + 1 < 5 && y + 1 <4 && curGrid !!(x + 1) !!(y + 1) == Bai && curGrid !! (x + 1) !!y == Bai then moveCaocaoDown curGrid x y else curGrid
                                    else curGrid                                    
-- >>> moveCaocao (grid initialPosition) 0 0 RightSide
-- [[Zhangfei,Caocao,Caocao,Machao],[Zhangfei,Caocao,Caocao,Machao],[Zhaoyun,Guanyu,Guanyu,Huangzhong],[Zhaoyun,Zu2,Zu3,Huangzhong],[Zu1,Bai,Bai,Zu4]]
--


-- >>>  (grid initialPosition) & element 0. element 2 .~ Bai
-- [[Zhangfei,Caocao,Bai,Machao],[Zhangfei,Caocao,Caocao,Machao],[Zhaoyun,Guanyu,Guanyu,Huangzhong],[Zhaoyun,Zu2,Zu3,Huangzhong],[Zu1,Bai,Bai,Zu4]]
--


-- >>> let x = 1
-- >>> let y = 2
--

moveFourDown :: Grid -> Int -> Int -> Role -> Grid
moveFourDown curGrid x y curRole   = do
                                    let tmp1 = curGrid & element (x + 1) . element y .~ curRole
                                    let tmp2 = tmp1 & element (x - 1). element y .~ Bai
                                    tmp2

moveFourLeft :: Grid -> Int -> Int -> Role -> Grid
moveFourLeft curGrid x y curRole   = do
                                    let tmp1 = curGrid & element x . element (y - 1) .~ curRole
                                    let tmp2 = tmp1 & element (x + 1). element (y - 1) .~ curRole
                                    let tmp3 = tmp2 & element x. element y .~ Bai
                                    let tmp4 = tmp3 & element (x + 1). element y .~ Bai
                                    tmp4

moveFourRight :: Grid -> Int -> Int -> Role -> Grid
moveFourRight curGrid x y curRole   = do
                                    let tmp1 = curGrid & element x . element (y + 1) .~ curRole
                                    let tmp2 = tmp1 & element (x + 1). element (y + 1) .~ curRole
                                    let tmp3 = tmp2 & element x. element y .~ Bai
                                    let tmp4 = tmp3 & element (x + 1). element y .~ Bai
                                    tmp4

moveFourUp :: Grid -> Int -> Int -> Role -> Grid
moveFourUp curGrid x y curRole = do
                                    let tmp1 = curGrid & element (x + 1) . element y .~ Bai
                                    let tmp2 = tmp2 & element (x - 1). element y .~ curRole
                                    tmp2

moveFour :: Grid -> Int -> Int -> Direction -> Grid
moveFour curGrid x y direction =  do
                                    let curRole = getRole curGrid x y
                                    if curRole == Huangzhong || curRole == Zhaoyun || curRole == Machao || curRole == Zhangfei then
                                        case direction of 
                                          Up -> if x -1 >= 0 && curGrid !! (x-1) !!y == Bai  then moveFourUp curGrid x y curRole else curGrid
                                          LeftSide -> if x + 1 < 5 && y - 1 >= 0 && curGrid !! x !!(y - 1) == Bai && curGrid !! (x + 1) !!(y - 1) == Bai then moveFourLeft curGrid x y curRole else curGrid
                                          RightSide -> if x + 1 < 5 && y + 1 < 4 && curGrid !! x !!(y + 1) == Bai && curGrid !! (x + 1) !!(y + 1) == Bai then moveFourRight curGrid x y curRole else curGrid
                                          Down -> if x + 1 <5 && curGrid !! (x+1) !!y == Bai  then moveFourDown curGrid x y curRole else curGrid
                                    else curGrid                                    

getRole :: Grid -> Int -> Int ->Role
getRole curGrid x y = curGrid!!x!!y

-- >>> moveFour (grid initialPosition) 0 0 RightSide
-- [[Bai,Zhangfei,Caocao,Machao],[Bai,Zhangfei,Caocao,Machao],[Zhaoyun,Guanyu,Guanyu,Huangzhong],[Zhaoyun,Zu2,Zu3,Huangzhong],[Zu1,Bai,Bai,Zu4]]
--

moveZuDown :: Grid -> Int -> Int -> Role -> Grid
moveZuDown curGrid x y curRole   = do
                                    let tmp1 = curGrid & element (x + 1) . element y .~ curRole
                                    let tmp2 = tmp1 & element x. element y .~ Bai
                                    tmp2

moveZuLeft :: Grid -> Int -> Int -> Role -> Grid
moveZuLeft curGrid x y curRole   = do
                                    let tmp1 = curGrid & element x . element (y - 1) .~ curRole
                                    let tmp2 = tmp1 & element x. element y .~ Bai
                                    tmp2

moveZuRight :: Grid -> Int -> Int -> Role -> Grid
moveZuRight curGrid x y curRole   = do
                                    let tmp1 = curGrid & element x . element (y + 1) .~ curRole
                                    let tmp2 = tmp1 & element x. element y .~ Bai
                                    tmp2

moveZuUp :: Grid -> Int -> Int -> Role -> Grid
moveZuUp curGrid x y curRole = do
                                    let tmp1 = curGrid & element (x - 1) . element y .~ curRole
                                    let tmp2 = tmp2 & element x. element y .~ Bai
                                    tmp2

moveZu :: Grid -> Int -> Int -> Direction -> Grid
moveZu curGrid x y direction =  do
                                    let curRole = getRole curGrid x y
                                    if curRole == Zu1 || curRole == Zu2 || curRole == Zu3 || curRole == Zu4 then
                                        case direction of 
                                          Up -> if x - 1 >= 0 && curGrid !! (x - 1) !! y == Bai  then moveZuUp curGrid x y curRole else curGrid
                                          LeftSide -> if y - 1 >= 0 && curGrid !! x !!(y - 1) == Bai  then moveZuLeft curGrid x y curRole else curGrid
                                          RightSide -> if y + 1 < 4 && curGrid !! x !!(y + 1) == Bai  then moveZuRight curGrid x y curRole else curGrid
                                          Down -> if x + 1 < 5 && curGrid !! (x + 1) !! y == Bai  then moveZuDown curGrid x y curRole else curGrid
                                    else curGrid             


-- >>> moveZu (grid initialPosition) 4 0 Up
-- [[Zhangfei,Caocao,Caocao,Machao],[Zhangfei,Caocao,Caocao,Machao],[Zhaoyun,Guanyu,Guanyu,Huangzhong],[Zu1,Zu2,Zu3,Huangzhong],[Bai,Bai,Bai,Zu4]]
--

moveGuanDown :: Grid -> Int -> Int  -> Grid
moveGuanDown curGrid x y = do
                                    let tmp1 = curGrid & element (x + 1) . element y .~ Guanyu
                                    let tmp2 = tmp1 & element (x + 1) . element (y+1) .~ Guanyu
                                    let tmp3 = tmp2 & element x. element y .~ Bai
                                    let tmp4 = tmp3 & element x. element (y+1) .~ Bai
                                    tmp4

moveGuanLeft :: Grid -> Int -> Int  -> Grid
moveGuanLeft curGrid x y = do
                                    let tmp1 = curGrid & element x . element (y - 1) .~ Guanyu
                                    let tmp2 = tmp1 & element x. element (y + 1) .~ Bai
                                    tmp2

moveGuanRight :: Grid -> Int -> Int  -> Grid
moveGuanRight curGrid x y = do
                                    let tmp1 = curGrid & element x . element (y + 1) .~ Guanyu
                                    let tmp2 = tmp1 & element x. element y .~ Bai
                                    tmp2

moveGuanUp :: Grid -> Int -> Int  -> Grid
moveGuanUp curGrid x y  = do
                                    let tmp1 = curGrid & element (x-1) . element y .~ Guanyu
                                    let tmp2 = tmp1 & element (x-1) . element (y+1) .~ Guanyu
                                    let tmp3 = tmp2 & element x. element y .~ Bai
                                    let tmp4 = tmp3 & element x. element (y+1) .~ Bai
                                    tmp4

moveGuan :: Grid -> Int -> Int -> Direction -> Grid
moveGuan curGrid x y direction =  do
                                    let curRole = getRole curGrid x y
                                    if curRole == Guanyu then
                                        case direction of 
                                          Up ->  if x - 1 >= 0 && y + 1 < 4 && curGrid !! (x - 1) !!y == Bai && curGrid !! (x - 1) !!(y + 1) == Bai then moveGuanUp curGrid x y else curGrid
                                          LeftSide -> if y - 1 >= 0 && curGrid !! x !!(y - 1) == Bai  then moveGuanLeft curGrid x y else curGrid
                                          RightSide -> if y + 2 <4 && curGrid !! x !!(y + 2) == Bai then moveGuanRight curGrid x y else curGrid
                                          Down -> if x + 1 < 5 && y + 1 <4 && curGrid !!(x + 1) !!(y + 1) == Bai && curGrid !! (x + 1) !!y == Bai then moveGuanDown curGrid x y else curGrid
                                    else curGrid     

-- >>> moveGuan (grid initialPosition) 2 1 Up
-- [[Zhangfei,Caocao,Caocao,Machao],[Zhangfei,Guanyu,Guanyu,Machao],[Zhaoyun,Bai,Bai,Huangzhong],[Zhaoyun,Zu2,Zu3,Huangzhong],[Zu1,Bai,Bai,Zu4]]

