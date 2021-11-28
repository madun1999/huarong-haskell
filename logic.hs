-- Tiles represent different roles
type Tile = Role
-- use a 5x4 matrix to represent the grid
type Grid = [[Tile]]


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
    deriving (Eq, Show)

-- grid[x][y] stands for the top left tile of current character
-- if movement is valid :
--    update(step, grid)
--    if caocao already escape :
--      update(status)      
move :: Game -> Int -> Int -> Direction -> Game
move game x y direction = game 

--the initial board
initialPosition :: Game
initialPosition = Game {
  grid = [[Zhangfei,Caocao,Caocao,Machao],
          [Zhangfei,Caocao,Caocao,Machao],
          [Zhaoyun,Guanyu,Guanyu,Huangzhong],
          [Zhaoyun,Zu2,Zu3,Huangzhong],
          [Zu1,Nothing,Nothing,Zu4]]
  , step = 0
  , status = False,
  , name = Nothing
}

