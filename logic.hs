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
    | Zu
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
          [Zhaoyun,Zu,Zu,Huangzhong],
          [Zu,Nothing,Nothing,Zu]]
  , step = 0
  , status = False,
  , name = Nothing
}

-- >>> [1,2,3,4] !! 2