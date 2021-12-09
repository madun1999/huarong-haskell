module Test where

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

middlePosition :: Game
middlePosition = Game {
 grid = [[Zhangfei,Caocao,Caocao,Machao],
        [Zhangfei,Caocao,Caocao,Machao], 
        [Zhaoyun,Bai,Bai,Huangzhong], 
        [Zhaoyun,Guanyu,Guanyu,Huangzhong], 
        [Zu1,Zu2,Zu3,Zu4]]
 ,step = 3
 ,status = False
 ,name = "player"
}

middlePosition2 :: Game
middlePosition2 = Game {
 grid = [[Zhangfei,Caocao,Caocao,Machao],
        [Zhangfei,Caocao,Caocao,Machao], 
        [Zhaoyun,Guanyu,Guanyu,Huangzhong], 
        [Zhaoyun,Bai,Bai,Huangzhong], 
        [Zu1,Zu2,Zu3,Zu4]]
 ,step = 4
 ,status = False
 ,name = "player"
}

finalPosition :: Game
finalPosition = Game {
 grid = [[Zhangfei,Machao,Zhaoyun,Huangzhong],
        [Zhangfei,Machao,Zhaoyun,Huangzhong], 
        [Zu1,Zu2,Guanyu,Guanyu], 
        [Caocao,Caocao,Bai,Zu3], 
        [Caocao,Caocao,Bai,Zu4]]
 ,step = 80
 ,status = True
 ,name = "player"
}

finalPosition2 :: Game
finalPosition2 = Game {
 grid = [[Zhangfei,Machao,Zhaoyun,Huangzhong],
        [Zhangfei,Machao,Zhaoyun,Huangzhong], 
        [Zu1,Zu2,Guanyu,Guanyu], 
        [Bai,Caocao,Caocao,Zu3], 
        [Bai,Caocao,Caocao,Zu4]]
 ,step = 81
 ,status = True
 ,name = "player"
}




testMove :: Bool
testMove =  grid (move (move initialPosition 4 0 RightSide) 4 1 LeftSide) == grid initialPosition

testStep1 :: Bool 
testStep1 = step (move (move initialPosition 4 0 RightSide) 4 1 LeftSide) == 2

testStep2 :: Bool 
testStep2 = step (move initialPosition 4 0 RightSide) == 1
--  name check
testName1 :: Bool 
testName1 =  name initialPosition == "player"
-- change name
testName2 :: Bool 
testName2 =  name(setName initialPosition "Drake") == "Drake"

-- reach bound
testInvalidMove :: Bool
testInvalidMove = (grid (move initialPosition 0 0 LeftSide) == grid initialPosition) && (step (move initialPosition 0 0 LeftSide) == 1)

-- no space
testInvalidMove2 :: Bool
testInvalidMove2 = (grid (move initialPosition 0 1 Down) == grid initialPosition) && (step (move initialPosition 0 1 Down) == 1)

-- move larger brick 
testMove2 :: Bool
testMove2 =  (grid (move middlePosition 3 1 UpSide) == grid middlePosition2) && (step (move middlePosition 3 1 UpSide) == 4)

-- test end game 
testMove3 :: Bool
testMove3 =  (grid (move middlePosition 3 0 RightSide) == grid finalPosition2) && (step (move middlePosition 3 0 RightSide) == 81) && (status (move middlePosition 3 0 RightSide) == True)



mainTest :: IO ()
mainTest = do
  if testMove
     && testStep1 
     && testStep2 && testName1 && testName2 && testInvalidMove && testInvalidMove2
     && testMove2 && testMove3
  then
    putStrLn "Passed tests."
  else
    putStrLn "Failed tests."
