module Test where
import Main


prop_moveStep :: (NonNegative Int, NonNegative Int) -> Property
prop_moveStep (NonNegative x, NonNegative y) = x<=4 && y<=3 ==> step (move initialPosition y x Right) == 1

prop_moveStepN :: (NonNegative Int, NonNegative Int, NonNegative Int) -> Property
prop_moveStepN (NonNegative x, NonNegative y, NonNegative n) = x<=4 && y<=3 ==> step (iterate (\tmp -> move tmp y x Right) initialPosition !! n) == n


testMove :: Bool
testMove =  grid (move (move initialPosition 4 0 RightSide) 4 1 LeftSide) == grid initialPosition

testStep1 :: Bool 
testStep1 = step (move (move initialPosition 4 0 RightSide) 4 1 LeftSide) == 2

testStep2 :: Bool 
testStep2 = step (move initialPosition 4 0 RightSide) == 1

testName1 :: Bool 
testName1 =  name initialPosition == "player"

testName2 :: Bool 
testName2 =  name(setName initialPosition "Drake") == "Drake"

-- reach bound
testInvalidMove :: Bool
testInvalidMove = (grid (move initialPosition 0 0 LeftSide) == grid initialPosition) && (step (move initialPosition 0 0 LeftSide) == 1)

-- no space
testInvalidMove2 :: Bool
testInvalidMove2 = (grid (move initialPosition 0 1 Down) == grid initialPosition) && (step (move initialPosition 0 1 Down) == 1)
