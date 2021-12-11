import Logic
import Test.QuickCheck (NonNegative (NonNegative), Property, (==>), quickCheck)


prop_moveStep :: (NonNegative Int, NonNegative Int) -> Property
prop_moveStep (NonNegative x, NonNegative y) = x<=4 && y<=3 ==> step (move defaultLevel y x Logic.Right) == 1

prop_moveStepN :: (NonNegative Int, NonNegative Int, NonNegative Int) -> Property
prop_moveStepN (NonNegative x, NonNegative y, NonNegative n) = x<=4 && y<=3 ==> step (iterate (\tmp -> move tmp y x Logic.Right) defaultLevel !! n) == n


testMove :: Bool
testMove =  grid (move (move defaultLevel 0 4 Logic.Right) 1 4 Logic.Left) == grid defaultLevel

testStep1 :: Bool 
testStep1 = step (move (move defaultLevel 0 4 Logic.Right) 1 4 Logic.Left) == 2

testStep2 :: Bool 
testStep2 = step (move defaultLevel 0 4 Logic.Right) == 1

testName1 :: Bool 
testName1 =  name defaultLevel == "player"

testName2 :: Bool 
testName2 =  name(setName defaultLevel "Drake") == "Drake"

-- reach bound
testInvalidMove :: Bool
testInvalidMove = (grid (move defaultLevel 0 0 Logic.Left) == grid defaultLevel) && (step (move defaultLevel 0 0 Logic.Left) == 1)

-- no space
testInvalidMove2 :: Bool
testInvalidMove2 = (grid (move defaultLevel 0 1 Down) == grid defaultLevel) && (step (move defaultLevel 0 1 Down) == 1)

main :: IO ()
main = do
    quickCheck prop_moveStep
    quickCheck prop_moveStepN
    quickCheck testMove
    quickCheck testStep1
    quickCheck testStep2
    quickCheck testInvalidMove
    quickCheck testInvalidMove2
    putStrLn "done!"