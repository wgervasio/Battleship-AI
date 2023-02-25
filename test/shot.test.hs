{-# OPTIONS_GHC -F -pgmF htfpp #-}


module ShotTest where
    import Shot 
    import Test.Framework

    testBoard = emptyBoard
    testBoat= Boat [(1,2), (1,3)] [False, False]
    testBoat2= Boat [(2,2), (2,3)] [False, False]

    testCheckShot :: Assertion
    testCheckShot = do
        assertEqual (checkShot (1,2) [testBoat] testBoard) ((updateBoardElement testBoard (1,2) 'x'), Boat [(1,2), (1,3)] [True, False])
        assertEqual (checkShot (2,2) [testBoat] testBoard) (testBoard, testBoat)

    testCheckShotEnemy :: Assertion
    testCheckShotEnemy = do
        assertEqual (checkShot (1,2) [testBoat] testBoard) ((updateBoardElement testBoard (1,2) 'x'), Boat [(1,2), (1,3)] [True, False], True)
        assertEqual (checkShot (2,2) [testBoat] testBoard) (testBoard, testBoat, False)

    testCheckHitBoat :: Assertion
    testCheckHitBoat = do
        assertEqual (checkHitBoat (1,2) testBoat) (Boat [(1,2), (1,3)] [True, False])
        assertEqual (checkHitBoat (1,3) testBoat) (Boat [(1,2), (1,3)] [False, True])
        assertEqual (checkHitBoat (2,2) testBoat) (Boat [(1,2), (1,3)] [False, False])

    testCheckHitBoats :: Assertion
    testCheckHitBoats = do
        assertEqual (checkHitBoats (1,2) [testBoat, testBoat2]) [Boat [(1,2), (1,3)] [True, False], Boat [(2,2), (2,3)] [False, False]]
        assertEqual (checkHitBoats (2,2) [testBoat, testBoat2]) [Boat [(1,2), (1,3)] [False, False], Boat [(2,2), (2,3)] [True, False]]
        assertEqual (checkHitBoats (3,2) [testBoat, testBoat2]) [Boat [(1,2), (1,3)] [False, False], Boat [(2,2), (2,3)] [False, False]]

  