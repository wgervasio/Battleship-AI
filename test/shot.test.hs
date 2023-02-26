{-# OPTIONS_GHC -F -pgmF htfpp #-}


module ShotTest where
    import Shot 
    import Test.Framework
    
    testCheckShot:: Assertion
    testCheckShot = do
        let boatBoard = addBoatsToBoard [Boat [(1,2), (1,3)] [False, False]]
        assertEqual (checkShot (1,2) [testBoat] boatBoard) ((updateBoardElement boatBoard (1,2) 'x'), [Boat [(1,2), (1,3)] [True, False]], True)
        assertEqual (checkShot (2,2) [testBoat] boatBoard) ((updateBoardElement boatBoard (2,2) 'o'), [testBoat], False)

    testCheckHitBoat :: Assertion
    testCheckHitBoat = do
        let b1 = Boat [(1,1),(1,2),(1,3)] [False, False, False]
        let b2 = Boat [(10,10),(9,9),(8,8)] [False, True, False]
        let b3 = Boat [(5,4),(3,2),(1,0)] [True, True, True]
        assertEqual (checkHitBoat (1,1) b1) ([(1,1),(1,2),(1,3)] [True, False, False])
        assertEqual (checkHitBoat (9,9) b2) (Boat [(10,10),(9,9),(8,8)] [False, True, False])
        assertEqual (checkHitBoat (2,2) b3) (Boat [(5,4),(3,2),(1,0)] [True, True, True])
        assertEqual (checkHitBoat (1,1) b3) (Boat [(5,4),(3,2),(1,0)] [True, True, True])
        assertEqual (checkHitBoat (1,2) b2) (Boat [(10,10),(9,9),(8,8)] [False, True, False])
        assertEqual (checkHitBoat (2,2) b1) ([(1,1),(1,2),(1,3)] [False, False, False])


    testCheckHitBoats :: Assertion
    testCheckHitBoats = do
        let b1 = Boat [(1,1),(1,2),(1,3)] [False, False, False]
        let b2 = Boat [(10,10),(9,9),(8,8)] [False, True, False]
        let b3 = Boat [(5,4),(3,2),(1,0)] [True, True, True]
        lob1 = []
        lob2 = [b1]
        lob3 = [b1, b2]
        lob4 = [b1, b2, b3]
        assertEqual (checkHitBoats (1,1) lob1) ([], False)
        assertEqual (checkHitBoats (1,1) lob2) ([Boat [(1,1),(1,2),(1,3)] [True,False,False]], True)
        assertEqual (checkHitBoats (8,8) lob3)  ([Boat [(1,1),(1,2),(1,3)] [False,False,False],Boat [(10,10),(9,9),(8,8)] [False,True,True]], True)
        assertEqual (checkHitBoats (1,0) lob4) ([Boat [(1,1),(1,2),(1,3)] [False,False,False],Boat [(10,10),(9,9),(8,8)] [False,True,False],Boat [(5,4),(3,2),(1,0)] [True,True,True]], False)
        assertEqual (checkHitBoats (1,5) lob4) ([Boat [(1,1),(1,2),(1,3)] [False,False,False],Boat [(10,10),(9,9),(8,8)] [False,True,False],Boat [(5,4),(3,2),(1,0)] [True,True,True]], False)

  