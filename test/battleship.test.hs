{-# OPTIONS_GHC -F -pgmF htfpp #-}

module BattleShipTest where
    import BattleShip
    import Test.Framework

    main = htfMain htf_thisModulesTests
    
    test_GameWon :: Assertion
    testGameWon = do
        let b1 = Boat [] [False, False, False]
        let b2 = Boat [] [False, True, False]
        let b3 = Boat [] [True, True, True]
        assertEquals (gameWon [b1, b1]) False
        assertEquals (gameWon [b1, b2]) False
        assertEquals (gameWon [b1, b2, b3]) False
        assertEquals (gameWon [b3, b3, b3]) True

    test_CheckOverlaps :: Assertion
    testCheckOverlaps = do
        let b0 = Boat [] []
        let  b1 = Boat [(1,1), (1,2), (1,3)] []
        let b2 = Boat [(1,1), (1,2), (1,3)] []
        let b3 = Boat [(1,1), (4,4), (5,5)] [] 
        let b4 = Boat [(2,2), (4,4), (7,7)] [] 
        let b5 = Boat [(3,3), (5,5), (7,7)] []
        assertEquals (checkOverlaps b1 b2) True
        assertEquals (checkOverlaps b2 b3) True
        assertEquals (checkOverlaps b3 b4) True
        assertEquals (checkOverlaps b4 b5) True
        assertEquals (checkOverlaps b5 b1) False
        assertEquals (checkOverlaps b4 b2) False
        assertEquals (checkOverlaps b0 b1) False
    
    test_CheckOverlapsList :: Assertion
    testCheckOverlapsList = do 
        let b0 = Boat [] []
        let b1 = Boat [(1,1), (1,2), (1,3)] []
        let b2 = Boat [(1,1), (1,2), (1,3)] []
        let b3 = Boat [(1,1), (4,4), (5,5)] [] 
        let b4 = Boat [(2,2), (4,4), (7,7)] [] 
        let b5 = Boat [(3,3), (5,5), (7,7)] []
        let b6 = Boat [(10,10), (9,9)] []
        let lob1 = []
        let lob2 = [b2, b3, b4, b5]
        let lob3 = [b3]
        let lob4 = [b1, b5]
        assertEquals (checkOverlapsList b1 lob1) False
        assertEquals (checkOverlapsList b1 lob2) True
        assertEquals (checkOverlapsList b1 lob3) True
        assertEquals (checkOverlapsList b0 lob4) False
        assertEquals (checkOverlapsLIst b6 lob4) False

    test_PlaceBoatRandom :: Assertion
    testPlaceBoatRandom = do
        let list = placeBoatRandom([], 4)
        let boat = head list
        assertEquals (length $ positions boat) 4
        assertEquals (length $ hitmap boat) 4
        let list = placeBoatRandom([], 2)
        let boat = head list
        assertEquals (length $ positions boat) 2
        assertEquals (length $ hitmap boat) 2
    
    test_GrabLengths :: Assertion
    testGrabLengths = do
        let list1 = grabLengths(2, 4, 4, "H", "L")
        let list2 = grabLengths(2, 4, 4, "H", "R")
        let list3 = grabLengths(2, 4, 4, "V", "U")
        let list4 = grabLengths(2, 4, 4, "V", "D")

        assertEquals (list1) [(4,4), (5,4), (6,4)]
        assertEquals (list2) [(4,4), (3,4), (2,4)]
        assertEquals (list3) [(4,4), (4,3), (4,2)]
        assertEquals (list4) [(4,4), (4,5), (4,6)]

    test_AddAdjacent :: Assertion
    testAddAdjacent = do
        let testBoard = emptyBoard
        assertEquals (addAdjacent [] (5,5) testBoard) [(6,5),(4,5),(5,6),(5,4)]
        assertEquals (addAdjacent [] (10,10) testBoard) [(9,10),(10,9)]
        assertEquals (addAdjacent [] (1,1) testBoard) [(2,1),(1,2)]
        assertEquals (addAdjacent [] (10,1) testBoard) [(9,1),(10,2)]
        assertEquals (addAdjacent [] (1,10) testBoard) [(2,10),(1,9)]
        assertEquals (addAdjacent [(6,5)] (5,5) testBoard) [(6,5),(4,5),(5,6),(5,4)]
        
        let testBoard = updateBoardElements testBoard (5,6) 'o'
        let testBoard = updateBoardElements testBoard (5,4) 'x'
        assertEquals (addAdjacent [] (5,5) testBoard) [(6,5),(4,5)]