{-# OPTIONS_GHC -F -pgmF htfpp #-}


module ShipDataTest where
    import ShipData 
    import Test.Framework

    testBoard = (Board (array ((1,1),(2,2) [((i,j), '_') | i <- [1..2], j <- [1..2]])))

    test_EmptyBoard :: Assertion
    -- test properly creates an empty board
    testEmptyBoard = assertEquals emptyBoard (Board (array ((1,1),(10,10)) [((i,j), '_') | i <- [1..10], j <- [1..10]]))

    test_GetBoardElement :: Assertion
    testGetBoardElement = assertEquals (getBoardElement testBoard (1,1)) "_"
        let testBoard2 = (Board (array ((1,1),(2,2) [((i,j), 'X') | i <- [1..2], j <- [1..2]])))
        assertEquals (getBoardElement testBoard2 (1,1)) "X"
    
    test_UpdateBoardElement :: Assertion
    testUpdateBoardElement = assertEquals (getBoardElement testBoard (1,1)) "_"
        let testBoard2 = updateBoardElements testBoard (1,1) 'X'
        assertEquals (getBoardElement testBoard2 (1,1)) "X"

    test_AddBoatsToBoard :: Assertion
    testAddBoatsToBoard = assertEquals 1 1

    test_CheckSunk :: Assertion
    testCheckSunk = assertEquals (checkSunk (Boat [] [False, False, True])) False
        assertEquals (checkSunk (Boat [] [False, False, False])) True
        assertEquals (checkSunk (Boat [] [True, True, True])) False
    
    test_CheckBounds :: Assertion
    testCheckBounds = assertEquals (checkBounds (1,1)) True
        assertEquals (checkBounds (11, 1)) False
        assertEquals (checkBounds (0, 1)) False
        assertEquals (checkBounds (1, 11)) False
        assertEquals (checkBounds (1, 0)) False