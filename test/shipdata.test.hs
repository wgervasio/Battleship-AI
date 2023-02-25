{-# OPTIONS_GHC -F -pgmF htfpp #-}


module ShipDataTest where
    import ShipData 
    import Test.Framework

    testBoard = (Board (array ((1,1),(2,2) [((i,j), '_') | i <- [1..2], j <- [1..2]])))

    testEmptyBoard :: Assertion
    testEmptyBoard = assertEquals emptyBoard (Board (array ((1,1),(10,10)) [((i,j), '_') | i <- [1..10], j <- [1..10]]))

    testGetBoardElement :: Assertion
    testGetBoardElement = assertEquals (getBoardElement testBoard (1,1)) "_"
        let testBoard2 = (Board (array ((1,1),(2,2) [((i,j), 'X') | i <- [1..2], j <- [1..2]])))
        assertEquals (getBoardElement testBoard2 (1,1)) "X"
    
    testUpdateBoardElement :: Assertion
    testUpdateBoardElement = assertEquals (getBoardElement testBoard (1,1)) "_"
        let testBoard2 = updateBoardElements testBoard (1,1) 'X'
        assertEquals (getBoardElement testBoard2 (1,1)) "X"

    testAddBoatsToBoard :: Assertion
    testAddBoatsToBoard = assertEquals 1 1

    testCheckSunk :: Assertion
    testCheckSunk = assertEquals (checkSunk (Boat [] [False, False, True])) False
        assertEquals (checkSunk (Boat [] [False, False, False])) True
        assertEquals (checkSunk (Boat [] [True, True, True])) False
    
    testCheckBounds :: Assertion
    testCheckBounds = assertEquals (checkBounds (1,1)) True
        assertEquals (checkBounds (11, 1)) False
        assertEquals (checkBounds (0, 1)) False
        assertEquals (checkBounds (1, 11)) False
        assertEquals (checkBounds (1, 0)) False