{-# OPTIONS_GHC -F -pgmF htfpp #-}

module EnemyTest where
    import Enemy 
    import Test.Framework

    -- only one square left
    testBoard1 = (Board (array ((1,1),(10,10)) [((i,j), 'X') | i <- [1..10], j <- [1..10]]))
    updatedBoard1 = testBoard1 // [((10, 10), '_')]
    
    -- two squares left, only one with parity 2
    testBoard2 = (Board (array ((1,1),(10,10)) [((i,j), 'X') | i <- [1..10], j <- [1..10]]))
    updatedBoard2_1 = testBoard2 // [((4, 4), '_')]
    updatedBoard2_2 = testBoard2_1 // [((9, 9), '_')]

    test_PickShotOneLeft :: Assertion
    testPickShotOneLeft = assertEquals getBoardElement (pickShot updatedBoard1) "_"
        assertEquals (pickShot updatedBoard1) (10,10)

    test_PickShotTwoLeft :: Assertion
    testPickShotTwoLeft = assertEquals getBoardElement (pickShot updatedBoard2_2) "_"
        assertEquals (pickShot updatedBoard2_2) (4,4)