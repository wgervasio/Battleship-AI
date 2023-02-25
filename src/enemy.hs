module Enemy where

    import ShipData
    import Shot
    import System.Random 
    import System.IO



    -- whiteSquares = [(1,1),(1,3),... (2,2),(2,4),...(3,1),(3,3),...]
    whiteSquares :: [(Integer, Integer)]
    whiteSquares = [(x,y) | x <- [1,3..10], y <- [1,3..10]] ++ [(x,y) | x <- [2,4..10], y <- [2,4..10]]
    -- if there are no shots adjacent to a hit ship, we pick a random square with parity of 2
    -- pickShot :: Board -> IO (Integer, Integer)

    pickShot :: Board -> IO (Integer, Integer)
    pickShot board = do
        i <- randomRIO (0, (length whiteSquares)-1)
        let choice = whiteSquares !! i
        print (choice) -- for testing
        if shotAlready board choice then
            pickShot board
        else
            return choice
    
    