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

    pickShot :: IO (Integer, Integer)
    pickShot = do
        i <- randomRIO (0, (length whiteSquares)-1)
        return (whiteSquares !! i)   


    addAdjacent :: [(Integer, Integer)] -> (Integer, Integer) -> Board -> [(Integer, Integer)]
    addAdjacent shots (x,y) board = filter (validCoords board) [(x+1,y), (x-1,y), (x,y+1), (x,y-1)] ++ shots

    
    