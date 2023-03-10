module Enemy where

    import ShipData
    import Shot
    import System.Random 
    import System.IO
    


    -- whiteSquares = [(1,1),(1,3),... (2,2),(2,4),...(3,1),(3,3),...]
    -- creates a list of all squares with parity 2, similar to the white squares on a chessboard
    whiteSquares :: [(Integer, Integer)]
    whiteSquares = [(x,y) | x <- [1,3..10], y <- [1,3..10]] ++ [(x,y) | x <- [2,4..10], y <- [2,4..10]]
    -- if there are no shots adjacent to a hit ship, we pick a random square with parity of 2

    pickShot :: IO (Integer, Integer)
    pickShot = do
        i <- randomRIO (0, (length whiteSquares)-1)
        return (whiteSquares !! i)   


    -- function to create a target list for enemy.
    -- once a boat is hit successfully, this is called to create a list of all adjacent spaces
    -- filtering spaces that are out of bounds, already shot, or already in the list of targets
    addAdjacent :: [(Integer, Integer)] -> (Integer, Integer) -> Board -> [(Integer, Integer)]
    addAdjacent shots (x,y) board = filter (validCoords board) [(x+1,y), (x-1,y), (x,y+1), (x,y-1)] ++ shots

    
    -- Given a list of guessed coordinates and a previous hit coordinate,
    -- pick a new coordinate to guess that is adjacent to the previous hit.
    -- If there are no adjacent coordinates available, pick a random coordinate.
    pickAdjacentCoordinate :: [(Integer, Integer)] -> (Integer, Integer) -> Board -> IO (Integer,Integer)
    pickAdjacentCoordinate shots (x, y) board = do
        let adjacentCoords = filter (\(x', y') -> abs (x' - x) + abs (y' - y) == 1) [(x, y) | x <- [1..10], y <- [1..10]]
        let adjacentFiltered = filter (validCoords board) adjacentCoords
        let availableCoords = filter (\coord -> not $ elem coord shots) adjacentFiltered
        if null availableCoords
            then pickShot >>= return
            else return $ head availableCoords
