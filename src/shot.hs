module Shot where
    import ShipData
    import Battleship

    checkShot :: (Integer, Integer) -> [Boat] -> Board -> ([Boat], Board)
    checkShot (x,y) boats board = ([Boat [(0,0)] [False]], Board [['a']])


    promptShot :: IO (Integer, Integer)
    -- prompts user for row and col position of shot
    -- checks if shot is valid
    -- returns shot coordinates, otherwise prompts again
    promptShot = do
        putStrLn "What column do you want to fire at?"
        x <- getLine
        putStrLn "What row do you want to fire at?"
        y <- getLine
        coords <- ((read x :: Integer), (read y :: Integer))
        if checkBounds coords then
            return coords
        else
            putStrLn "Invalid coordinates. Try again."
            promptShot


    randomShot :: (Integer, Integer)
    -- randomly generates a shot
    -- checks if shot is valid
    -- returns shot coordinates, otherwise tries again
    randomShot = do
        coords <- (randomRIO(1, 10), randomRIO(1, 10))
        if checkBounds coords then
            return coords
        else
            -- silently try again
            randomShot