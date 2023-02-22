module Shot where
    import ShipData
    import Battleship

    checkShot :: (Integer, Integer) -> [Boat] -> Board -> ([Boat], Board)
    checkShot (x,y) boats board = do 
        (updatedBoats, hit) <- checkHitBoats (x,y) boats
        if hit then
            return (updatedBoats, updateBoard (x,y) board)
        else
            return (updatedBoats, board)
        

        -- for the board
        -- find tuple in board that matches (x,y)
        -- if found, change char to 'x' if boat is hit
        -- otherwise change char to 'o'
        
        ([Boat [(0,0)] [False]], Board [['a']])

    checkHitBoats :: (Integer, Integer) -> [Boat] -> ([Boat], Bool)
    checkHitBoats _ [] = ([], False)
    checkHitBoats (xShot,yShot) (headBoat:restBoats) = do
        (updatedBoat, firstHit) <- checkHitBoat (xShot,yShot) headBoat
        if firstHit then
            return (updatedBoat:restBoats, True)
        else do
            (updatedBoats, restHit) <- checkHitBoats (xShot,yShot) restBoats
            return (updatedBoat:updatedBoats, restHit)

    checkHitBoat _ (Boat [] []) = (Boat [] [], False) 
    -- find tuple in boat that matches (x,y)
    -- if found, change bool to True
    -- return updated boat and bool if hit
    checkHitBoat (xShot,yShot) (Boat ((xi,yi):xyRest) (hitI:hitRest)) = do
        if (xShot == xi) && (yShot == yi) then
            return (Boat ((xi,yi):xyRest) (True:hitRest), True)
        else do
            (Boat xyRes, found) <- checkHitBoat (xShot,yShot) (Boat xyRest hitRest)
            return (Boat ((xi,yi):xyRes), found)


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