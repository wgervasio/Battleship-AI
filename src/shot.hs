module Shot where
    import ShipData ( updateBoardElement, Board, Boat(Boat) )
    import System.Random ( randomRIO )
    import ShipData (checkBounds)
    
    checkShot :: (Integer, Integer) -> [Boat] -> Board -> ([Boat], Board)
    -- checks if shot hit any boats
    -- records outcome on the baord
    -- returns updated boat list and board
    checkShot (x,y) boats board = do 
        (updatedBoats, hit) <- checkHitBoats (x,y) boats
        if hit then
            return (updatedBoats, updateBoardElement (x,y) board 'x')
        else
            return (boats, (updateBoardElement (x,y) board 'o'))
        

        -- for the board
        -- find tuple in board that matches (x,y)
        -- if found, change char to 'x' if boat is hit
        -- otherwise change char to 'o'
    
    
    
    checkHitBoats :: (Integer, Integer) -> [Boat] -> ([Boat], Bool)
    checkHitBoats _ [] = ([], False)
    checkHitBoats (xShot,yShot) (headBoat:restBoats) = do
        updatedBoat <- checkHitBoat (xShot,yShot) headBoat
        if headBoat /= updatedBoat then
            return (updatedBoat:restBoats, True)
        else do
            (updatedBoats, restHit) <- checkHitBoats (xShot,yShot) restBoats
            return (headBoat:updatedBoats, restHit)

    checkHitBoat :: (Integer, Integer) -> Boat -> Boat
    checkHitBoat _ (Boat [] []) = (Boat [] []) 
    -- find tuple in boat that matches (x,y)
    -- if found, change bool to True
    -- return updated boat and bool if hit
    checkHitBoat (xShot,yShot) (Boat ((xi,yi):xyRest) (hitI:hitRest)) = do
        if (xShot == xi) && (yShot == yi) then
            return (Boat ((xi,yi):xyRest) (True:hitRest))
        else 
            let (Boat xyRest hitRest) = checkHitBoat (xShot,yShot) (Boat xyRest hitRest)
            in Boat ((xi,yi):xyRest) (hitI:hitRest)


    promptShot :: IO (Integer, Integer)
    -- prompts user for row and col position of shot
    -- checks if shot is valid
    -- returns shot coordinates, otherwise prompts again
    promptShot = do
        putStrLn "What column do you want to fire at?"
        x <- getLine
        putStrLn "What row do you want to fire at?"
        y <- getLine
        coords <- (,) <$> read x <*> read y
        if checkBounds coords
            then return coords
            else do
                putStrLn "Invalid coordinates. Try again."
                promptShot


    randomShot :: (Integer, Integer)
    -- randomly generates a shot
    -- checks if shot is valid
    -- returns shot coordinates, otherwise tries again
    randomShot = do
        coords <- (,) <$> randomRIO (1, 10) <*> randomRIO (1, 10)
        if checkBounds coords then
            coords
        else
            -- silently try again
            randomShot