module Shot where
    import ShipData 
    import System.Random ( randomRIO )


    -- check through the entire boat list to see if the shot hits any
    -- handles any updates needed to the boat and board 
    -- returns the boat, board, and boolean value reflecting whether the shot was a hit
    checkShot :: (Integer, Integer) -> [Boat] -> Board -> ([Boat], Board, Bool)
    checkShot _ [] board = ([], board, False)
    checkShot (x,y) boats board =
        
        if  shotAlready board (x,y) then 
            let previousState  = getBoardElement board (x,y) in
                (boats, board, if previousState == 'x' then True else False)
        else
            let (updatedBoats, hit) = checkHitBoats (x,y) boats in
                if hit then
                    (updatedBoats, (updateBoardElement board (x,y) 'x'), hit) :: ([Boat], Board, Bool)
                else
                    (boats, (updateBoardElement board (x,y) 'o'), hit) ::([Boat], Board, Bool)
            
    -- find tuple in boat that matches (x,y)
    -- if found, change bool to True
    -- return updated boat and bool if hit
    checkHitBoat :: (Integer, Integer) -> Boat -> Boat
    checkHitBoat _ (Boat [] []) = (Boat [] []) 
    checkHitBoat _ (Boat [] lst) = (Boat [] lst) 
    checkHitBoat _ (Boat lst []) = (Boat lst []) 
    checkHitBoat (xShot,yShot) (Boat ((xi,yi):xyRest) (hitI:hitRest)) = do
        if (xShot == xi) && (yShot == yi) then
            (Boat ((xi,yi):xyRest) (True:hitRest))
        else 
            let (Boat _ updatedHitRest) = checkHitBoat (xShot,yShot) (Boat xyRest hitRest)
            in Boat ((xi,yi):xyRest) (hitI:updatedHitRest)

    -- recurse through a list of boats to see if one got hit by the shot. returns the boat and a bool representing whether a boat
    -- was hit or not, modifying the boat in the list of neede
    checkHitBoats :: (Integer, Integer) -> [Boat] -> ([Boat], Bool)
    checkHitBoats _ [] = ([], False)
    checkHitBoats (xShot,yShot) (headBoat:restBoats) = 
        -- does not work if point was already hit
        let updatedBoat = checkHitBoat (xShot,yShot) headBoat in
        if headBoat /= updatedBoat then
            -- if head /= updated, we know updated was hit and can keep recursing
            (updatedBoat:restBoats, True) :: ([Boat], Bool)
        else
            -- keep recursing
            let (updatedBoats, restHit::Bool) = checkHitBoats (xShot,yShot) restBoats in
                (headBoat:updatedBoats, restHit) :: ([Boat], Bool)

    -- prompts user for row and col position of shot
    -- checks if shot is valid
    -- returns shot coordinates, otherwise prompts again
    promptShot :: IO (Integer, Integer)
    promptShot = do
        putStrLn "What column do you want to fire at?"
        x <- getLine
        putStrLn "What row do you want to fire at?"
        y <- getLine
        coords <- (,) <$> readIO x <*> readIO y
        if checkBounds coords then
            return coords
            else do
                putStrLn "Invalid coordinates. Try again."
                promptShot

    -- repeatedly prompt the player until they provide an input that doesn't hit a cell they previously fired on
    getValidShot :: Board -> IO (Integer, Integer) -> IO (Integer, Integer)
    getValidShot eboard coordLambda = do
         potentialCoords <- coordLambda
         if shotAlready eboard potentialCoords then do
            putStrLn "Target already hit. Try again."
            getValidShot eboard coordLambda
         else
            return potentialCoords 