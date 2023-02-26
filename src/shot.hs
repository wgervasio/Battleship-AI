module Shot where
    import ShipData 
    import System.Random ( randomRIO )
    import Text.Read(readMaybe)


    -- check through the entire boat list to see if the shot hits any
    -- handles any updates needed to the boat and board 
    -- returns the boat, board, and boolean value reflecting whether the shot was a hit
    checkShot :: (Integer, Integer) -> [Boat] -> Board -> ([Boat], Board, Bool)
    checkShot _ [] board = ([], board, False)
    checkShot coords boats board =
        let (updatedBoats, hit) = checkHitBoats coords boats in
            if hit then
                (updatedBoats, (updateBoardElement board coords 'x'), hit)
            else
                (boats, (updateBoardElement board coords 'o'), hit)
            
    -- find tuple in boat that matches (x,y)
    -- if found, change bool to True
    -- return updated boat and bool if hit
    checkHitBoat :: (Integer, Integer) -> Boat -> Boat
    checkHitBoat _ (Boat [] []) = (Boat [] []) 
    checkHitBoat _ (Boat [] lst) = (Boat [] lst) 
    checkHitBoat _ (Boat lst []) = (Boat lst []) 
    checkHitBoat shotCoords (Boat (coords:xyRest) (hitI:hitRest)) = do
        if shotCoords == coords then
            (Boat (coords:xyRest) (True:hitRest))
        else 
<<<<<<< HEAD
            let (Boat _ updatedHitRest) = checkHitBoat shotCoords (Boat xyRest hitRest)
            in Boat (coords:xyRest) (hitI:updatedHitRest)

    -- recurse through a list of boats to see if one got hit by the shot. returns the boat and a bool representing whether a boat
    -- was hit or not, modifying the boat in the list of needed
=======
            let (Boat _ updatedHitRest) = checkHitBoat (xShot,yShot) (Boat xyRest hitRest)
            in Boat ((xi,yi):xyRest) (hitI:updatedHitRest)

    -- recurse through a list of boats to see if one got hit by the shot. returns the boat and a bool representing whether a boat
    -- was hit or not, modifying the boat in the list of neede
>>>>>>> main
    checkHitBoats :: (Integer, Integer) -> [Boat] -> ([Boat], Bool)
    checkHitBoats _ [] = ([], False)
    checkHitBoats shotCoords (headBoat:restBoats) = 
        -- does not work if point was already hit
        let updatedBoat = checkHitBoat shotCoords headBoat in
        if headBoat /= updatedBoat then
<<<<<<< HEAD
            (updatedBoat:restBoats, True)
        else
            let (updatedBoats, restHit) = checkHitBoats shotCoords restBoats in
                (headBoat:updatedBoats, restHit)
=======
            -- if head /= updated, we know updated was hit and can keep recursing
            (updatedBoat:restBoats, True) :: ([Boat], Bool)
        else
            -- keep recursing
            let (updatedBoats, restHit::Bool) = checkHitBoats (xShot,yShot) restBoats in
                (headBoat:updatedBoats, restHit) :: ([Boat], Bool)
>>>>>>> main

    -- prompts user for row and col position of shot
    -- checks if shot is valid
    -- returns shot coordinates, otherwise prompts again
    promptShot :: IO (Integer, Integer)
    promptShot = do
        putStrLn "What column do you want to fire at?"
        x <- getLine
        putStrLn "What row do you want to fire at?"
        y <- getLine
        case (readMaybe x :: Maybe Integer, readMaybe y :: Maybe Integer) of
            (Just col, Just row) -> return (col, row)
            _ -> do
                putStrLn "Invalid input. Please enter numbers for column and row."
                promptShot

    validateInput :: Monad m => a1 -> (a1 -> Bool) -> m a1 -> m a2 -> m a1
    validateInput coords pred failFunc errMsg = do
        if pred coords then
            return coords
        else do
            errMsg
            failFunc

    validCoords :: Board -> (Integer, Integer) -> Bool
    validCoords board coords =  (checkBounds coords && (not (shotAlready board coords)))

    -- repeatedly prompt the player until they provide an input that doesn't hit a cell they previously fired on
    getValidShot :: Board -> IO (Integer, Integer) -> IO (Integer, Integer)
    getValidShot board coordLambda = do
        potentialCoords <- coordLambda
        putStrLn("Firing at " ++ show potentialCoords)
        validateInput potentialCoords 
            (validCoords board)
            (getValidShot board coordLambda)
            (putStrLn "Target invalid. Try again.")