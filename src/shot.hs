module Shot where
    import ShipData 
    import System.Random ( randomRIO )


    -- same behaviour as checkShot
    -- but returns hit bool to adjust target space
    checkShot :: (Integer, Integer) -> [Boat] -> Board -> ([Boat], Board, Bool)
    checkShot _ [] board = ([], board, False)
    checkShot coords boats board =
        let (updatedBoats, hit) = checkHitBoats coords boats in
            if hit then
                (updatedBoats, (updateBoardElement board coords 'x'), hit)
            else
                (boats, (updateBoardElement board coords 'o'), hit)
            

    
    checkHitBoat :: (Integer, Integer) -> Boat -> Boat
    -- find tuple in boat that matches (x,y)
    -- if found, change bool to True
    -- return updated boat and bool if hit
    checkHitBoat _ (Boat [] []) = (Boat [] []) 
    checkHitBoat _ (Boat [] lst) = (Boat [] lst) 
    checkHitBoat _ (Boat lst []) = (Boat lst []) 
    checkHitBoat shotCoords (Boat (coords:xyRest) (hitI:hitRest)) = do
        if shotCoords == coords then
            (Boat (coords:xyRest) (True:hitRest))
        else 
            let (Boat _ updatedHitRest) = checkHitBoat shotCoords (Boat xyRest hitRest)
            in Boat (coords:xyRest) (hitI:updatedHitRest)


    -- b1 = Boat [(1,1),(1,2),(1,3)] [False, False, False]
    -- b2 = Boat [(10,10),(9,9),(8,8)] [False, True, False]
    -- b3 = Boat [(5,4),(3,2),(1,0)] [True, True, True]
    -- checkHitBoat (1,1) b1 -> true
    -- checkHitBoat (9,9) b2 -> true
    -- checkHitBoat (1,0) b3 -> true
    -- checkHitBoat (1,1) b3 -> false
    -- checkHitBoat (1,2) b2 -> false

    checkHitBoats :: (Integer, Integer) -> [Boat] -> ([Boat], Bool)
    checkHitBoats _ [] = ([], False)
    checkHitBoats shotCoords (headBoat:restBoats) = 
        -- does not work if point was already hit
        let updatedBoat = checkHitBoat shotCoords headBoat in
        if headBoat /= updatedBoat then
            (updatedBoat:restBoats, True)
        else
            let (updatedBoats, restHit) = checkHitBoats shotCoords restBoats in
                (headBoat:updatedBoats, restHit)

    -- lob1 = []
    -- lob2 = [b1]
    -- lob3 = [b1, b2]
    -- lob4 = [b1, b2, b3]


    -- checkHitBoats (1,1) lob1 -> ([], False)
    -- checkHitBoats (1,1) lob2 -> ([Boat [(1,1),(1,2),(1,3)] [True,False,False]], True)
    -- checkHitBoats (8,8) lob3 -> ([Boat [(1,1),(1,2),(1,3)] [False,False,False],Boat [(10,10),(9,9),(8,8)] [False,True,True]], True)
    -- could be an error if not handled:
    -- checkHitBoats (1,0) lob4 -> ([Boat [(1,1),(1,2),(1,3)] [False,False,False],Boat [(10,10),(9,9),(8,8)] [False,True,False],Boat [(5,4),(3,2),(1,0)] [True,True,True]], False)
    -- checkHitBoats (1,5) lob4 -> ([Boat [(1,1),(1,2),(1,3)] [False,False,False],Boat [(10,10),(9,9),(8,8)] [False,True,False],Boat [(5,4),(3,2),(1,0)] [True,True,True]], False)

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
        return coords
        
    validateInput coords pred failFunc errMsg = do
        if pred coords then
            return coords
        else do
            errMsg
            failFunc

    validCoords board coords =  (not (shotAlready board coords)) && checkBounds coords

    getValidShot :: Board -> IO (Integer, Integer) -> IO (Integer, Integer)
    getValidShot board coordLambda = do
        potentialCoords <- coordLambda
        validateInput potentialCoords 
            (validCoords board)
            (getValidShot board coordLambda)
            (putStrLn "Target already hit. Try again.")