module Shot where
    import ShipData 
    import System.Random ( randomRIO )

    -- checks if shot hit any boats
    -- records outcome on the baord
    -- returns updated boat list and board
    checkShot :: (Integer, Integer) -> [Boat] -> Board -> ([Boat], Board)
    checkShot _ [] board = ([], board)
    checkShot (x,y) boats board =
        let (updatedBoats, hit) = checkHitBoats (x,y) boats in
            if hit then
                (updatedBoats, (updateBoardElement board (x,y) 'x')) :: ([Boat], Board)
            else
                (boats, (updateBoardElement board (x,y) 'o')) ::([Boat], Board)

    -- same behaviour as checkShot
    -- but returns hit bool to adjust target space
    checkShotEnemy :: (Integer, Integer) -> [Boat] -> Board -> ([Boat], Board, Bool)
    checkShotEnemy _ [] board = ([], board, False)
    checkShotEnemy (x,y) boats board =
        let (updatedBoats, hit) = checkHitBoats (x,y) boats in
            if hit then
                (updatedBoats, (updateBoardElement board (x,y) 'x'), hit) :: ([Boat], Board, Bool)
            else
                (boats, (updateBoardElement board (x,y) 'o'), hit) ::([Boat], Board, Bool)
        

    
    checkHitBoat :: (Integer, Integer) -> Boat -> Boat
    -- find tuple in boat that matches (x,y)
    -- if found, change bool to True
    -- return updated boat and bool if hit
    checkHitBoat _ (Boat [] []) = (Boat [] []) 
    checkHitBoat _ (Boat [] lst) = (Boat [] lst) 
    checkHitBoat _ (Boat lst []) = (Boat lst []) 
    checkHitBoat (xShot,yShot) (Boat ((xi,yi):xyRest) (hitI:hitRest)) = do
        if (xShot == xi) && (yShot == yi) then
            (Boat ((xi,yi):xyRest) (True:hitRest))
        else 
            let (Boat _ updatedHitRest) = checkHitBoat (xShot,yShot) (Boat xyRest hitRest)
            in Boat ((xi,yi):xyRest) (hitI:updatedHitRest)


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
    checkHitBoats (xShot,yShot) (headBoat:restBoats) = 
        -- does not work if point was already hit
        let updatedBoat = checkHitBoat (xShot,yShot) headBoat in
        if headBoat /= updatedBoat then
            (updatedBoat:restBoats, True) :: ([Boat], Bool)
        else
            let (updatedBoats, restHit::Bool) = checkHitBoats (xShot,yShot) restBoats in
                (headBoat:updatedBoats, restHit) :: ([Boat], Bool)

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
        if checkBounds coords then
            return coords
            else do
                putStrLn "Invalid coordinates. Try again."
                promptShot


    randomShot :: IO (Integer, Integer)
    -- randomly generates a shot
    -- checks if shot is valid
    -- returns shot coordinates, otherwise tries again
    randomShot = do
        coords <- (,) <$> randomRIO (1, 10) <*> randomRIO (1, 10)
        if checkBounds coords then
            return coords
        else
            -- silently try again
            randomShot


    getValidShot :: Board -> IO (Integer, Integer) -> IO (Integer, Integer)
    getValidShot eboard coordLambda = do
         potentialCoords <- coordLambda
         if getBoardElement eboard potentialCoords /= '_' then do
            putStrLn "Target already hit. Try again."
            getValidShot eboard coordLambda
         else
            return potentialCoords 