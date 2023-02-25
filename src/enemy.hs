module Enemy where

    import ShipData
    import Shot
    import System.Random ( randomR, mkStdGen )

    -- if there are no shots adjacent to a hit ship, we pick a random square with parity of 2
    pickShot :: Board -> IO (Integer, Integer)
    pickShot board = do
        -- this endlessly loops actually. Always picks (8,8)
        let x = 2 * fst (randomR (1,5) (mkStdGen 100))
        let y = 2 * fst (randomR (1,5) (mkStdGen 100)) 
        print (x,y)
        if getBoardElement board (x,y) `elem` ['o','x'] then 
            pickShot board 
        else
             return (x,y)
    