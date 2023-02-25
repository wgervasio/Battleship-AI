module Enemy where

    import ShipData
    import Shot
    import System.Random 
    import System.IO




    -- if there are no shots adjacent to a hit ship, we pick a random square with parity of 2
    -- pickShot :: Board -> IO (Integer, Integer)
    pickShot board = do
        (x,y) <- (,) <$> randomRIO (1, 5) <*> randomRIO (1, 5)
        print (x,y) -- for testing
        return (x*2,y*2)
    
    