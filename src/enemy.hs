module Enemy where

    import ShipData
    import Shot
    import System.Random ( randomR, mkStdGen )

    -- if there are no shots adjacent to a hit ship, we pick a random square with parity of 2
    pickShot :: Board -> IO (Integer, Integer)
    pickShot (Board board) = do
        let x = 2 * fst (randomR (1,5) (mkStdGen 100))
        let y = 2 * fst (randomR (1,5) (mkStdGen 100)) 
        if getBoardElement (Board board) (x,y) /= '_' && getBoardElement (Board board) (x,y) /= 'B' then pickShot (Board board) else return (x,y)
    