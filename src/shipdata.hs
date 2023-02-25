{-# LANGUAGE ParallelListComp #-}
module ShipData where
    import Data.Array ( Array, (!), (//), listArray, array )
    import Data.List
    import Data.Foldable
    
    data Boat = Boat {positions :: [(Integer, Integer)], hitmap:: [Bool]}
        deriving (Show)
    instance Eq Boat where
        (==) :: Boat -> Boat -> Bool
        (Boat p1 h1) == (Boat p2 h2) = (p1 == p2) && (h1 == h2)
    
    data Board = Board (Array (Integer,Integer) Char)
        deriving (Show)



    getBoardElement (Board board) (i,j) = board ! (i,j)

    updateBoardElement (Board board) (i,j) value = Board (board // [((i,j),value)])


    updateBoardElements board positions value = foldl (\ boardNew position -> updateBoardElement boardNew position value) board positions


    emptyBoard = (Board (array ((1,1),(10,10)) [((i,j), '_') | i <- [1..10], j <- [1..10]]))


    addBoatsToBoard boats = foldl (\ boardNew boat -> 
        updateBoardElements boardNew (positions boat) 'B')
         emptyBoard
          boats
    

    -- checks if a boat is sunk
    checkSunk (Boat _ lst) = and lst


    checkBounds (x, y) = (x >= 1 && x <= 10) && (y >= 1 && y <= 10)



    printBoard :: Board -> IO ()
    printBoard (Board board) = do
        putStrLn "  1 2 3 4 5 6 7 8 9 10"
        putStrLn "  -------------------"
        mapM_ (\n -> do
            putStrLn (intersperse ' ' ((if n <= 9 then show n else show 0) ++ [getBoardElement (Board board) (i, n) | i <- [1..10]])))
            [1..10]
        
        
    printBoardEnemy :: Board -> IO ()
    printBoardEnemy board = do
        putStrLn "  1 2 3 4 5 6 7 8 9 10"
        putStrLn "  -------------------"
        mapM_ (\n -> do
            putStrLn (intersperse ' ' ((if n <= 9 then show n else show 0) ++ [if getBoardElement board (i, n) /= 'B' then getBoardElement board (i, n) else '_' | i <- [1..10]])))
            [1..10]

   


    
    

