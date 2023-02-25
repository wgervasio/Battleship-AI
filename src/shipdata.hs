{-# LANGUAGE ParallelListComp #-}
module ShipData where
    import Data.Array ( Array, (!), (//), listArray, array )
    import Data.List
    import Data.Foldable
    import qualified GHC.TypeLits as cheat
    
    -- a data type representing a boat. each boat has an array of (Integer, Integer) pairs representing each cell it occupies on the board
    -- and a list of corresponding booleans representing if the given cell has been hit.
    data Boat = Boat {positions :: [(Integer, Integer)], hitmap:: [Bool]}
        deriving (Show)
    instance Eq Boat where
        (==) :: Boat -> Boat -> Bool
        (Boat p1 h1) == (Boat p2 h2) = (p1 == p2) && (h1 == h2)
    
    -- a 2D array of chars with (Integer, Integer) coordinates representing the state of each player's board. '_' indicates empty ocean, 
    -- 'B' indicates a cell of a ship, 'x' indicates a previous hit and 'o' indicates a previous miss.
    data Board = Board (Array (Integer,Integer) Char)
        deriving (Show)


    -- return the char of the Board at coordinates (i,j)
    getBoardElement (Board board) (i,j) = board ! (i,j)

    -- replace the element of the Board at (i,j) with value
    updateBoardElement (Board board) (i,j) value = Board (board // [((i,j),value)])

    -- replace every position in the corresponding to a position in positions with value
    updateBoardElements board positions value = foldl (\ boardNew position -> updateBoardElement boardNew position value) board positions

    -- create a 10 x 10 board where every space is occupied by '_', symbolizing empty ocean
    emptyBoard = (Board (array ((1,1),(10,10)) [((i,j), '_') | i <- [1..10], j <- [1..10]]))

    -- given a list of boats, take each boat's positions and update every matching cell of the board to a 'B' to visually
    -- represent the positions of each boat
    addBoatsToBoard boats = foldl (\ boardNew boat -> 
        updateBoardElements boardNew (positions boat) 'B')
         emptyBoard
          boats
    

    -- checks if a boat is sunk
    checkSunk (Boat _ lst) = and lst


    -- ensures a coordinate is within the bounds of the board, (1,1) to (10,10)
    checkBounds (x, y) = (x >= 1 && x <= 10) && (y >= 1 && y <= 10)



    -- map a function to compose a string of every row of the board, interspersed with " ", and then print each row
    printBoard :: Board -> IO ()
    printBoard board = do
        putStrLn "  PLAYER BOARD"
        putStrLn "  1 2 3 4 5 6 7 8 9 10"
        putStrLn "  -------------------"
        mapM_ (\n -> do
            putStrLn (intersperse ' ' ((if n <= 9 then show n else show 0) ++ [getBoardElement board (i, n) | i <- [1..10]])))
            [1..10]
        
        
    -- same behaviour as printBoard, but censor out the Bs on the enemy board so the player can't cheat
    printBoardEnemy :: Board -> IO ()
    printBoardEnemy  board = do
        putStrLn "  ENEMY BOARD"
        putStrLn "  1 2 3 4 5 6 7 8 9 10"
        putStrLn "  -------------------"
        mapM_ (\n -> do
            putStrLn (intersperse ' ' ((if n <= 9 then show n else show 0) ++ [if getBoardElement  board (i, n) /= 'B' then getBoardElement  board (i, n) else '_'  | i <- [1..10]])))
            [1..10]
    
    -- returns whether a square was previous shot by the player or enemy(square is x for hit, or o for miss)
    shotAlready :: Board -> (Integer, Integer) -> Bool
    shotAlready board coords = (getBoardElement board coords) `elem` ['o','x']
 


    
    

