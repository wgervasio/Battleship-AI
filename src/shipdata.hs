

module ShipData where
    import Data.Array ( Array, (!), (//), listArray, array )

    data Boat = Boat {positions :: [(Integer, Integer)], hitmap:: [Bool]}
        deriving (Show)
    instance Eq Boat where
        (==) :: Boat -> Boat -> Bool
        (Boat p1 h1) == (Boat p2 h2) = (p1 == p2) && (h1 == h2)
    
    data Board = Board (Array (Integer,Integer) Char)
        deriving (Show)
    instance Eq Board where
        (==) :: Board -> Board -> Bool
        (Board b1) == (Board b2) = (b1 == b2)

    getBoardElement :: Board -> (Integer, Integer) -> Char
    getBoardElement (Board board) (i,j) = board ! (i,j)

    updateBoardElement :: Board -> (Integer, Integer) -> Char -> Board
    updateBoardElement (Board board) (i,j) value = Board (board // [((i,j),value)])

    updateBoardElements :: Foldable t => Board -> t (Integer, Integer) -> Char -> Board
    updateBoardElements (Board board) positions value = foldl (\ boardNew position -> updateBoardElement boardNew position value) (Board board) positions

    emptyBoard :: Board
    emptyBoard = (Board (array ((1,1),(10,10)) [((i,j), ' ') | i <- [1..10], j <- [1..10]]))

    addBoatsToBoard :: [Boat] -> Board
    addBoatsToBoard boats = foldl (\ boardNew boat -> 
        updateBoardElements boardNew (positions boat) 'B')
         emptyBoard
          boats

