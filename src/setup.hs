module Setup where

    import ShipData

    createBoard = [['a']]

    createPlayerBoard :: [Boat] -> Board
    createPlayerBoard lst = Board [['a']]