module Shot where
    import ShipData

    checkShot :: (Integer, Integer) -> [Boat] -> Board -> ([Boat], Board)
    checkShot (x,y) boats board = ([Boat [(0,0)] [False]], Board [['a']])

    promptShot = (0, 0)

    randomShot = (0,0)