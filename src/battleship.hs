-- Battleship in Haskell
{-# LANGUAGE BlockArguments #-}
module BattleShip where
{-
data State = State InternalState [Action]  -- internal_state available_actions
         deriving (Ord, Eq, Show)

data Result = EndOfGame Double State    -- end of game: value, starting state
            | ContinueGame State        -- continue with new state
         deriving (Eq, Show)

type Game = Action -> State -> Result

type Player = State -> Action
-}

import ShipData
import Shot
import Setup
import Enemy
import System.Random
import System.IO


-- places all 5 boats on the board
-- places the enemy boats randomly
-- plays the first turn for the player

startGame=do
   enemyList <- foldl (\acc x -> placeBoatRandom x acc) (return []) [2,3,3,4,5]
   playerList <- foldl (\acc x -> placeBoatRandom x acc) (return []) [2,3,3,4,5]

   
   -- place your 5 boats
   
   -- call player turn
   let pboard =  addBoatsToBoard playerList
   let eboard = addBoatsToBoard enemyList
   playerTurn playerList enemyList pboard eboard []
   


-- prompts user for shot
-- checks if shot is valid
-- checks if shot hits a boat
-- checks if all enemy boats are sunk
   
playerTurn :: [Boat] -> [Boat] -> Board -> Board -> [(Integer, Integer)]-> IO ()
playerTurn plist elist pboard eboard targets = do
--    -- print enemy board
--    -- TODO change this
   printBoard(eboard)

   
   coords <- getValidShot eboard promptShot

   let (newElist, newEboard) = checkShot coords elist eboard

   -- print enemy board after shot
   printBoard(newEboard)


   if gameWon newElist then
      putStrLn "You won!"
   else
      enemyTurn plist newElist pboard newEboard targets



enemyTurn :: [Boat] -> [Boat] -> Board -> Board -> [(Integer, Integer)]-> IO ()
enemyTurn plist elist pboard eboard [] = do
   -- print player board
   -- TODO: fix this
   printBoard(pboard)

   coords <- pickShot pboard

   -- putStrLn "The enemy shoots at row " ++ show x ++ ", column " ++ show y
   let (newPlist, newPboard, hit) = checkShotEnemy coords plist pboard 
   putStrLn "Enemy fired!"
   let newTargets = if hit then addAdjacent [] coords else []
   if gameWon newPlist then
      putStrLn "Enemy won!"
   else
      playerTurn newPlist elist newPboard eboard newTargets

enemyTurn plist elist pboard eboard (target:targets) = do
   -- print player board
   -- TODO: fix this
   printBoard(pboard)

   coords <- pure target

   -- putStrLn "The enemy shoots at row " ++ show x ++ ", column " ++ show y
   let (newPlist, newPboard, hit) = checkShotEnemy coords plist pboard 
   let newTargets = if hit then addAdjacent targets coords else targets
   if gameWon newPlist then
      putStrLn "Enemy won!"
   else
         playerTurn newPlist elist newPboard eboard newTargets
   



-- checks if all boats are sunk in lst
gameWon :: [Boat] -> Bool
gameWon lst = and (map checkSunk lst)



checkAligned :: (Eq a, Num a) => (a, a) -> (a, a) -> a -> Bool
checkAligned (x,y) (w,z) num = if (x /= w) && (y /= z) then False else (abs(x - w) == num) || (abs(y -z ) == num)


addAdjacent :: [(Integer, Integer)] -> (Integer, Integer) -> [(Integer, Integer)]
addAdjacent shots (x,y) = (filter checkBounds [(x+1,y), (x-1,y), (x,y+1), (x,y-1)])


checkOverlaps :: Boat -> Boat -> Bool
-- check if two boats are overlapped
checkOverlaps b1 b2 =
   any (`elem` b2coords) b1coords
      where
         b1coords = positions b1 
         b2coords = positions b2

-- b0 = Boat [] []
-- b1 = Boat [(1,1), (1,2), (1,3)] [False, False, False]
-- b2 = Boat [(1,1), (1,2), (1,3)] [False, True, False]
-- b3 = Boat [(1,1), (4,4), (5,5)] [False, False, False]
-- b4 = Boat [(2,2), (4,4), (7,7)] [True, False, False]
-- b5 = Boat [(3,3), (5,5), (7,7)] [False, False, True]
-- checkOverlaps b1 b2 -> true
-- checkOverlaps b2 b3 -> true
-- checkOverlaps b3 b4 -> true
-- checkOverlaps b4 b5 -> true
-- checkOverlaps b5 b1 -> false
-- checkOverlaps b4 b2 -> false 
-- checkOverlaps b0 b1 -> false

checkOverlapsList :: Boat -> [Boat] -> Bool
-- check if single boat has overlap with any boat in list
checkOverlapsList boat lst =  any (checkOverlaps boat) lst

-- lob1 = []
-- lob2 = [b2, b3, b4, b5]
-- lob3 = [b3]
-- lob4 = [b1, b5]

-- checkOverlapsList b1 lob1 -> false
-- checkOverlapsList b1 lob2 -> true
-- checkOverlapsList b1 lob3 -> true
-- checkOverlapsList b0  lob4 -> false
-- checkOverlapsList (Boat [(10,10), (9,9)] [True, False])  lob4 -> false




placeBoat :: Integer -> IO [Boat] -> IO [Boat]
placeBoat n lst = do
    putStrLn("You are now placing your boat of size " ++ show n ++ ".")
    putStrLn "Do you want your boat to be horizontal or vertical? Type H or V"
    dir <- getLine
    let maincoord = if dir == "H" then "x" else "y"
    let secondcoord = if dir == "H" then "y" else "x"
    (val0, val1, val2) <- if dir == "H" then do
        putStrLn "What row is your boat on?"
        row <- readLn :: IO Integer
        putStrLn "What column do you want the head of your boat to be at?"
        headCol <- readLn :: IO Integer
        putStrLn "What column do you want the tail of your boat to be at?"
        tailCol <- readLn :: IO Integer
        return (row, headCol, tailCol)
    else do
        putStrLn "What column is your boat on?"
        col <- readLn :: IO Integer
        putStrLn "What row do you want the head of your boat to be at?"
        headRow <- readLn :: IO Integer
        putStrLn "What row do you want the tail of your boat to be at?"
        tailRow <- readLn :: IO Integer
        return (headRow, col, tailRow)
    let boat = Boat [(if dir == "H" then (i, val0) else (val0, i)) | i <- [(min val1 val2)..(max val1 val2)]] (replicate (fromInteger n) False)
    nonMonadBoats <- lst
    if checkOverlapsList boat nonMonadBoats then do
        putStrLn "You cannot place the boat here. Please try again."
        placeBoat n lst
    else return (boat:nonMonadBoats)

placeBoatRandom :: Int -> IO [Boat] -> IO [Boat]
placeBoatRandom n lst = do
  -- random orientation/direction
  dir::Integer <- randomRIO (1, 2)
  let dirStr = if dir `mod` 2 == 0 then "H" else "V"

  -- random pointing
  pointing::Integer <- randomRIO (1, 2)
  let pointingStr = if pointing `mod` 2 == 0 then "L" else "R"

  unMonadLst <- lst
  placeBoatRandomHelper n unMonadLst dirStr pointingStr

grabLengths :: Int -> Int -> Int -> String -> String -> (Int, Int, [(Int, Int)])
grabLengths n start_x start_y dir pointing
      | dir == "V" && pointing == "R" = ((start_x + n), start_y, [(i, start_y) | i <- [(min start_x (start_x + n))..(max start_x (start_x + n))]])
      | dir == "V" && pointing == "L" = ((start_x - n), start_y, [(i, start_y) | i <- [(min start_x (start_x - n))..(max start_x (start_x - n))]])
      | dir == "H" && pointing == "R" = (start_x, (start_y + n), [(start_x, i) | i <- [(min start_y (start_y + n))..(max start_y (start_y + n))]])
      | dir == "H" && pointing == "L" = (start_x, (start_y - n), [(start_x, i) | i <- [(min start_y (start_y - n))..(max start_y (start_y - n))]])

createBoat :: String -> Int -> Int -> Int -> Int -> Boat
createBoat dir start_x start_y end_x end_y = Boat [if dir == "H" then (fromIntegral start_x, i) else (i, fromIntegral start_y) | i <- [(min (fromIntegral start_x) (fromIntegral end_x))..(max (fromIntegral start_x) (fromIntegral end_x))]] (replicate (max (end_x - start_x) (end_y - start_y) + 1) False)
-- todo: change this to include start_y and end_y 



placeBoatRandomHelper :: Int -> [Boat] -> String -> String -> IO [Boat]
placeBoatRandomHelper n lst dir pointing = do

   (start_x, start_y) <- (,) <$> randomRIO (1, 10) <*> randomRIO (1, 10)

   let (end_x, end_y, coords) = grabLengths n start_x start_y dir pointing
   if not $ all checkBounds coords then
      placeBoatRandomHelper n lst dir pointing
   else
      let boat = createBoat dir start_x start_y end_x end_y in 
      if checkOverlapsList boat lst then
         placeBoatRandomHelper n lst dir pointing
      else 
         return (boat:lst)


   
