-- Battleship in Haskell
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
import System.Random


startGame :: IO [Boat]
-- places all 5 boats on the board
-- places the enemy boats randomly
-- plays the first turn for the player
startGame=do
   let enemyList = []
   placeBoatRandom(5, playerList)
   placeBoatRandom(4, playerList)
   placeBoatRandom(3, playerList)
   placeBoatRandom(3, playerList)
   placeBoatRandom(2, playerList)

   let playerList = []
   placeBoat(5, playerList)
   placeBoat(4, playerList)
   placeBoat(3, playerList)
   placeBoat(3, playerList)
   placeBoat(2, playerList)
   -- place your 5 boats
   
   playerTurn(playerList, enemyList, createBoard(playerList), createBoard(enemyList))
   -- call player turn



playerTurn :: [Boat] -> [Boat] -> [[Char]] -> [[Char]] -> Bool
-- prompts user for shot
-- checks if shot is valid
-- checks if shot hits a boat
-- checks if all enemy boats are sunk
playerTurn plist elist pboard eboard = do
   -- print enemy board
   -- TODO change this
   printBoard(pboard, eboard)


   (x,y) <- promptShot()
   (elist-new, eboard-new)<- checkShot((x,y), elist, eboard)

   -- print enemy board after shot
   -- TODO change this
   printBoard(eboard-new)

   if gameWon elist-new then
      putStrLn "You won!"
   else
      enemyTurn(plist elist-new pboard eboard-new)


enemyTurn :: [Boat] -> [Boat] -> [[Char]] -> [[Char]] -> Bool
enemyTurn plist elist pboard eboard = do
   -- print player board
   -- TODO: fix this
   printBoard(pboard)

   (x,y) <- randomShot()

   putStrLn "The enemy shoots at row " ++ show x ++ ", column " ++ show y
   (plist-new, pboard-new) <- checkShot((x,y), plist, pboard)
   if gameWon plist-new then
      putStrLn "Enemy won!"
   else
      playerTurn(plist-new elist pboard-new eboard)



gameWon :: [Boat] -> Bool
-- checks if all boats are sunk in lst
gameWon lst = and (map checkSunk lst)

checkSunk :: Boat -> Bool
-- checks if a boat is sunk
checkSunk (Boat _ lst) = and lst

checkBounds :: (Integer, Integer) -> Bool
checkBounds (x, y) = (x >= 1 && x <= 10) && (y >= 1 && y <= 10)

checkAligned :: (Integer, Integer) -> (Integer, Integer) -> Integer -> Bool
checkAligned (x,y) (w,z) num = if (x /= w) && (y /= z) then False else (abs(x - w) == num) || (abs(y -z ) == num)

checkOverlaps :: [Boat] -> Bool
-- TODO: implement this


placeBoat :: Integer -> [Boat] -> IO [Boat]
placeBoat n lst = do
      putStrLn("you are now placing your boat of size " ++ n ".")
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
   let is_overlap = any (checkOverlaps boat) lst
   if is_overlap then do
      putStrLn "You cannot place the boat here. Please try again."
      placeBoat n lst
   else return (boat:lst)

placeBoatRandom :: Int -> [Boat] -> IO [Boat]
placeBoatRandom n lst = do
  -- random orientation/direction
  dir <- randomRIO (1, 2)
  let dirStr = if dir `mod` 2 == 0 then "H" else "V"

  -- random pointing
  pointing <- randomRIO (1, 2)
  let pointingStr = if pointing `mod` 2 == 0 then "L" else "R"

  let loop = do
      start_x <- randomRIO (1, 10)
      start_y <- randomRIO (1, 10)
      let (end_x, end_y, coords) = if dir == 1 then
         if pointing == 1 then
            let ex = start_x + n in (ex, start_y, [(i, start_y) | i <- [(min(start_x, ex))..(max(start_x, ex))]])
         else
            let ex = start_x - n in (ex, start_y, [(i, start_y) | i <- [(min(start_x, ex))..(max(start_x, ex))]])
         else
            if pointing == 1 then
               let ey = start_y + n in (start_x, ey, [(start_x, i) | i <- [(min(start_y, ey))..(max(start_y, ey))]])
            else
               let ey = start_y - n in (start_x, ey, [(start_x, i) | i <- [(min(start_y, ey))..(max(start_y, ey))]])
      let not_valid = not $ all checkBounds coords
      if not_valid then
         loop
      else 
         let boat = if dirStr == "H" then
            Boat [(val0, i) | i <- [(min(start_x, end_x))..(max(start_x, end_x))]] (replicate n False):lst
         else
            Boat [(i, val0) | i <- [(min(start_y, end_y))..(max(start_y, end_y))]] (replicate n False):lst
      let is_overlap = any (checkOverlaps boat) lst
      if is_overlap then do
         placeBoatRandom n lst
      else return (boat:lst)
   loop







