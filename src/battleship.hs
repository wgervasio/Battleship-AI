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

startGame=do
   let playerList = []
   placeBoat(5, playerList)
   placeBoat(4, playerList)
   placeBoat(3, playerList)
   placeBoat(3, playerList)
   placeBoat(2, playerList)
   let enemyList = placeBoatRandom()
   playGame(playerList, enemyList, createPlayerBoard(playerList), makeBoard)




playGame :: [Boat] -> [Boat] -> [[Char]] -> [[Char]] -> Bool
playGame plist elist pboard eboard = do
   printBoard(pboard, eboard)
   putStrLn "What row do you want to fire at?"
   x <- read getLine :: Integer
   putStrLn "What column do you want to fire at?"
   y <- read getLine :: Integer
   checkShot((x,y), enemyBoard, enemyList)
   if gameWon playerList then
      putStrLn "You won!"
      True
   else
      playGameEnemy(plist elist pboard eboard)


playGameEnemy :: [Boat] -> [Boat] -> [[Char]] -> [[Char]] -> Bool
playGameEnemy plist elist pboard eboard = do
   printBoard(pboard, eboard)
   let shot = (randomR(1, 10), randomR(1,10))
   putStrLn "The enemy shoots at row " ++ show fst randomShot ++ ", column " ++ show snd randomShot
   checkShot(shot, enemyBoard, enemyList)
   if gameWon playerList then
      putStrLn "Enemy won!"
      False
   else
      playGame(plist elist pboard eboard)

gameWon :: [Boat] -> Bool
gameWon lst = foldr (\x y -> checkSunk x && y ) lst True

checkSunk :: Boat -> Bool
checkSunk Boat positions sunk = Foldr (||) sunk False

checkBounds :: (Integer, Integer) -> Bool
checkBounds (x, y) = (x >= 1 && x <= 10) && (y >= 1 && y <= 10)

checkAligned :: (Integer, Integer) -> (Integer, Integer) -> Integer -> Bool
checkAligned (x,y) (w,z) num = if (x /= w) && (y /= z) then False else (abs(x - w) == num) || (abs(y -z ) == num)


placeBoat :: Integer -> [Boat] -> [Boat]
placeBoat n lst = do
   -- we need to add checks for overlapping boats here 
   putStrLn("you are now placing your boat of size " ++ n ".")
   putStrLn "Do you want your boat to be horizontal or vertical? Type H or V"
   dir <- getLine
   let maincoord = if dir == "H" then "x" else "y"
   let secondcoord = if dir == "H" then "y" else "x"
   if dir =="H" then
      putStrLn "What row is your boat on?"
      val0 <- read getLine :: Integer
      putStrLn "What column do you want the head of your boat to be at?"
      val1 <- read getLine :: Integer
      putStrLn "What column do you want the tail of your boat to be at?"
      val2 <- read getLine :: Integer
      (Boat [(i, val0) | i <- [(min(val1, val2))..(max(val1,val2))] [ False | i <- [(min(val1, val2))..(max(val1,val2))]]):lst
   else 
      putStrLn "What column is your boat on?"
      val0 <- read getLine :: Integer
      putStrLn "What row do you want the head of your boat to be at?"
      val1 <- read getLine :: Integer
      putStrLn "What row do you want the tail of of your boat to be at?"
      val2 <- read getLine :: Integer
      (Boat [(val0, i) | i <- [(min(val1, val2))..(max(val1,val2))] [ False | i <- [(min(val1, val2))..(max(val1,val2))]]):lst
   
   -- placeBoatRandom :: Integer -> [Boat]
   placeBoatRandom n [] = 
   
<<<<<<< Updated upstream
-- TODO: implement placeBoatRandom
-- placeBoatRandom :: [Boat]
placeBoatRandom = putStrLn "Not implemented yet"

-- TODO: implement printBoard
-- printBoard :: IO [Char]
printBoard a b = putStrLn "Not implemented yet"

=======
      -- random orientation
      pos = (randomR(1,2))
      if pos `mod` 2 == 0 then
         pos == "H"
      else
         pos == "V"


      -- random position
      coords = (randomR(1, 10), randomR(1,10))
>>>>>>> Stashed changes







