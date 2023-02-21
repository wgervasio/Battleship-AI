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
   playGame(playerList, enemyList, makePlayerBoard(playerList), makeBoard)




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
checkBounds (x, y) = (x >= 0 && x <= 10) && (y >= 0 && y <= 10)

checkAligned :: (Integer, Integer) -> (Integer, Integer) -> Integer -> Bool
checkAligned (x,y) (w,z) num = if (x /= w) && (y /= z) then False else (abs(x - w) == num) || (abs(y -z ) == num)


placeBoat :: Integer -> [Boat] -> [Boat]
placeBoat n lst = do
   -- we need to add checks for overlapping boats here 
   putStrLn "Do you want your boat to be horizontal or vertical? Type H or V"
   dir <- getLine
   let maincoord = if dir == "H" then "x" else "y"
   let secondcoord = if dir == "H" then "y" else "x"
   putStrLn "What should the first " ++ maincoord ++ " value of your boat be?"
   val1 <- read getLine :: Integer
   putStrLn "What should the second " ++ maincoord ++ " value of your boat be?"
   val2 <- read getLine :: Integer
   putStrLn "What should the " ++ secondcoord ++ " value of your boat be?"
   val3 <- read getLine :: Integer
   if dir == "H" then   
      (Boat [(i, val3) | i <- [val1..val2]] [i > 999 | i <- [val1..val2]]):lst
   else
      (Boat [(val3, i) | i <- [val1..val2]] [i > 999 | i <- [val1..val2]]):lst
   




