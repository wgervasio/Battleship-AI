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
import Control.Concurrent (threadDelay)

delaySecs = 0 * 1000000
-- places all 5 boats on the board
-- places the enemy boats randomly
-- plays the first turn for the player

startGame=do

   enemyList <- foldl (\acc x -> placeBoatRandom x acc) (return []) [2,3,3,4,5]
   -- place your 5 boats
   playerList <- foldl (\acc x -> placeBoatRandom x acc) (return []) [2,3,3,4,5]
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
   putStrLn "\n\n\nYour turn. Enemy board:\n\n\n"
   printBoard(eboard)
  

   -- print (elist) -- for debugging and quick aim bot
   -- print (targets) -- for debugging
   coords <- getValidShot eboard promptShot

   let (newElist, newEboard, _) = checkShot coords elist eboard

   -- print enemy board after shot
   threadDelay delaySecs
   putStrLn "\n\n\nEnemy board after shot:\n\n\n"
   printBoard(newEboard)
   threadDelay delaySecs

   if gameWon newElist then
      putStrLn "You won!"
   else
      enemyTurn plist newElist pboard newEboard targets



enemyTurn :: [Boat] -> [Boat] -> Board -> Board -> [(Integer, Integer)]-> IO ()
enemyTurn plist elist pboard eboard [] = do
   -- print player board
   -- TODO: fix this
   putStrLn"THE EMPTY LIST"
   putStrLn "\n\n\nEnemy turn. Player board:\n\n\n"
   printBoard(pboard)


   coords <- getValidShot pboard pickShot


   -- putStrLn "The enemy shoots at row " ++ show x ++ ", column " ++ show y
   let (newPlist, newPboard, hit) = checkShot coords plist pboard 

   -- putStrLn "Enemy fired!"
   let newTargets = if hit then addAdjacent [] coords newPboard else []
   -- print newTargets

   threadDelay delaySecs
   putStrLn "\n\n\nPlayer board after shot:\n\n\n"
   printBoard(newPboard)
   print(newPlist) -- for debugging
   threadDelay delaySecs

   if gameWon newPlist then
      putStrLn "Enemy won!"
   else
      playerTurn newPlist elist newPboard eboard newTargets

enemyTurn plist elist pboard eboard (target:targets) = do
   -- print player board
   -- TODO: fix this
   putStrLn "THE NON EMPTY LIST"
   putStrLn "\n\n\nEnemy turn. Player board:\n\n\n"
   printBoard(pboard)

   coords <- pure target

   if shotAlready pboard coords then do
      enemyTurn plist elist pboard eboard targets
   else do
      let (newPlist, newPboard, hit) = checkShot coords plist pboard 
      let newTargets = if hit then addAdjacent targets coords newPboard else targets

      threadDelay delaySecs
      putStrLn "\n\n\nPlayer board after shot:\n\n\n"
      printBoard(newPboard)
      -- print(newPlist) -- for debugging
      threadDelay delaySecs

      if gameWon newPlist then
         putStrLn "Enemy won!"
      else
            playerTurn newPlist elist newPboard eboard newTargets
      
   -- putStrLn "The enemy shoots at row " ++ show x ++ ", column " ++ show y
   
   



-- checks if all boats are sunk in lst
gameWon :: [Boat] -> Bool
gameWon lst = and (map checkSunk lst)



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

   if dir == "V" then
      putStrLn "Do you want your boat to point up or down? Type U or D"
   else
      putStrLn "Do you want your boat to point left or right? Type L or R"
   pointing <- getLine

   putStrLn "What row is your boat on?"
   headRow <- readLn :: IO Integer
   putStrLn "What column do you want the head of your boat to be at?"
   headCol <- readLn :: IO Integer

   let coords = grabLengths n headCol headRow dir pointing

   if not $ all checkBounds coords then do
      putStrLn "Invalid coordinates. Please try again."
      placeBoat n lst
   else do
      unMonadLst <- lst
      let boat = Boat coords (replicate (fromInteger n) False) 
      if checkOverlapsList boat unMonadLst then do
         putStrLn "This overlaps another boat. Please try again."
         placeBoat n lst
      else 
         return (boat:unMonadLst)

placeBoatRandom :: Integer -> IO [Boat] -> IO [Boat]
placeBoatRandom n lst = do
  -- random orientation/direction
  dir::Integer <- randomRIO (1, 2)
  let dirStr = if dir `mod` 2 == 0 then "H" else "V"

  -- random pointing
  let (op1,op2) = if dirStr == "H" then ("L", "R") else ("U", "D")
  pointing::Integer <- randomRIO (1, 2)
  let pointingStr = if pointing `mod` 2 == 0 then op1 else op2

  unMonadLst <- lst
  placeBoatRandomHelper n unMonadLst dirStr pointingStr

grabLengths :: Integer -> Integer -> Integer -> String -> String -> [(Integer, Integer)]
grabLengths n start_x start_y dir pointing
      | dir == "H" && pointing == "R" = [(i, start_y) | i <- [(start_x-n)..start_x]]
      | dir == "H" && pointing == "L" = [(i, start_y) | i <- [start_x..(start_x+n)]]
      | dir == "V" && pointing == "U" = [(start_x, i) | i <- [start_y..(start_y+n)]]
      | dir == "V" && pointing == "D" = [(start_x, i) | i <- [(start_y - n)..start_y]]


placeBoatRandomHelper :: Integer -> [Boat] -> String -> String -> IO [Boat]
placeBoatRandomHelper n lst dir pointing = do

   -- generate heads of boats
   (start_x, start_y) <- (,) <$> randomRIO (1, 10) <*> randomRIO (1, 10)

   let coords = grabLengths (n - 1) start_x start_y dir pointing
   if not $ all checkBounds coords then
      placeBoatRandomHelper n lst dir pointing
   else
      let boat = Boat coords (replicate (fromInteger n) False)  in 
      if checkOverlapsList boat lst then
         placeBoatRandomHelper n lst dir pointing
      else 
         return (boat:lst)


   
