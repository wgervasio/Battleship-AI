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
import qualified GHC.TypeLits

delaySecs = 0 * 1000000

-- function to start game. player places 5 boats manually, enemy places 5 randomly.
-- board is created and player starts
startGame=do
   -- enemy places 5 boats randomly, player is prompted to place 5 boats.
   -- each side has 1 boat of length 2, 2 boats of length 3, 1 of length 4, 1 of length 5
   enemyList <- foldl (\acc x -> placeBoatRandom x acc) (return []) [2,3,3,4,5]
   playerList <- foldl (\acc x -> placeBoatRandom x acc) (return []) [2,3,3,4,5]

   -- create boards for enemies and for player with their boat lists added on 
   let pboard =  addBoatsToBoard playerList
   let eboard = addBoatsToBoard enemyList

   -- start the game, player goes first
   playerTurn playerList enemyList pboard eboard []
   


-- function to handle player's turn at the game
-- takes inputs reflecting the player and enemy's boat lists, and the player and enemy's boards
-- also takes a list of the enemy's targets to be used on the next enemy turn
playerTurn :: [Boat] -> [Boat] -> Board -> Board -> [(Integer, Integer)]-> IO ()
playerTurn plist elist pboard eboard targets = do
--    -- print enemy board
   putStrLn "\n\n\nYour turn. Enemy board:\n\n\n"
   printBoard(eboard)
  

   -- prompt the player for a shot that is both within bounds and not targeting a cell they already shot
   coords <- getValidShot eboard promptShot

   -- check shot against enemy board and boats, updating if needed
   let (newElist, newEboard, _) = checkShot coords elist eboard

   -- print enemy board after shot
   threadDelay delaySecs
   putStrLn "\n\n\nEnemy board after shot:\n\n\n"
   printBoard(newEboard)
   threadDelay delaySecs

   -- check if game is won, move to enemy turn if not
   if gameWon newElist then
      putStrLn "You won!"
   else
      enemyTurn plist newElist pboard newEboard targets


-- function to handle enemy turn. takes the player and enemy boat lists, player and enemy boards
-- and a list of cells to target if applicable
-- enemy behaviour has two simple modes: if there are no cells to target, it will fire on a random cell with parity of 2
-- if it has valid targets, it will fire on those until exhausting them
-- if a shot is a hit, all adjacent valid spaces will be added to the targets list 
enemyTurn :: [Boat] -> [Boat] -> Board -> Board -> [(Integer, Integer)]-> IO ()
enemyTurn plist elist pboard eboard [] = do
   --  case where no valid targets
   putStrLn"THE EMPTY LIST"
   putStrLn "\n\n\nEnemy turn. Player board:\n\n\n"
   printBoard(pboard)

   --pick random shot of parity 2
   coords <- pickShot pboard


   -- putStrLn "The enemy shoots at row " ++ show x ++ ", column " ++ show y
   let (newPlist, newPboard, hit) = checkShot coords plist pboard 

   -- putStrLn "Enemy fired!"
   -- if hit, then add valid adjacet spaces to targets
   let newTargets = if hit then addAdjacent [] coords newPboard else []
   -- print newTargets

   threadDelay delaySecs
   putStrLn "\n\n\nPlayer board after shot:\n\n\n"
   printBoard(newPboard)
   threadDelay delaySecs

   if gameWon newPlist then
      putStrLn "Enemy won!"
   else
      playerTurn newPlist elist newPboard eboard newTargets

enemyTurn plist elist pboard eboard (target:targets) = do
   -- case with valid targets
   putStrLn "\n\n\nEnemy turn. Player board:\n\n\n"
   printBoard(pboard)

   --take the first from the list of valid targets
   coords <- pure target

   if shotAlready pboard coords then do
      -- if already shot, recurse with the rest of the list until an un-shot cell is discovered
      enemyTurn plist elist pboard eboard targets
   else do
      -- update board, list and targets as normal
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
         -- return to player turn
         playerTurn newPlist elist newPboard eboard newTargets
      
   -- putStrLn "The enemy shoots at row " ++ show x ++ ", column " ++ show y
   
   



-- checks if all boats are sunk in lst
gameWon :: [Boat] -> Bool
gameWon lst = and (map checkSunk lst)


-- function to create a target list for enemy.
-- once a boat is hit successfully, this is called to create a list of all adjacent spaces
-- filtering spaces that are out of bounds, already shot, or already in the list of targets
addAdjacent :: [(Integer, Integer)] -> (Integer, Integer) -> Board -> [(Integer, Integer)]
addAdjacent shots (x,y) board =  (filter (\x -> not (x `elem` shots)) (filter (\x -> (not (shotAlready board x))) (filter checkBounds [(x+1,y), (x-1,y), (x,y+1), (x,y-1)]))) ++ shots


checkOverlaps :: Boat -> Boat -> Bool
-- check if two boats are overlapped
checkOverlaps b1 b2 =
   any (`elem` b2coords) b1coords
      where
         b1coords = positions b1 
         b2coords = positions b2

checkOverlapsList :: Boat -> [Boat] -> Bool
-- check if single boat has overlap with any boat in list
checkOverlapsList boat lst =  any (checkOverlaps boat) lst




-- prompt the player to place their boats on the board via input-output
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

-- create a boat of length n in a valid board position 
placeBoatRandom :: Integer -> IO [Boat] -> IO [Boat]
placeBoatRandom n lst = do
  -- create a random boat oriented either horizontally or vertically
  dir::Integer <- randomRIO (1, 2)
  let dirStr = if dir `mod` 2 == 0 then "H" else "V"

  -- given a horizontal boat, point it either left or right. given a vertical boat, point it either up or down
  let (op1,op2) = if dirStr == "H" then ("L", "R") else ("U", "D")
  pointing::Integer <- randomRIO (1, 2)
  let pointingStr = if pointing `mod` 2 == 0 then op1 else op2

  unMonadLst <- lst
  placeBoatRandomHelper n unMonadLst dirStr pointingStr

-- provide a list of coordinates reflecting a boat's position given positional data 
grabLengths :: Integer -> Integer -> Integer -> String -> String -> [(Integer, Integer)]
grabLengths n start_x start_y dir pointing
      | dir == "H" && pointing == "R" = [(i, start_y) | i <- [(start_x-n)..start_x]]
      | dir == "H" && pointing == "L" = [(i, start_y) | i <- [start_x..(start_x+n)]]
      | dir == "V" && pointing == "U" = [(start_x, i) | i <- [start_y..(start_y+n)]]
      | dir == "V" && pointing == "D" = [(start_x, i) | i <- [(start_y - n)..start_y]]

-- generate a random boat of n length, facing in dir direction(H for horizontal or V for vertical)
-- pointing in pointing direction(R, L, U, D)
placeBoatRandomHelper :: Integer -> [Boat] -> String -> String -> IO [Boat]
placeBoatRandomHelper n lst dir pointing = do

   -- generate heads of boats
   (start_x, start_y) <- (,) <$> randomRIO (1, 10) <*> randomRIO (1, 10)

   let coords = grabLengths (n - 1) start_x start_y dir pointing
   if not $ all checkBounds coords then
      -- if boat is out of bounds then repeat
      placeBoatRandomHelper n lst dir pointing
   else
      let boat = Boat coords (replicate (fromInteger n) False)  in 
      if checkOverlapsList boat lst then
         -- if boat overlaps with other enemy boats then repeat
         placeBoatRandomHelper n lst dir pointing
      else 
         return (boat:lst)


   
