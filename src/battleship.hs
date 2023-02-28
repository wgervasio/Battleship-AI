-- Battleship in Haskell
{-# LANGUAGE BlockArguments #-}
module BattleShip where

import ShipData
import Shot
import Enemy
import System.Random
import System.IO
import Control.Concurrent (threadDelay)
import Text.Read

delaySecs = 0 * 1000000

-- a datatype representing the state of the game at a given time
-- contains the boards and boat lists of both players, and a list of valid spaces adjacent to the AI's previous hits 
data GameState = GameState {
    player :: Player,
    playerList :: [Boat],
    enemyList :: [Boat],
    playerBoard :: Board,
    enemyBoard :: Board,
    aiShots :: [(Integer, Integer)]
} deriving (Show)

data Player = User | AI deriving (Show)


-- proceeds through player turns by taking in a gamestate as an input
takeTurn :: GameState -> IO GameState
takeTurn gs =
   case player gs of 
      User -> do   
            (newElist, newEboard, _) <- playerTurn (enemyList gs) (enemyBoard gs)
            return GameState { player = AI, playerList = (playerList gs), enemyList = newElist, playerBoard = (playerBoard gs), enemyBoard = newEboard, aiShots = (aiShots gs) }
      AI -> do 
            (newPlist, newPboard, newTargets) <- enemyTurn (playerList gs) (playerBoard gs) (aiShots gs) 
            return GameState { player = User, playerList = newPlist, enemyList = (enemyList gs), playerBoard = newPboard, enemyBoard = (enemyBoard gs), aiShots = newTargets }


isGameOver :: GameState -> Bool
isGameOver gs = gameWon (playerList gs) || gameWon (enemyList gs)





-- places all 5 boats on the board
-- places the enemy boats randomly
-- plays the first turn for the player

setupGame = do
   -- hacky fix because using foldl made error handling very difficult
   playerBoats <- placeBoat 5 (return [])
   printBoard (addBoatsToBoard playerBoats)
   playerBoats <- placeBoat 4 (return playerBoats)
   printBoard (addBoatsToBoard playerBoats)
   playerBoats <- placeBoat 3 (return playerBoats)
   printBoard (addBoatsToBoard playerBoats)
   playerBoats <- placeBoat 3 (return playerBoats)
   printBoard (addBoatsToBoard playerBoats)
   playerBoats <- placeBoat 2 (return playerBoats)
   printBoard (addBoatsToBoard playerBoats)

   enemyBoats <- foldl (\acc x -> placeBoatRandom x acc) (return []) [2,3,3,4,5]
   return GameState { 
      player = User,
      playerList = playerBoats, 
      enemyList = enemyBoats, 
      playerBoard = (addBoatsToBoard playerBoats), 
      enemyBoard = (addBoatsToBoard enemyBoats), 
      aiShots = []
   }

-- function to start game. player places 5 boats manually, enemy places 5 randomly.
-- board is created and player starts
startGame :: IO ()
startGame=do
   initialGame <- setupGame
   gameLoop initialGame

-- loop to play through the game
gameLoop gs = do
   if isGameOver gs then
      return ()
   else do
      newState <- takeTurn gs
      gameLoop newState

-- function for processing each turn. takes a lambda function for determining each shot. on a player's turn, their shot will be
-- prompted through the console. on the enemy's turn, their shot will be determined according to the algorithm
-- defined elsewhere in this file.

turn :: [Boat] -> Board -> [Char] -> [Char] -> IO ([Boat], Board, [(Integer, Integer)]) -> IO ([Boat], Board, [(Integer, Integer)])
turn lst board player1 player2 shotLambda= do
   let line = replicate 30 '='
   putStrLn line
   putStrLn ("\n\n\n" ++ player1 ++ " turn. "++ player2 ++ " board:\n\n\n")
   -- use printBoardEnemy so the player can't see their enemy's boats 
   if player1 == "Player" then printBoardEnemy board else printBoard board

   (newList, newBoard, newTargets) <- shotLambda

   putStrLn ("\n\n\n" ++ player2 ++ "'s board after shot:\n\n\n")
   printBoard(newBoard)
   threadDelay delaySecs

   if (gameWon newList) then do
      putStrLn ("\n\n\n" ++ player1 ++ " wins!\n\n\n")
      return (newList, newBoard, newTargets)
   else do
      putStrLn ("\n\n\n" ++ player1 ++ " turn over!\n\n\n")
      return (newList, newBoard, newTargets)

   
   

-- function to handle player's turn at the game
-- takes inputs reflecting the player and enemy's boat lists, and the player and enemy's boards
-- also takes a list of the enemy's targets to be used on the next enemy turn

playerTurn :: [Boat] -> Board -> IO ([Boat], Board, [(Integer, Integer)])
playerTurn elist eboard = do
   turn elist eboard "Player" "Enemy" playerShot where
      playerShot = do
         coords <- getValidShot eboard promptShot
         let (newElist, newEboard, _) = checkShot coords elist eboard
         return (newElist, newEboard, [])




-- function to handle enemy turn. takes the player and enemy boat lists, player and enemy boards
-- and a list of cells to target if applicable
-- enemy behaviour has two simple modes: if there are no cells to target, it will fire on a random cell with parity of 2
-- if it has valid targets, it will fire on those until exhausting them
-- if a shot is a hit, all adjacent valid spaces will be added to the targets list 
-- enemy behaviour inspired by this blog post: http://www.datagenetics.com/blog/december32011/
enemyTurn :: [Boat] -> Board -> [(Integer, Integer)] -> IO ([Boat], Board, [(Integer, Integer)])
-- case with no valid targets
enemyTurn plist pboard [] = do
   turn plist pboard "Enemy" "Player" enemyShot where
      enemyShot = do
         coords <- getValidShot pboard pickShot
         let (newPlist, newPboard, hit) = checkShot coords plist pboard 
         let newTargets = if hit then addAdjacent [] coords newPboard else []
         return (newPlist, newPboard, newTargets)

-- case with some valid targets left
enemyTurn plist pboard (coords:targets) = do
   turn plist pboard "Enemy" "Player" enemyShot where
      enemyShot = do
         (x, y) <- if null coords then pickShot else return coords
         let (newPlist, newPboard, hit) = checkShot (x,y) plist pboard
         let newTargets = if hit then addAdjacent targets (x,y) newPboard else targets
         return (newPlist, newPboard, newTargets)
   
   



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


checkOverlapsList :: Boat -> [Boat] -> Bool
-- check if single boat has overlap with any boat in list
checkOverlapsList boat lst =  any (checkOverlaps boat) lst




-- prompt the player to place their boats on the board via input-output
placeBoat :: Integer -> IO [Boat] -> IO [Boat]
placeBoat n lst = do
   putStrLn("You are now placing your boat of size " ++ show n ++ ".")
   putStrLn "Do you want your boat to be horizontal or vertical? Type H or V"
   dir <- checkInput (`elem` ["H","V"]) "Invalid direction. Please try again."
   
   let points = []
   if dir == "V" then
      putStrLn "Do you want your boat's tail to point up or down? Type U or D"
   else
      putStrLn "Do you want your boat's tail to point left or right? Type L or R"
   let validPoints = if dir == "H" then ["L","R"] else ["U","D"]
   pointing <- checkInput(`elem` validPoints) "Invalid direction. Please try again."

   coordsHelper lst n dir pointing

coordsHelper :: IO [Boat] -> Integer -> String -> String -> IO [Boat]
coordsHelper lst n dir pointing= do
   putStrLn "What row is your boat on?"
   headRow <- getInt
   putStrLn "What column do you want the head of your boat to be at?"
   headCol <- getInt
   let coords = grabLengths (n - 1) headCol headRow dir pointing
   let boat = Boat coords (replicate (fromInteger n) False) 
   unmonadLst <- lst
   if not $ all checkBounds coords then do
      putStrLn "Invalid coordinates. Please try again."
      coordsHelper lst n dir pointing
   else if checkOverlapsList boat unmonadLst then do
         putStrLn ("This overlaps another boat. Please try again. You are trying to place a boat of size " ++ show n)
         coordsHelper lst n dir pointing
      else 
         return (boat:unmonadLst)


-- gets an IO input and checks if it is valid with f
-- if the input is invalid, output msg and call itself again until a valid input is received
checkInput :: (String -> Bool) -> String -> IO String
checkInput f msg =do
    input <- getLine
    if f input then 
      pure input
    else do
      putStrLn msg 
      checkInput f msg

-- io input function that calls itself until receiving a valid integer input
getInt :: IO Integer
getInt = do
   putStrLn("Please input a valid integer between 1 and 10.")
   input <- getLine
   case readMaybe input :: Maybe Integer of
      Just number -> pure number
      Nothing -> getInt

-- place a random boat onto the board. used for creating enemy boat positions
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
      | dir == "H" && pointing == "L" = [(i, start_y) | i <- [(start_x-n)..start_x]]
      | dir == "H" && pointing == "R" = [(i, start_y) | i <- [start_x..(start_x+n)]]
      | dir == "V" && pointing == "D" = [(start_x, i) | i <- [start_y..(start_y+n)]]
      | dir == "V" && pointing == "U" = [(start_x, i) | i <- [(start_y - n)..start_y]]

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


   
