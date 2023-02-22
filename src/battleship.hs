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
import System.Random



-- places all 5 boats on the board
-- places the enemy boats randomly
-- plays the first turn for the player
startGame=do
   let enemyList = foldl placeBoatRandom [] [2,3,3,4,5]


   let playerList = foldl placeBoatRandom [] [2,3,3,4,5]
   -- place your 5 boats
   
   playerTurn(playerList, enemyList, addBoatsToBoard(playerList), addBoatsToBoard(enemyList))
   -- call player turn




-- prompts user for shot
-- checks if shot is valid
-- checks if shot hits a boat
-- checks if all enemy boats are sunk
playerTurn plist elist pboard eboard = do
   -- print enemy board
   -- TODO change this
   printBoard(eboard)


   let (x,y) =  promptShot
   let (newElist, newEboard) = checkShot((x,y), elist, eboard)

   -- print enemy board after shot
   -- TODO change this
   printBoard(newEboard)

   if gameWon newElist then
      putStrLn "You won!"
   else
      enemyTurn(plist newElist pboard newEboard)



enemyTurn plist elist pboard eboard = do
   -- print player board
   -- TODO: fix this
   printBoard(pboard)

   let (x,y) =  randomShot

   putStrLn "The enemy shoots at row " ++ show x ++ ", column " ++ show y
   let (newPlist, newPboard) = checkShot((x,y), plist, pboard) in
      if gameWon newPlist then
         putStrLn "Enemy won!"
      else
         playerTurn(newPlist elist newPboard eboard)




-- checks if all boats are sunk in lst
gameWon lst = and (map checkSunk lst)



checkAligned (x,y) (w,z) num = if (x /= w) && (y /= z) then False else (abs(x - w) == num) || (abs(y -z ) == num)

-- check if two boats are overlapped

checkOverlaps b1 b2 =
   any b1coords `elem` b2coords
      where
         b1coords = positions b1 
         b2coords = positions b2



placeBoat n lst = do
    putStrLn("you are now placing your boat of size " ++ show n ++ ".")
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


placeBoatRandom n lst = do
  -- random orientation/direction
  dir <- randomRIO (1, 2)
  let dirStr = if dir `mod` 2 == 0 then "H" else "V"

  -- random pointing
  pointing <- randomRIO (1, 2)
  let pointingStr = if pointing `mod` 2 == 0 then "L" else "R"

  return placeBoatRandomHelper n lst dirStr pointingStr

grabLengths n start_x start_y dir pointing
      | dir == "V" && pointing == "R" = ((start_x + n), start_y, [(i, start_y) | i <- [(min(start_x, (start_x + n)))..(max(start_x, (start_x + n)))]])
      | dir == "V" && pointing == "L" = ((start_x - n), start_y, [(i, start_y) | i <- [(min(start_x, (start_x - n)))..(max(start_x, (start_x - n)))]])
      | dir == "H" && pointing == "R" = (start_x, start_y + n, [(start_x, i) | i <- [(min(start_y, start_y + n))..(max(start_y, start_y + n))]])
      | dir == "H" && pointing == "L" = (start_x, start_y - n, [(start_x, i) | i <- [(min(start_y, start_y - n))..(max(start_y, start_y - n))]])


createBoat dir start_x start_y end_x end_y n = Boat [if dir == "H" then (val0, i) else (i, val0) | i <- [(min(start_x, end_x))..(max(start_x, end_x))]] (replicate n False)

checkOverlapsList boat lst =  any (checkOverlaps boat) lst


placeBoatRandomHelper n lst dir pointing = do

   (start_x, start_y) <- (,) <$> randomRIO (1, 10) <*> randomRIO (1, 10)

   let (end_x, end_y, coords) = grabLengths n start_x start_y dir pointing
   if not $ all checkBounds coords then
      placeBoatRandomHelper n lst dir pointing
   else
      let boat = createBoat dir start_x start_y end_x end_y n in 
      if checkOverlapsList boat lst then
         placeBoatRandom n lst
      else 
         return (boat:lst)




