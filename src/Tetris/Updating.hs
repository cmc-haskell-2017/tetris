{-# LANGUAGE RecordWildCards #-}
module Tetris.Updating where

import Tetris.Types
import Tetris.Generating
import Tetris.Drawing
import Tetris.Colliding
import Tetris.Handling


-- =========================================
-- Updating
-- =========================================


-- | Checks if the game is over already
isGameOver::GameState -> Bool
isGameOver GameState{..} = collidesFigureDown (figureToDraw (head $ tail $ figures)) board


-- | Sorting rows (just to know which of the we should remove)
sortRows :: Board -> Board
sortRows []     = []
sortRows (c : brds) = sortRows (filter (\c1 -> y c1 > y c) brds) ++ [c] ++ sortRows (filter (\c1 -> y c1 <= y c) brds)


-- | Removing complete rows
deleteRows :: Board -> Board
deleteRows [] = []
deleteRows (c : brds)
  | isFullRow (row brd (y c)) = deleteRows . boardMoveDown $ (upperRows brd (y c)) ++ (lowerRows brd (y c))
  | otherwise = (row brd (y c)) ++ (deleteRows (upperRows brd (y c)))
  where 
    brd = (c : brds)


-- | Return all the rows above given one
upperRows :: Board -> Int -> Board
upperRows brd scope = (filter (\c1 -> y c1 < scope) brd)


-- | Return all the rows below given one
lowerRows :: Board -> Int -> Board
lowerRows brd scope = (filter (\c1 -> y c1 > scope) brd)


-- | Shifting rows downwards
boardMoveDown :: Board -> Board
boardMoveDown [] = []
boardMoveDown (c : brd) = c {y = (y c) + blockSize} : boardMoveDown brd


-- | returns n-th row of the board
row :: Board -> Int -> [Coord]
row b n = (filter (\b1 -> n == y b1) b)


-- | checks if the row is complete (contains 10 block in it)
isFullRow :: [Coord] -> Bool
isFullRow r = (length r) == 10


-- | turns a tuple of Coord to a a list
vectolist :: (Coord, Coord, Coord, Coord) -> [Coord]
vectolist (a,b,c,d) = [a,b,c,d]


-- | takes the figure that had fallen to the board
-- and makes it a part of the board
updateBoard::Figure -> Board -> Board
updateBoard (Figure sha dir c) a = a ++ vectolist (figureToDraw (Figure sha dir c))


-- | update handler for tetris. Provides updates depending on the time passed
updateTetris :: Float -> GameState -> GameState
updateTetris dt gs@GameState{..}  | gameover = GameState genEmptyBoard (tail figures) init_tact 0 0
                                  | otherwise = newLevel (newTact gs dt speed)
                                     where
                                       gameover = isGameOver gs


-- ===========================================
-- timing
-- ===========================================


-- | checks if it is tiem to move figure down 
newTact::GameState -> Float -> Float -> GameState
newTact gs@GameState{..} dt tact
  | paused = gs
  | new && collides = GameState (deleteRows $ sortRows $ updateBoard (head figures) board) (tail figures) speed time (score + 1)
  | new = newTact (GameState board ((moveFigureDown $ head figures) : (tail figures)) speed 0 score) (dt + time - tact) tact
  | collides = GameState board figures speed (time + dt + tact * 0.5) score
  | otherwise = GameState board figures speed (time + dt) score
                                        where
                                          new = time + dt >= tact
                                          collides =  collidesFigureDown (figureToDraw $ moveFigureDown $ head figures) board
                                          paused = speed < 0


-- | checks if the players score is high enough to start a new level
-- if it is - increases the speed of game
newLevel::GameState -> GameState
newLevel gs@GameState{..}
  | l5 = GameState board figures (signum(speed) * 0.1)  time score
  | l4 = GameState board figures (signum(speed) * 0.15) time score
  | l3 = GameState board figures (signum(speed) * 0.2)  time score
  | l2 = GameState board figures (signum(speed) * 0.25) time score
  | l2 = GameState board figures (signum(speed) * 0.3)  time score
  | l1 = GameState board figures (signum(speed) * 0.4)  time score
  | otherwise = gs
        where 
          l5 = score >= 5000
          l4 = score >= 3000 && score <= 5000
          l3 = score >= 2000 && score <= 3000
          l2 = score >= 1500 && score <= 2000
          l1 = score >= 1000 && score <= 1500



