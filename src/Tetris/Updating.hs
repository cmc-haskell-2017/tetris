{-# LANGUAGE RecordWildCards #-}
module Tetris.Updating where

import System.Random
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Geometry.Line
import Graphics.Gloss.Interface.Pure.Game
import GHC.Float

import Tetris.Types
import Tetris.Generating
import Tetris.Drawing
import Tetris.Colliding
import Tetris.Handling


isGameOver::GameState -> Bool
isGameOver GameState{..} = collidesFigureDown (figureToDraw (head $ tail $ figures)) board


-- | Сортируем строки.
sortRows :: Board -> Board
sortRows []     = []
sortRows (c : brds) = sortRows (filter (\c1 -> y c1 > y c) brds) ++ [c] ++ sortRows (filter (\c1 -> y c1 <= y c) brds)

-- | Удалям заполненные строки.
deleteRows :: Board -> Board
deleteRows [] = []
deleteRows (c : brds)
  | isFullRow (row brd (y c)) = deleteRows . boardMoveDown $ (upperRows brd (y c)) ++ (lowerRows brd (y c))
  | otherwise = (row brd (y c)) ++ (deleteRows (upperRows brd (y c)))
  where 
    brd = (c : brds)

-- | Строки выше заданной строки.
upperRows :: Board -> Int -> Board
upperRows brd scope = (filter (\c1 -> y c1 < scope) brd)

-- | Строки ниже заданной строки.
lowerRows :: Board -> Int -> Board
lowerRows brd scope = (filter (\c1 -> y c1 > scope) brd)

-- | Сдвигаем строки доски вниз.
boardMoveDown :: Board -> Board
boardMoveDown [] = []
boardMoveDown (c : brd) = c {y = (y c) + blockSize} : boardMoveDown brd

-- | n-ая строка доски.
row :: Board -> Int -> [Coord]
row b n = (filter (\b1 -> n == y b1) b)

-- | Заполнена ли доска?
isFullRow :: [Coord] -> Bool
isFullRow r = (length r) == 10

--При нажатии клавиши "вниз" роняет фигуру 

-- =========================================
-- Updating
-- =========================================


vectolist :: (Coord, Coord, Coord, Coord) -> [Coord]
vectolist (a,b,c,d) = [a,b,c,d]

updateBoard::Figure -> Board ->Board
updateBoard (Figure sha dir c) a = a ++ vectolist (figureToDraw (Figure sha dir c))

--На основании прошедшего времени меняет скорость полета фигур
updateSpeed::Time -> Speed -> Speed
updateSpeed _ _ = 0


--Аргумент функции play, обновляет состояние тетриса
--С каждым кадром двигает фигуру вниз и пока здесь же проверяет, не достигла ли фигура нижней границы


updateTetris :: Float -> GameState -> GameState
updateTetris dt gs@GameState{..}  | gameover = GameState genEmptyBoard (tail figures) init_tact 0 0
                                                              -- | collide =  (deleteRows (sortRows (updateBoard (Figure sha dir (b ,c,cl)) a)), rest, (sp, ti), e + 1)
                                  | otherwise = newLevel (newTact gs dt speed)
                                     where
                                       -- collide =  collidesFigureDown (figureToDraw (Figure sha dir (b ,c + blockSize,cl)))   a
                                       gameover = isGameOver gs
-- ===========================================
-- timing
-- =======================================


-- (b, (Figure sha dir (f1,f2,f3):rest), (sp, ti), s)
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

--Аргумент функции play, которая говорит, что длает каждая клавиша



