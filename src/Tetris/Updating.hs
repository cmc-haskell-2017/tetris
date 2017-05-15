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
-- import Tetris.Handling


isGameOver::GameState -> Bool
isGameOver GameState{..} = collidesFigureDown (figureToDraw (head $ tail $ figures)) board


sortRows :: Board -> Board
sortRows []     = []
sortRows ((brda,brdb,z):brds) = sortRows (filter (\(x,y,z) -> y > brdb) brds) ++ [(brda,brdb,z)] ++ sortRows (filter (\(x,y,z) -> y <= brdb) brds)


deleteRows :: Board -> Board
deleteRows [] = []
deleteRows ((brda,brdb,z):brds) | (length (filter (\(x,y,z) -> brdb == y) ((brda,brdb,z):brds)) == 10)  =  (deleteRows (map (\(x,y,z) -> (x, y + blockSize,z)) (filter (\(x,y,z) -> y < brdb) l)) ++ (filter (\(x,y,z) -> y > brdb) l))
                              | otherwise = (filter (\(x,y,z) -> brdb == y) ((brda,brdb,z):brds)) ++ (deleteRows  (filter (\(x,y,z) -> brdb /= y) ((brda,brdb,z):brds)))                  -----   ToDo:   Обработать левый операнд аппенда.  После функции проверить, что между У нет зазоров.
                         where l = (filter (\(x,y,z) -> brdb /= y) ((brda,brdb,z):brds))

--При нажатии клавиши "вниз" роняет фигуру 

-- =========================================
-- Updating
-- =========================================


vectolist :: (Coord, Coord, Coord, Coord) -> [Coord]
vectolist (a,b,c,d) = [a,b,c,d]

updateBoard::Figure -> Board ->Board
updateBoard (Figure sha dir (b ,c,z)) a = a ++ vectolist (figureToDraw (Figure sha dir (b ,c,z)))

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



