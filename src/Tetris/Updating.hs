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


isGameOver::Gamestate -> Bool
isGameOver (a,(f1:f2:rest),d,e) = collidesFigureDown (figureToDraw f2) a


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


updateTetris :: Float -> Gamestate -> Gamestate
updateTetris dt (a,(Figure sha dir (b,c,cl):rest),(sp, ti),e) | gameover = (genEmptyBoard,rest,(init_tact, 0),0)
                                                              -- | collide =  (deleteRows (sortRows (updateBoard (Figure sha dir (b ,c,cl)) a)), rest, (sp, ti), e + 1)
                                                              | otherwise = newLevel (newTact (a,(Figure sha dir (b,c,cl):rest),(sp, ti),e) dt sp)
                                                                 where
                                                                   -- collide =  collidesFigureDown (figureToDraw (Figure sha dir (b ,c + blockSize,cl)))   a
                                                                   gameover = isGameOver (a,(Figure sha dir (b,c,cl):rest),(sp, ti),e)
-- ===========================================
-- timing
-- =======================================

newTact::Gamestate -> Float -> Float -> Gamestate
newTact (b, (Figure sha dir (f1,f2,f3):rest), (sp, ti), s) dt tact
  | paused = (b, (Figure sha dir (f1,f2,f3):rest), (sp, ti), s)
  | new && collides = (deleteRows (sortRows (updateBoard (Figure sha dir (f1,f2,f3)) b)), rest, (sp, ti), s + 1)
  | new = newTact (b, (Figure sha dir (f1,f2 + blockSize,f3):rest), (sp, 0), s) (dt + ti - tact) tact
  | collides = (b, (Figure sha dir (f1,f2,f3):rest), (sp, ti + dt + tact * 0.3), s)
  | otherwise = (b, (Figure sha dir (f1,f2,f3):rest), (sp, ti + dt), s)
                                        where
                                          new = ti + dt >= tact
                                          collides =  collidesFigureDown (figureToDraw (Figure sha dir (f1 ,f2 + blockSize,f3))) b
                                          paused = sp < 0

newLevel::Gamestate -> Gamestate
newLevel (b, (Figure sha dir (f1,f2,f3)):rest, (sp, ti), s)
  | l5 = (b, (Figure sha dir (f1,f2,f3)):rest, (signum(sp) * 0.1, ti), s)
  | l4 = (b, (Figure sha dir (f1,f2,f3)):rest, (signum(sp) * 0.15, ti), s)
  | l3 = (b, (Figure sha dir (f1,f2,f3)):rest, (signum(sp) * 0.2, ti), s)
  | l2 = (b, (Figure sha dir (f1,f2,f3)):rest, (signum(sp) * 0.25, ti), s)
  | l2 = (b, (Figure sha dir (f1,f2,f3)):rest, (signum(sp) * 0.3, ti), s)
  | l1 = (b, (Figure sha dir (f1,f2,f3)):rest, (signum(sp) * 0.4, ti), s)
  | otherwise = (b, (Figure sha dir (f1,f2,f3)):rest, (sp, ti), s)
        where 
          l5 = s >= 5000
          l4 = s >= 3000 && s <= 5000
          l3 = s >= 2000 && s <= 3000
          l2 = s >= 1500 && s <= 2000
          l1 = s >= 1000 && s <= 1500

--Аргумент функции play, которая говорит, что длает каждая клавиша



