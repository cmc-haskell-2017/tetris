module Tetris.Generating where

import System.Random
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Geometry.Line
import Graphics.Gloss.Interface.Pure.Game
import GHC.Float

import Tetris.Types
import Tetris.Drawing

------------------------------------------------------------------------------------------------------------------------------------------

-- =========================================
-- Generating
-- =========================================

--На вход принимается случайное число от 0 до 6, которое определяет
--Фигуру
genFigure::Int -> Figure
genFigure a | a== 0  =  Figure O DUp (div screenWidth 2, blockSize * 2,0) 
            | a== 1  =  Figure I DUp (div screenWidth 2, blockSize * 2,1) 
            | a== 2  =  Figure T DUp (div screenWidth 2, blockSize * 2,2) 
            | a== 3  =  Figure J DUp (div screenWidth 2, blockSize * 2,3) 
            | a== 4  =  Figure L DUp (div screenWidth 2, blockSize * 2,4) 
            | a== 5  =  Figure S DUp (div screenWidth 2, blockSize * 2,5) 
            | a== 6  =  Figure Z DUp (div screenWidth 2, blockSize * 2,6) 

-- | Инициализировать случайный бесконечный
-- список чисел от 0 до 6 которые соответствуют фигурам
initFigures :: StdGen -> [Figure]
initFigures g = map genFigure
  (randomRs getrange g)

-- диапазон генерации случайных чисел
getrange :: (Int, Int)
getrange = (0, 6)
  


--Заполняем доску пустыми значениями и генерируем бесконечное количество фигур

genEmptyBoard::Board
genEmptyBoard = []

genRows::Int->Int->[Row]
genRows _ 0 = []
genRows w h = (genRows w (h-1)) ++ [genRow w]


genRow::Int->Row
genRow 0 = []
genRow w = (genRow (w-1)) ++ [Free]


genUniverse::StdGen -> Gamestate
genUniverse g = (genEmptyBoard,initFigures g,(init_tact, 0),0)


genEmptyUniverse::StdGen -> Gamestate
genEmptyUniverse _ = (genEmptyBoard,[],(init_tact, 0),0)


-------------------------------------------------------------------------------------------------------------------------------------


