{-# LANGUAGE RecordWildCards #-}
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
genFigure :: Int -> Figure
genFigure a
  | a == 0    = Figure O DUp startpos
  | a == 1    = Figure I DUp startpos
  | a == 2    = Figure T DUp startpos
  | a == 3    = Figure J DUp startpos
  | a == 4    = Figure L DUp startpos
  | a == 5    = Figure S DUp startpos
  | otherwise = Figure Z DUp startpos
  where
    startpos = Coord {x = div screenWidth 2, y = blockSize * 2, clr = a}

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

genUniverse::StdGen -> GameState
genUniverse g = GameState genEmptyBoard (initFigures g) init_tact 0 0


genEmptyUniverse::StdGen -> GameState
genEmptyUniverse _ = GameState genEmptyBoard [] init_tact 0 0


-------------------------------------------------------------------------------------------------------------------------------------




