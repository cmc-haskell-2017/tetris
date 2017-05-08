module Tetris.Types where

import System.Random
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Geometry.Line
import Graphics.Gloss.Interface.Pure.Game
import GHC.Float

glob_fps = 60::Int

-- =========================================
-- Types
-- =========================================

blockSize :: Int
blockSize = 30

init_tact::Time
init_tact = 0.7

                               --data Shape = J | L | I | S | Z | O | T
                               --         deriving (Eq, Show, Enum)

-- Представление фигуры, как списка блоков
type BlockedFigure = (Coord, Coord, Coord, Coord)

--Клетка заполнена?
data Block = Free | Full
         deriving(Eq, Show)

--Строки нашей доски
type Row = [Block]

--Все поле
type Board = [Coord]

--Счет
type Score = Int

--Координаты фигуры, поворот однозначно определяется 
--их последовательностью
type Coord = (Int, Int,Int)

--время
type Time = Float


--Состояние игры в текущий момент(разделили доску и фигуру,
--чтобы при полете фигуры не мигала вся доска, также, чтобы было более 
--оптимизировано)
--[Figure] - бесконечный список фигур, в текущем состоянии берем первый элемент списка
----------------------------------------------------------------------------------------------------------------------------------------------------------
type Gamestate = (Board,  [Figure], (Speed, Time), Score)

------------------------------------------------------------------------------------------------------------------------------------------------------------

--Скорость
type Speed = Float

--Для каждой фигуры свой тип, чтобы однозначно можно было 
--определить ее и тип операций над ней, например, фигуру I можно вращать
--произвольно только на расстоянии больше 4 клеток от края,
--а фигуру O на расстоянии больше 2 клеток от края

data FigureType = O | I | T | J | L | S | Z
                      deriving(Eq, Show)

data Direction = DUp | DDown | DLeft | DRight
                      deriving(Eq, Show)

data Figure = Figure FigureType Direction Coord 
                      deriving(Eq, Show)


-- | Ширина экрана.
screenWidth :: Int
screenWidth = 300

-- | Высота экрана.
screenHeight :: Int
screenHeight = 600

