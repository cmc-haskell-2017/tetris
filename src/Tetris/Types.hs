{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Tetris.Types where

import System.Random
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Geometry.Line
import Graphics.Gloss.Interface.Pure.Game
import GHC.Float

import Data.Binary
import Network.WebSockets
import GHC.Generics

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

type PlayerName = String

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
type Coord = (Int, Int, Int)

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
                      deriving(Generic, Eq, Show)

data Direction = DUp | DDown | DLeft | DRight
                      deriving(Generic, Eq, Show)

data Figure = Figure FigureType Direction Coord 
                      deriving(Generic, Eq, Show)

instance Binary FigureType
instance Binary Direction 
instance Binary Figure


-- | Ширина экрана.
screenWidth :: Int
screenWidth = 300

-- | Высота экрана.
screenHeight :: Int
screenHeight = 600


data WebGS = WebGS
  { board   :: Board
  , figures :: [Figure]
  , speed   :: Speed
  , time    :: Time
  , score   :: Score
  } deriving (Generic)

instance Show WebGS where
	show WebGS{..} = 
		show board ++ " " ++
		show (head figures) ++ " " ++ 
		show speed ++ " " ++
		show time ++ " " ++
		show score ++ "end "

instance Binary WebGS

instance WebSocketsData WebGS where
  fromLazyByteString = decode
  toLazyByteString   = encode


fromWebGS :: WebGS -> Gamestate
fromWebGS WebGS{..} = (board, figures, (speed, time), score)


toWebGS :: Gamestate -> WebGS
toWebGS (board, figures, (speed, time), score) = WebGS board figures speed time score