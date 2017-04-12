module Tetris where

import System.Random
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Geometry.Line
import Graphics.Gloss.Interface.Pure.Game

run :: IO ()
run = putStrLn "This project is not yet implemented"


-- =========================================
-- Types
-- =========================================





                               --data Shape = J | L | I | S | Z | O | T
                               --       	deriving (Eq, Show, Enum)
--Клетка заполнена?
data Block = Free | Full

--Строки нашей доски
type Row = [Block]

--Все поле
type Board = [Row]

--Счет
type Score = Integer

--Координаты фигуры, поворот однозначно определяется 
--их последовательностью
type Coord = (Int, Int)

--Состояние игры в текущий момент(разделили доску и фигуру,
--чтобы при полете фигуры не мигала вся доска, также, чтобы было более 
--оптимизировано)
type Time = Float
type Gamestate = (Board,  Figure, Speed, Time)

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
screenWidth = 800

-- | Высота экрана.
screenHeight :: Int
screenHeight = 450

-- =========================================
-- Generating
-- =========================================





--На вход принимается случайное число от 0 до 6, которое определяет
--Фигуру
genFigure::Int -> Figure
genFigure _ = Figure O DUp (0,0)

--Заполняем доску пустыми значениями
genEmptyBoard::Board
genEmptyBoard =  [[Free]]
--Генерируем бесконечный список из случайных фигур
generateRandomFigureList:: StdGen -> [Figure]
generateRandomFigureList _ =  [Figure O DUp (0,0)]



-- =========================================
-- Moves
-- =========================================




--Поворачивает фигуру: положение фигуры в пространстве опредляется 
--двумя числами, функция смотрит, какая ей дана фигура, и вычисляет 
--расстояние до края доски и на основании этой информации поворачивает ее
--(если это можно сделать), т.е. изменяет 3 координату

-- data Figure = O Direction Coord |
-- 			  I Direction Coord | 
-- 			  T Direction Coord |
-- 			  J Direction Coord | L  Direction Coord | 
-- 			  S Direction Coord | Z  Direction Coord
-- 			  	deriving(Eq, Show)

-- data Direction = Up | Down | Left | Right
type BlockedFigure = (Coord, Coord, Coord, Coord)


turn::Figure -> Figure
turn (Figure t DUp c) = Figure t DRight c
turn (Figure t DRight c) = Figure t DDown c
turn (Figure t DDown c) = Figure t DLeft c
turn (Figure t DLeft c) = Figure t DUp c



figureToDraw::Figure->BlockedFigure
figureToDraw (Figure O d c) = figureToDrawO (Figure O d c)
figureToDraw (Figure I d c) = figureToDrawI (Figure I d c)
figureToDraw (Figure T d c) = figureToDrawT (Figure T d c)
figureToDraw (Figure J d c) = figureToDrawJ (Figure J d c)
figureToDraw (Figure L d c) = figureToDrawL (Figure L d c)
figureToDraw (Figure S d c) = figureToDrawS (Figure S d c)
figureToDraw (Figure Z d c) = figureToDrawZ (Figure Z d c)


figureToDrawO::Figure -> BlockedFigure
figureToDrawO (Figure O _ (x, y)) = ((x, y), (x+1, y), (x, y-1), (x+1, y-1))


figureToDrawI::Figure -> BlockedFigure
figureToDrawI (Figure I d (x, y)) | (d == DUp) || (d == DDown) = ((x, y+1), (x, y), (x, y-1), (x, y-2))
				 				  | otherwise = ((x-1, y), (x, y), (x+1, y), (x+2, y))

figureToDrawZ::Figure -> BlockedFigure
figureToDrawZ (Figure Z d (x, y)) | (d == DUp) || (d == DDown) = ((x-1, y+1), (x-1, y), (x, y), (x, y-1))
				  				  | otherwise = ((x-1, y), (x, y), (x, y+1), (x+1, y+1))

figureToDrawS::Figure -> BlockedFigure
figureToDrawS (Figure S d (x, y)) | (d == DUp) || (d == DDown) = ((x, y-1), (x, y), (x+1, y), (x+1, y+1))
				  				  | otherwise = ((x-1, y), (x, y), (x, y-1), (x+1, y-1))


figureToDrawJ::Figure -> BlockedFigure
figureToDrawJ (Figure J d (x,y)) | d == DDown = ((x, y+1), (x, y), (x, y-1), (x-1, y-1))
								 | d == DUp = ((x, y-1), (x, y), (x, y+1), (x+1, y+1))
								 | d == DRight = ((x-1, y), (x, y), (x+1, y), (x+1, y-1))
								 | otherwise = ((x-1, y+1), (x-1, y), (x, y), (x+1, y))


figureToDrawL::Figure -> BlockedFigure
figureToDrawL (Figure L d (x,y)) | d == DDown = ((x, y+1), (x, y), (x, y-1), (x+1, y-1))
								 | d == DUp = ((x, y-1), (x, y), (x, y+1), (x-1, y+1))
								 | d == DRight = ((x-1, y), (x, y), (x+1, y), (x+1, y+1))
								 | otherwise = ((x-1, y-1), (x-1, y), (x, y), (x+1, y))

figureToDrawT::Figure -> BlockedFigure
figureToDrawT (Figure T d (x,y)) | d == DDown = ((x-1, y), (x, y), (x+1, y), (x, y-1))
								 | d == DUp = ((x-1, y), (x, y), (x+1, y), (x, y+1))
								 | d == DRight = ((x, y+1), (x, y), (x, y-1), (x+1, y))
								 | otherwise = ((x, y+1), (x, y), (x, y-1), (x-1, y))

--Принимает пустую доску, моделирует всю игру, после
--окончания возвращает счет
startGame::Board -> Score
startGame  _ =  0
--Переещает фигуру влево	
moveLeft::Figure -> Figure
moveLeft _ =  Figure O DUp (0,0)
--Перемещает фигуру вправо
moveRight::Figure -> Figure
moveRight _ =  Figure O DUp (0,0)
--При нажатии клавиши "вниз" роняет фигуру 
drop::Gamestate -> Gamestate
drop  _ =  ([[Free]],Figure O DUp (0,0),0,0)



-- =========================================
-- Checking rows and deleting
-- =========================================




--Смотрит, нет ли строк, которые можно удалить
checkRowsToDelete::Board -> [Bool]
checkRowsToDelete _ =  [False]
--Смотрит, можно ли удаоить строку
checkRow::Row -> Bool
checkRow _ =  False
--Удаляет строку
deleteRow::Int -> Board -> Board
deleteRow _ _=  [[Free]]
--проверяет, конец игру т.е. приземлилась ли фигура до появления на
--экране, т.е. конец игры
gameover :: Gamestate -> Bool
gameover _ =  False



-- =========================================
-- Drawing
-- =========================================





--Рисуем доску
drawBoard::Board  -> Picture
drawBoard _ =  translate (-w) h (scale 30 30 (pictures
  [ color white (polygon [ (0, 0), (0, -2), (6, -2), (6, 0) ])            -- белая рамка
  , color black (polygon [ (0, 0), (0, -1.9), (5.9, -1.9), (5.9, 0) ])    -- чёрные внутренности
  , translate 2 (-1.5) (scale 0.01 0.01 (color red (text (show 0))))  -- красный счёт
  ]))
  where
    w = fromIntegral screenWidth  / 2
    h = fromIntegral screenHeight / 2
--Рисуем фигуру
drawFigure::Figure  ->  Picture
drawFigure _ = translate (-w) h (scale 30 30 (pictures
  [ color white (polygon [ (0, 0), (0, -2), (6, -2), (6, 0) ])            -- белая рамка
  , color black (polygon [ (0, 0), (0, -1.9), (5.9, -1.9), (5.9, 0) ])    -- чёрные внутренности
  , translate 2 (-1.5) (scale 0.01 0.01 (color red (text (show 0))))  -- красный счёт
  ]))
  where
    w = fromIntegral screenWidth  / 2
    h = fromIntegral screenHeight / 2
--Рисуем тетрис
drawTetris ::Gamestate-> Picture
drawTetris _ =  translate (-w) h (scale 30 30 (pictures
  [ color white (polygon [ (0, 0), (0, -2), (6, -2), (6, 0) ])            -- белая рамка
  , color black (polygon [ (0, 0), (0, -1.9), (5.9, -1.9), (5.9, 0) ])    -- чёрные внутренности
  , translate 2 (-1.5) (scale 0.01 0.01 (color red (text (show 0))))  -- красный счёт
  ]))
  where
    w = fromIntegral screenWidth  / 2
    h = fromIntegral screenHeight / 2
--Рисуем блок
drawBlock :: Block-> Picture
drawBlock _ =  translate (-w) h (scale 30 30 (pictures
  [ color white (polygon [ (0, 0), (0, -2), (6, -2), (6, 0) ])            -- белая рамка
  , color black (polygon [ (0, 0), (0, -1.9), (5.9, -1.9), (5.9, 0) ])    -- чёрные внутренности
  , translate 2 (-1.5) (scale 0.01 0.01 (color red (text (show 0))))  -- красный счёт
  ]))
  where
    w = fromIntegral screenWidth  / 2
    h = fromIntegral screenHeight / 2


-- =========================================
-- Updating
-- =========================================


--Проверяет, достигла ли нижняя часть фигуры нижней 
--границы доски или другой фигуры: делаем xor доски и фигуры,
--если количество свободных блоков одно и то же, то не достигла, иначе
--достигла
collidesFloor::Gamestate -> Bool
collidesFloor _ =  False
--Проверяет, не выходит ли правая или левая часть фигуры за правую или
-- левую часть доски соответственно
collidesSide::Gamestate -> Bool
collidesSide _ =  False
--Делает пустые блоки доски, на в которых находится фигура заполненными,
--вызываем ее после падения фигуры
updateBoard::Figure -> Board ->Board
updateBoard _ _ =  [[Free]]
--На основании прошедшего времени меняет скорость полета фигур
updateSpeed::Time -> Speed -> Speed
updateSpeed _ _ = 0
--Аргумент функции play, обновляет состояние тетриса
updateTetris :: Float -> Board -> Board
updateTetris _ _ =  [[Free]]

--Обновить весь тетрис
updateTheWholeTetris:: Time -> Speed -> Gamestate -> Gamestate
updateTheWholeTetris _ _ _ =  ([[Free]],Figure O DUp (0,0),0,0)
-- ===========================================
-- timing
-- =======================================





--Обновляет общее состояние тетриса
newTact::Figure -> Board -> Speed -> Gamestate
newTact _ _ _ =  ([[Free]],Figure O DUp (0,0),0,0)

--Застявляет фигуру постоянно падать, вызываем эту фунцию на каждом такте
newMove::Board -> Gamestate
newMove _ =  ([[Free]],Figure O DUp (0,0),0,0)


--Аргумент функции play, которя говорит, что длает каждая клавиша
handleTetris :: Event -> Gamestate -> Gamestate
handleTetris (EventKey (SpecialKey KeyRight) Down _ _) _ = ([[Free]],Figure O DUp (0,0),0,0)
handleTetris (EventKey (SpecialKey KeyLeft) Down _ _)  _ = ([[Free]],Figure O DUp (0,0),0,0)
handleTetris(EventKey (SpecialKey KeyDown) Down _ _ ) _ = ([[Free]],Figure O DUp (0,0),0,0)
handleTetris (EventKey (SpecialKey KeyUp) Down _ _ ) _ = ([[Free]],Figure O DUp (0,0),0,0)