module Tetris where

import System.Random
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Geometry.Line
import Graphics.Gloss.Interface.Pure.Game
import GHC.Float

glob_fps = 60

run :: IO ()

run = do
 g <- newStdGen
 play display bgColor fps (genUniverse g ) drawTetris handleTetris updateTetris
   where
    display = InWindow "Tetris" (screenWidth, screenHeight) (200, 200)
    bgColor = black   -- цвет фона
    fps     = glob_fps   -- кол-во кадров в секунду



-- =========================================
-- Types
-- =========================================



blockSize :: Int
blockSize = 30

init_tact::Time
init_tact = 0.7

                               --data Shape = J | L | I | S | Z | O | T
                               --         deriving (Eq, Show, Enum)
--Клетка заполнена?
--data Block = Free | Full
  --       deriving(Eq, Show)

--Строки нашей доски
--type Row = [Block]

--Доска - упавшие блоки
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
------------------------------------------------------------------------------------------------------------------------------------------

-- | Инициализировать случайный бесконечный
-- список чисел от 0 до 6 которые соответствуют фигурам
initFigures :: StdGen -> [Figure]
initFigures g = map genFigure
  (randomRs getrange g)

-- диапазон генерации случайных чисел
getrange :: (Int, Int)
getrange = (0, 6)
  


--Заполняем доску пустыми значениями и генерируем бесконечное количество фигур


fff :: Int -> [Coord]
fff 0 = []
fff hei = [(bs*10,bs*hei,0),(bs*11,bs*hei,0),(bs*12,bs*hei,0),(bs*13,bs*hei,0),(bs*14,bs*hei,0),(bs*15,bs*hei,0),
  (-1*bs,bs*hei,0),(-2*bs,bs*hei,0),(-3*bs,bs*hei,0),(-4*bs,bs*hei,0),(-5*bs,bs*hei,0),(-6*bs,bs*hei,0)] ++ (fff (hei-1))
    where bs = blockSize


genEmptyBoard::Board
--genEmptyBoard = []
genEmptyBoard = [(bs*9,bs*20,0),(bs*8,bs*20,0),(bs*7,bs*20,0),(bs*6,bs*20,0),(bs*5,bs*20,0),(bs*4,bs*20,0),
  (3*bs,bs*20,0),(2*bs,bs*20,0),(1*bs,bs*20,0)]
   where bs = blockSize
--genEmptyBoard = [(bs*9,bs*20,0),(bs*8,bs*20,0),(bs*7,bs*20,0),(bs*6,bs*20,0),(bs*5,bs*20,0),(bs*4,bs*20,0),
  --(3*bs,bs*20,0),(2*bs,bs*20,0),(1*bs,bs*20,0)]  ++ (fff 20)
   
--genRows::Int->Int->[Row]
--genRows _ 0 = []
--genRows w h = (genRows w (h-1)) ++ [genRow w]


--genRow::Int->Row
--genRow 0 = []
--genRow w = (genRow (w-1)) ++ [Free]


genUniverse::StdGen -> Gamestate
genUniverse g = (genEmptyBoard,initFigures g,(init_tact, 0),0)


-------------------------------------------------------------------------------------------------------------------------------------
--Генерируем бесконечный список из случайных фигур
-- == initFigures
--generateRandomFigureList:: StdGen -> [Figure]
--generateRandomFigureList _ =  [Figure O DUp (0,0,0)]



-- =========================================
-- Moves
-- =========================================

--Поворачивает фигуру: положение фигуры в пространстве опредляется 
--двумя числами, функция смотрит, какая ей дана фигура, и вычисляет 
--расстояние до края доски и на основании этой информации поворачивает ее
--(если это можно сделать), т.е. изменяет 3 координату

type BlockedFigure = (Coord, Coord, Coord, Coord)


turn::Gamestate -> Gamestate
turn (a,(Figure t DUp c):rest,d,e) | collide1 = (a,(Figure t DUp c):rest,d,e)
                                   | otherwise = (a,(Figure t DRight c):rest,d,e)
                            where 
                                collide1 = collidesFigure (figureToDraw (Figure t DRight c)) a
turn (a,(Figure t DRight c):rest,d,e) | collide2 = (a,(Figure t DRight c):rest,d,e)
                                      | otherwise = (a,(Figure t DDown c):rest,d,e)
                            where 
                                collide2 = collidesFigure (figureToDraw (Figure t DDown c)) a
turn (a,(Figure t DDown c):rest,d,e) | collide3 = (a,(Figure t DDown c):rest,d,e)
                                     | otherwise = (a,(Figure t DLeft c):rest,d,e)
                            where 
                                collide3 = collidesFigure (figureToDraw (Figure t DLeft c)) a
turn (a,(Figure t DLeft c):rest,d,e) | collide4 = (a,(Figure t DLeft c):rest,d,e)
                                     | otherwise = (a,(Figure t DUp c):rest,d,e)
                            where 
                                collide4 = collidesFigure (figureToDraw (Figure t DUp c)) a

figureToDraw::Figure->BlockedFigure
figureToDraw (Figure O d c) = figureToDrawO (Figure O d c)
figureToDraw (Figure I d c) = figureToDrawI (Figure I d c)
figureToDraw (Figure T d c) = figureToDrawT (Figure T d c)
figureToDraw (Figure J d c) = figureToDrawJ (Figure J d c)
figureToDraw (Figure L d c) = figureToDrawL (Figure L d c)
figureToDraw (Figure S d c) = figureToDrawS (Figure S d c)
figureToDraw (Figure Z d c) = figureToDrawZ (Figure Z d c)


figureToDrawO::Figure -> BlockedFigure
figureToDrawO (Figure O _ (x, y,z)) = ((x, y,z), (x+blockSize, y,z), (x, y-blockSize,z), (x+blockSize, y-blockSize,z))


figureToDrawI::Figure -> BlockedFigure
figureToDrawI (Figure I d (x, y,z)) | (d == DUp) || (d == DDown) = ((x, y+blockSize,z), (x, y,z), (x, y-blockSize,z), (x, y-2*blockSize,z))
                  | otherwise = ((x-blockSize, y,z), (x, y,z), (x+blockSize, y,z), (x+2*blockSize, y,z))

figureToDrawZ::Figure -> BlockedFigure
figureToDrawZ (Figure Z d (x, y,z)) | (d == DUp) || (d == DDown) = ((x-blockSize, y-blockSize,z), (x-blockSize, y,z), (x, y,z), (x, y+blockSize,z))
                    | otherwise = ((x-blockSize, y,z), (x, y,z), (x, y-blockSize,z), (x+blockSize, y-blockSize,z))

figureToDrawS::Figure -> BlockedFigure
figureToDrawS (Figure S d (x, y,z)) | (d == DUp) || (d == DDown) = ((x-blockSize, y+blockSize,z), (x-blockSize, y,z), (x, y,z), (x, y-blockSize,z))
                    | otherwise = ((x-blockSize, y,z), (x, y,z), (x, y+blockSize,z), (x+blockSize, y+blockSize,z))


figureToDrawJ::Figure -> BlockedFigure
figureToDrawJ (Figure J d (x,y,z)) | d == DDown = ((x-blockSize, y-blockSize,z), (x, y-blockSize,z), (x, y,z), (x, y+blockSize,z))
                 | d == DUp = ((x, y-blockSize,z), (x, y,z), (x, y+blockSize,z), (x+blockSize, y+blockSize,z))
                 | d == DRight = ((x-blockSize, y,z), (x, y,z), (x+blockSize, y,z), (x+blockSize, y-blockSize,z))
                 | otherwise = ((x-blockSize, y+blockSize,z), (x-blockSize, y,z), (x, y,z), (x+blockSize, y,z))


figureToDrawL::Figure -> BlockedFigure
figureToDrawL (Figure L d (x,y,z)) | d == DDown = ((x, y+blockSize,z), (x, y,z), (x, y-blockSize,z), (x+blockSize, y-blockSize,z))
                 | d == DUp = ((x, y-blockSize,z), (x, y,z), (x, y+blockSize,z), (x-blockSize, y+blockSize,z))
                 | d == DRight = ((x-blockSize, y,z), (x, y,z), (x+blockSize, y,z), (x+blockSize, y+blockSize,z))
                 | otherwise = ((x-blockSize, y-blockSize,z), (x-blockSize, y,z), (x, y,z), (x+blockSize, y,z))

figureToDrawT::Figure -> BlockedFigure
figureToDrawT (Figure T d (x,y,z)) | d == DDown = ((x-blockSize, y,z), (x, y,z), (x+blockSize, y,z), (x, y-blockSize,z))
                 | d == DUp = ((x-blockSize, y,z), (x, y,z), (x+blockSize, y,z), (x, y+blockSize,z))
                 | d == DRight = ((x, y+blockSize,z), (x, y,z), (x, y-blockSize,z), (x+blockSize, y,z))
                 | otherwise = ((x, y+blockSize,z), (x, y,z), (x, y-blockSize,z), (x-blockSize, y,z))

--Принимает пустую доску, моделирует всю игру, после
--окончания возвращает счет
startGame::Board -> Score
startGame  _ =  0
--Переещает фигуру влево  



moveLeft::Gamestate -> Gamestate
moveLeft (a,((Figure s t (b,c,z)):rest),d,e) | collide = (a, ((Figure s t (b,c,z)):rest),d,e)
        |otherwise = (a, ((Figure s t (b - blockSize,c,z)):rest),d,e)
  where 
    collide = collidesFigureSides (figureToDraw (Figure s t (b - blockSize,c,z))) a

moveRight::Gamestate -> Gamestate
moveRight (a,(Figure s t (b,c,z)):rest,d,e) | collide = (a, ((Figure s t (b,c,z)):rest),d,e)
        |otherwise = (a, ((Figure s t (b + blockSize,c,z)):rest),d,e)
  where 
    collide = collidesFigureSides (figureToDraw (Figure s t (b + blockSize,c,z))) a


collidesBlock::Coord -> Bool
collidesBlock (a,b,z) | (a < 0) || (a  + blockSize > screenWidth) || (b < 0) || (b + blockSize > screenHeight) = True
       |otherwise = False


collidesBlockSides::Coord -> Board -> Bool
collidesBlockSides (a,b,z) [] = (a < 0) || (a  + blockSize > screenWidth)
collidesBlockSides (a,b,z) ((brda, brdb,z1):[]) = (a < 0) || (a  + blockSize > screenWidth) || (a==brda) && (b==brdb)
collidesBlockSides (a,b,z) ((brda, brdb,z1):brds) | (a < 0) || (a  + blockSize > screenWidth) || (a==brda) && (b==brdb)  = True
                                             | otherwise = collidesBlockSides (a,b,z) brds


collidesBlockDown::Coord -> Board-> Bool
collidesBlockDown (a,b,z) []  =   (b + blockSize > screenHeight)
collidesBlockDown (a,b,z) ((brda,brdb,z1):[])  =   ((b + blockSize > screenHeight) || (a==brda) && (b==brdb))
collidesBlockDown (a,b,z) ((brda,brdb,z1):brds)  | (b + blockSize > screenHeight) || (a==brda) && (b==brdb)  = True
                                            |  otherwise = collidesBlockDown (a,b,z) brds



collidesBlockUp::Coord -> Board-> Bool
collidesBlockUp (a,b,z) []  =  b < 0
collidesBlockUp (a,b,z) ((brda,brdb,z1):[])  =   (b < 0 && (b==brdb))
collidesBlockUp (a,b,z) ((brda,brdb,z1):brds)  | b < 0 && (b==brdb)  = True
                                          |  otherwise = collidesBlockUp (a,b,z) brds


collidesFigure::BlockedFigure -> Board -> Bool
collidesFigure (a,b,c,d) board = (collidesFigureSides (a,b,c,d) board) || (collidesFigureDown (a,b,c,d) board)

collidesFigureSides::BlockedFigure -> Board -> Bool
collidesFigureSides (a,b,c,d) board | (collidesBlockSides a board) || (collidesBlockSides b board) || (collidesBlockSides c board) || (collidesBlockSides d board) = True
        |otherwise = False


collidesFigureDown::BlockedFigure -> Board -> Bool
collidesFigureDown (a,b,c,d) board | (collidesBlockDown a board) || (collidesBlockDown b board) || (collidesBlockDown c board) || (collidesBlockDown d board) = True
        |otherwise = False


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


dropit::Gamestate -> Int -> Gamestate
dropit (a,((Figure sha dir (b,c,z)):rest),d,e) pts  | collide = (a,((Figure sha dir (b,c,z)):rest),d,e+(div pts blockSize))                   
                                                  | otherwise = dropit (a,((Figure sha dir (b,c + blockSize,z)):rest),d,e) pts                                        
                                          where                                           
                                              collide = collidesFigureDown (figureToDraw (Figure sha dir (b,c + blockSize,z))) a


drawBoard::Board  -> Picture
drawBoard s = pictures (map drawBlock s)

drawBlock :: Coord-> Picture

drawBlock  (b,c,1) =  pictures [ translate (-w) h (scale  1 1 (pictures
 [ color blue  (polygon [ ( fromIntegral b, fromIntegral (-c)), (fromIntegral b, fromIntegral (-c - 30)), (fromIntegral  (b + 30),fromIntegral (-c - 30)), (fromIntegral  (b + 30),fromIntegral (- c)) ])            -- белая рамка
   ,color magenta  (polygon [ ( fromIntegral b, fromIntegral (-c)), (fromIntegral b, fromIntegral (-c - 2)), (fromIntegral  (b + 30),fromIntegral (-c - 2)), (fromIntegral  (b + 30),fromIntegral (- c)) ])
   ,color magenta  (polygon [ ( fromIntegral b, fromIntegral (-c)), (fromIntegral b, fromIntegral (-c - 30)), (fromIntegral  (b + 2),fromIntegral (-c-30 )), (fromIntegral  (b +2),fromIntegral (- c)) ])
   ,color magenta  (polygon [ ( fromIntegral b, fromIntegral (-c-28)), (fromIntegral b, fromIntegral (-c - 30)), (fromIntegral  (b + 30),fromIntegral (-c - 30)), (fromIntegral  (b + 30),fromIntegral (- c-28)) ])
   ,color magenta  (polygon [ ( fromIntegral b+28, fromIntegral (-c)), (fromIntegral b+28, fromIntegral (-c - 30)), (fromIntegral  (b + 30),fromIntegral (-c - 30)), (fromIntegral  (b + 30),fromIntegral (- c)) ])

   ]))
    ]
  where
  w = fromIntegral screenWidth  / 2
  h = fromIntegral screenHeight / 2
drawBlock  (b,c,2) =  pictures [ translate (-w) h (scale  1 1 (pictures
 [ color yellow  (polygon [ ( fromIntegral b, fromIntegral (-c)), (fromIntegral b, fromIntegral (-c - 30)), (fromIntegral  (b + 30),fromIntegral (-c - 30)), (fromIntegral  (b + 30),fromIntegral (- c)) ])            -- белая рамка
   ,color magenta  (polygon [ ( fromIntegral b, fromIntegral (-c)), (fromIntegral b, fromIntegral (-c - 2)), (fromIntegral  (b + 30),fromIntegral (-c - 2)), (fromIntegral  (b + 30),fromIntegral (- c)) ])
   ,color magenta  (polygon [ ( fromIntegral b, fromIntegral (-c)), (fromIntegral b, fromIntegral (-c - 30)), (fromIntegral  (b + 2),fromIntegral (-c-30 )), (fromIntegral  (b +2),fromIntegral (- c)) ])
   ,color magenta  (polygon [ ( fromIntegral b, fromIntegral (-c-28)), (fromIntegral b, fromIntegral (-c - 30)), (fromIntegral  (b + 30),fromIntegral (-c - 30)), (fromIntegral  (b + 30),fromIntegral (- c-28)) ])
   ,color magenta  (polygon [ ( fromIntegral b+28, fromIntegral (-c)), (fromIntegral b+28, fromIntegral (-c - 30)), (fromIntegral  (b + 30),fromIntegral (-c - 30)), (fromIntegral  (b + 30),fromIntegral (- c)) ])
   ]))]
  where
  w = fromIntegral screenWidth  / 2
  h = fromIntegral screenHeight / 2
drawBlock  (b,c,3) =  pictures [ translate (-w) h (scale  1 1 (pictures
 [ color red  (polygon [ ( fromIntegral b, fromIntegral (-c)), (fromIntegral b, fromIntegral (-c - 30)), (fromIntegral  (b + 30),fromIntegral (-c - 30)), (fromIntegral  (b + 30),fromIntegral (- c)) ])            -- белая рамка
   ,color magenta  (polygon [ ( fromIntegral b, fromIntegral (-c)), (fromIntegral b, fromIntegral (-c - 2)), (fromIntegral  (b + 30),fromIntegral (-c - 2)), (fromIntegral  (b + 30),fromIntegral (- c)) ])
   ,color magenta  (polygon [ ( fromIntegral b, fromIntegral (-c)), (fromIntegral b, fromIntegral (-c - 30)), (fromIntegral  (b + 2),fromIntegral (-c-30 )), (fromIntegral  (b +2),fromIntegral (- c)) ])
   ,color magenta  (polygon [ ( fromIntegral b, fromIntegral (-c-28)), (fromIntegral b, fromIntegral (-c - 30)), (fromIntegral  (b + 30),fromIntegral (-c - 30)), (fromIntegral  (b + 30),fromIntegral (- c-28)) ])
   ,color magenta  (polygon [ ( fromIntegral b+28, fromIntegral (-c)), (fromIntegral b+28, fromIntegral (-c - 30)), (fromIntegral  (b + 30),fromIntegral (-c - 30)), (fromIntegral  (b + 30),fromIntegral (- c)) ])
   ]))]
  where
  w = fromIntegral screenWidth  / 2
  h = fromIntegral screenHeight / 2
drawBlock  (b,c,4) =  pictures [ translate (-w) h (scale  1 1 (pictures
 [ color green  (polygon [ ( fromIntegral b, fromIntegral (-c)), (fromIntegral b, fromIntegral (-c - 30)), (fromIntegral  (b + 30),fromIntegral (-c - 30)), (fromIntegral  (b + 30),fromIntegral (- c)) ])            -- белая рамка
   ,color magenta  (polygon [ ( fromIntegral b, fromIntegral (-c)), (fromIntegral b, fromIntegral (-c - 2)), (fromIntegral  (b + 30),fromIntegral (-c - 2)), (fromIntegral  (b + 30),fromIntegral (- c)) ])
   ,color magenta  (polygon [ ( fromIntegral b, fromIntegral (-c)), (fromIntegral b, fromIntegral (-c - 30)), (fromIntegral  (b + 2),fromIntegral (-c-30 )), (fromIntegral  (b +2),fromIntegral (- c)) ])
   ,color magenta  (polygon [ ( fromIntegral b, fromIntegral (-c-28)), (fromIntegral b, fromIntegral (-c - 30)), (fromIntegral  (b + 30),fromIntegral (-c - 30)), (fromIntegral  (b + 30),fromIntegral (- c-28)) ])
   ,color magenta  (polygon [ ( fromIntegral b+28, fromIntegral (-c)), (fromIntegral b+28, fromIntegral (-c - 30)), (fromIntegral  (b + 30),fromIntegral (-c - 30)), (fromIntegral  (b + 30),fromIntegral (- c)) ])
   ]))]
  where
  w = fromIntegral screenWidth  / 2
  h = fromIntegral screenHeight / 2
drawBlock  (b,c,5) =  pictures [ translate (-w) h (scale  1 1 (pictures
 [ color orange  (polygon [ ( fromIntegral b, fromIntegral (-c)), (fromIntegral b, fromIntegral (-c - 30)), (fromIntegral  (b + 30),fromIntegral (-c - 30)), (fromIntegral  (b + 30),fromIntegral (- c)) ])            -- белая рамка
   ,color magenta  (polygon [ ( fromIntegral b, fromIntegral (-c)), (fromIntegral b, fromIntegral (-c - 2)), (fromIntegral  (b + 30),fromIntegral (-c - 2)), (fromIntegral  (b + 30),fromIntegral (- c)) ])
   ,color magenta  (polygon [ ( fromIntegral b, fromIntegral (-c)), (fromIntegral b, fromIntegral (-c - 30)), (fromIntegral  (b + 2),fromIntegral (-c-30 )), (fromIntegral  (b +2),fromIntegral (- c)) ])
   ,color magenta  (polygon [ ( fromIntegral b, fromIntegral (-c-28)), (fromIntegral b, fromIntegral (-c - 30)), (fromIntegral  (b + 30),fromIntegral (-c - 30)), (fromIntegral  (b + 30),fromIntegral (- c-28)) ])
   ,color magenta  (polygon [ ( fromIntegral b+28, fromIntegral (-c)), (fromIntegral b+28, fromIntegral (-c - 30)), (fromIntegral  (b + 30),fromIntegral (-c - 30)), (fromIntegral  (b + 30),fromIntegral (- c)) ])
   ]))]
  where
  w = fromIntegral screenWidth  / 2
  h = fromIntegral screenHeight / 2


drawBlock  (b,c,_) =  pictures [ translate (-w) h (scale  1 1 (pictures
 [ color white  (polygon [ ( fromIntegral b, fromIntegral (-c)), (fromIntegral b, fromIntegral (-c - 30)), (fromIntegral  (b + 30),fromIntegral (-c - 30)), (fromIntegral  (b + 30),fromIntegral (- c)) ])            -- белая рамка
   ,color magenta  (polygon [ ( fromIntegral b, fromIntegral (-c)), (fromIntegral b, fromIntegral (-c - 2)), (fromIntegral  (b + 30),fromIntegral (-c - 2)), (fromIntegral  (b + 30),fromIntegral (- c)) ])
   ,color magenta  (polygon [ ( fromIntegral b, fromIntegral (-c)), (fromIntegral b, fromIntegral (-c - 30)), (fromIntegral  (b + 2),fromIntegral (-c-30 )), (fromIntegral  (b +2),fromIntegral (- c)) ])
   ,color magenta  (polygon [ ( fromIntegral b, fromIntegral (-c-28)), (fromIntegral b, fromIntegral (-c - 30)), (fromIntegral  (b + 30),fromIntegral (-c - 30)), (fromIntegral  (b + 30),fromIntegral (- c-28)) ])
   ,color magenta  (polygon [ ( fromIntegral b+28, fromIntegral (-c)), (fromIntegral b+28, fromIntegral (-c - 30)), (fromIntegral  (b + 30),fromIntegral (-c - 30)), (fromIntegral  (b + 30),fromIntegral (- c)) ])
   ]))]
  where
  w = fromIntegral screenWidth  / 2
  h = fromIntegral screenHeight / 2

drawFigure::Gamestate  ->  Picture
drawFigure (b,(f:fs),s,t) = drawBlockedFigure  (figureToDraw f)

drawBlockedFigure::BlockedFigure -> Picture

 
drawBlockedFigure ((a, b, c, d)) =         pictures  [drawBlock   a ,
                                                     drawBlock    b ,
                                                     drawBlock     c ,
                                                     drawBlock     d ]

--Рисуем тетрис
--Пока только рисует квадрат
drawTetris ::Gamestate-> Picture
drawTetris (b,fs,s,t) = pictures
  [ drawFigure (b,fs,s,t),
   drawBoard b ,
    drawScore t
  ] 


drawScore :: Score -> Picture
drawScore score = translate (-w) h (scale 30 30 (pictures
  [ color yellow (polygon [ (0, 0), (0, -2), (6, -2), (6, 0) ])            -- белая рамка
  , color black (polygon [ (0, 0), (0, -1.9), (5.9, -1.9), (5.9, 0) ])    -- чёрные внутренности
  , translate 2 (-1.5) (scale 0.01 0.01 (color green (text (show score))))  -- красный счёт
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
--достигла Пока она реализована в updateTetris
collidesFloor::Gamestate -> Bool
collidesFloor _ =  False
--Проверяет, не выходит ли правая или левая часть фигуры за правую или
-- левую часть доски соответственно
--пока реализована в обраюотчиках клавиш
collidesSide::Gamestate -> Bool
collidesSide _ =  False
--Делает пустые блоки доски, на в которых находится фигура заполненными,
--вызываем ее после падения фигуры
vectolist :: (Coord, Coord, Coord, Coord) -> [Coord]
vectolist (a,b,c,d) = [a,b,c,d]

updateBoard::Gamestate ->Board
updateBoard (a,(Figure sha dir (b,c,cl):rest),(sp, ti),e) = a ++ vectolist (figureToDraw (Figure sha dir (b ,c,cl)))

--updateBoard::Figure -> Board ->Board
--updateBoard (Figure sha dir (b ,c,z)) a = a ++ vectolist (figureToDraw (Figure sha dir (b ,c,z)))


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
-- AI
-- =======================================
sortBoard :: Board -> Board
sortBoard []     = []
--sortBoard ((brda,brdb,z):brds) = sortRows (filter (\(x,y,z) -> y > brdb) brds) ++ [(brda,brdb,z)] ++ sortRows (filter (\(x,y,z) -> y <= brdb) brds)
sortBoard (brd:brds) = sortBoard (filter (\x -> greater x brd ) brds) ++ [brd] ++ sortBoard (filter (\x -> not (greater x brd)) brds)

greater:: Coord -> Coord -> Bool
greater (x1,y1,z1) (x2,y2,z2) | x1 > x2 = True
                              | (x1 == x2) && (y1 < y2) = True
                              | otherwise = False

topcells :: Board -> [Coord]
topcells [] = []
topcells ((x1,y1,z1):bs) =  (x1,y1,z1) : topcells (filter  (\(x,y,z) -> not (x == x1)) bs)

--Мы хотим максисизировать количество удаленных строк, минимизировать количество дырок, минимизировать высоту тетриса
--numberholes :: Board -> Int
--numberholes crds = sortBoard(crds) 

heightofboard :: Board -> Int
heightofboard brds  = minimum (map (\(x,y,z) -> y) brds)
   --                       --| (null brds) = 10
     --                     --|otherwise =  minimum (map (\(x,y,z) -> y) brds)

mymax :: (Int, Int, Int) -> (Int, Int, Int) -> (Int, Int, Int)
mymax (x1,y1,t1) (x2,y2,t2) | x1 >= x2 = (x1, y1, t1)
                      | otherwise = (x2, y2, t2)

--bestheight :: [(Int, Int, Int)] -> (Int,Int,Int)
--bestheight [] = (0, -1, 0)
--bestheight ((y,x,t):hs) = mymax  (bestheight hs) (y,x,t)

bestchoice :: [(Int, Int, Int)] -> (Int,Int,Int)
bestchoice [] = (0, -1, 0)
bestchoice ((y,x,t):hs) = mymax (y,x,t) (bestchoice hs) 

-- topcells (sortBoard)
-- (сколько поворотов, относительное смещение)
profitofboard :: Board -> Int
profitofboard b = 2000 + 10000 * (numberdeletes b) - 100 * (numberholes b) - ((avgheightofboard b) - (heightofboard b))

--figurefromgs brdfromgs
numberdeletes :: Board -> Int
numberdeletes b = (heightofboard (deleteRows (sortRows b))) - (heightofboard b)

avgheightofboard :: Board -> Int
avgheightofboard b = div (sumheightofboard b) 10

numcols :: Board ->  Int
numcols [] = 0
numcols ((x1,y1,z1):hs) = 1 +  sumhofb (filter (\(x,y,z) -> not (x == x1)) ((x1,y1,z1):hs))

sumheightofboard :: Board -> Int
sumheightofboard b = sumhofb (sortBoard b)

sumhofb :: Board -> Int
sumhofb [] = 0
sumhofb ((x1,y1,z1):hs) = y1  +  sumhofb (filter (\(x,y,z) -> not (x == x1)) ((x1,y1,z1):hs))

numberholes :: Board -> Int
numberholes b = nh (sortBoard b)

nh :: Board -> Int
nh [] = 0
nh ((x1,y1,z1):hs) = nhcolumn (filter (\(x,y,z) -> x == x1) ((x1,y1,z1):hs))  + nh (filter (\(x,y,z) -> not (x == x1)) ((x1,y1,z1):hs))

nhcolumn :: [Coord] -> Int
nhcolumn [] = 0
nhcolumn ((x1,y1,z1):hs) = (div (screenHeight - y1) blockSize) - length ((x1,y1,z1):hs)

--nhcolumn 



beststep :: Gamestate -> (Int, Int, Int)
beststep (b, (Figure sha dir (f1,f2,f3):rest), (sp, ti), s) =
   bestchoice ((profitofboard (updateBoard (dropit gs (screenHeight-f2))), 0, 0) :
    (profitofboard (updateBoard (dropit (moveLeft gs) (screenHeight-f2))) , -1, 0) :
    (profitofboard (updateBoard (dropit (moveRight gs) (screenHeight-f2))), 1, 0) :
    (profitofboard (updateBoard (dropit (moveLeft (moveLeft gs)) (screenHeight-f2))), -2, 0) :
    (profitofboard (updateBoard (dropit (moveRight (moveRight gs)) (screenHeight-f2))), 2, 0) :
    (profitofboard (updateBoard (dropit (moveLeft (moveLeft (moveLeft gs))) (screenHeight-f2))), -3, 0) :
    (profitofboard (updateBoard (dropit (moveRight (moveRight (moveRight gs))) (screenHeight-f2))), 3, 0) :
    (profitofboard (updateBoard (dropit (moveLeft (moveLeft (moveLeft (moveLeft gs)))) (screenHeight-f2))), -4, 0) :
    (profitofboard (updateBoard (dropit (moveRight (moveRight (moveRight (moveRight gs)))) (screenHeight-f2))), 4, 0) : 
    (profitofboard (updateBoard (dropit (moveLeft (moveLeft (moveLeft (moveLeft (moveLeft gs))))) (screenHeight-f2))), -5, 0) :

    (profitofboard (updateBoard (dropit gs (screenHeight-f2))), 0, 1) :
    (profitofboard (updateBoard (dropit (moveLeft (turn gs)) (screenHeight-f2))) , -1, 1) :
    (profitofboard (updateBoard (dropit (moveRight (turn gs)) (screenHeight-f2))), 1, 1) :
    (profitofboard (updateBoard (dropit (moveLeft (moveLeft (turn gs))) (screenHeight-f2))), -2, 1) :
    (profitofboard (updateBoard (dropit (moveRight (moveRight (turn gs))) (screenHeight-f2))), 2, 1) :
    (profitofboard (updateBoard (dropit (moveLeft (moveLeft (moveLeft (turn gs)))) (screenHeight-f2))), -3, 1) :
    (profitofboard (updateBoard (dropit (moveRight (moveRight (moveRight (turn gs)))) (screenHeight-f2))), 3, 1) :
    (profitofboard (updateBoard (dropit (moveLeft (moveLeft (moveLeft (moveLeft (turn gs))))) (screenHeight-f2))), -4, 1) :
    (profitofboard (updateBoard (dropit (moveRight (moveRight (moveRight (moveRight (turn gs))))) (screenHeight-f2))), 4, 1) : 
    (profitofboard (updateBoard (dropit (moveLeft (moveLeft (moveLeft (moveLeft (moveLeft (turn gs)))))) (screenHeight-f2))), -5, 1) :

    (profitofboard (updateBoard (dropit gs (screenHeight-f2))), 0, 2) :
    (profitofboard (updateBoard (dropit (moveLeft (turn (turn gs))) (screenHeight-f2))) , -1, 2) :
    (profitofboard (updateBoard (dropit (moveRight (turn (turn gs))) (screenHeight-f2))), 1, 2) :
    (profitofboard (updateBoard (dropit (moveLeft (moveLeft (turn (turn gs)))) (screenHeight-f2))), -2, 2) :
    (profitofboard (updateBoard (dropit (moveRight (moveRight (turn (turn gs)))) (screenHeight-f2))), 2, 2) :
    (profitofboard (updateBoard (dropit (moveLeft (moveLeft (moveLeft (turn (turn gs))))) (screenHeight-f2))), -3, 2) :
    (profitofboard (updateBoard (dropit (moveRight (moveRight (moveRight (turn (turn gs))))) (screenHeight-f2))), 3, 2) :
    (profitofboard (updateBoard (dropit (moveLeft (moveLeft (moveLeft (moveLeft (turn (turn gs)))))) (screenHeight-f2))), -4, 2) :
    (profitofboard (updateBoard (dropit (moveRight (moveRight (moveRight (moveRight (turn (turn gs)))))) (screenHeight-f2))), 4, 2) : 
    (profitofboard (updateBoard (dropit (moveLeft (moveLeft (moveLeft (moveLeft (moveLeft (turn (turn gs))))))) (screenHeight-f2))), -5, 2) :

    (profitofboard (updateBoard (dropit gs (screenHeight-f2))), 0, 3) :
    (profitofboard (updateBoard (dropit (moveLeft (turn (turn (turn gs)))) (screenHeight-f2))) , -1, 3) :
    (profitofboard (updateBoard (dropit (moveRight (turn (turn (turn gs)))) (screenHeight-f2))), 1, 3) :
    (profitofboard (updateBoard (dropit (moveLeft (moveLeft (turn (turn (turn gs))))) (screenHeight-f2))), -2, 3) :
    (profitofboard (updateBoard (dropit (moveRight (moveRight (turn (turn (turn gs))))) (screenHeight-f2))), 2, 3) :
    (profitofboard (updateBoard (dropit (moveLeft (moveLeft (moveLeft (turn (turn (turn gs)))))) (screenHeight-f2))), -3, 3) :
    (profitofboard (updateBoard (dropit (moveRight (moveRight (moveRight (turn (turn (turn gs)))))) (screenHeight-f2))), 3, 3) :
    (profitofboard (updateBoard (dropit (moveLeft (moveLeft (moveLeft (moveLeft (turn (turn (turn gs))))))) (screenHeight-f2))), -4, 3) :
    (profitofboard (updateBoard (dropit (moveRight (moveRight (moveRight (moveRight (turn (turn (turn gs))))))) (screenHeight-f2))), 4, 3) : 
    (profitofboard (updateBoard (dropit (moveLeft (moveLeft (moveLeft (moveLeft (moveLeft (turn (turn (turn gs)))))))) (screenHeight-f2))), -5, 3) :

    [])
      where
        gs = (b, (Figure sha dir (f1,f2,f3):rest), (sp, ti), s) 

makestep:: Gamestate -> Gamestate 
makestep (b, (Figure sha dir (f1,f2,f3):rest), (sp, ti), s)
  | needturn = turn (b, (Figure sha dir (f1,f2,f3):rest), (sp, ti), s)  
  | needleft = moveLeft (b, (Figure sha dir (f1,f2,f3):rest), (sp, ti), s)  
  | needright = moveRight (b, (Figure sha dir (f1,f2,f3):rest), (sp, ti), s)  
  | otherwise   = dropit (b, (Figure sha dir (f1,f2,f3):rest), (sp, ti), s) (screenHeight-f2)
    where
      needturn = (\(x,y,t) -> t) (beststep (b, (Figure sha dir (f1,f2,f3):rest), (sp, ti), s)) > 0
      needleft = (\(x,y,t) -> y) (beststep (b, (Figure sha dir (f1,f2,f3):rest), (sp, ti), s)) < 0
      needright = (\(x,y,t) -> y) (beststep (b, (Figure sha dir (f1,f2,f3):rest), (sp, ti), s)) > 0


-- ===========================================
-- timing
-- =======================================


newTact::Gamestate -> Float -> Float -> Gamestate
newTact (b, (Figure sha dir (f1,f2,f3):rest), (sp, ti), s) dt tact
  | paused = (b, (Figure sha dir (f1,f2,f3):rest), (sp, ti), s)
  | new && collides = (deleteRows (sortRows (updateBoard (b, (Figure sha dir (f1,f2,f3):rest), (sp, ti), s))), rest, (sp, ti), s + 1)
  | new = newTact (makestep(makestep(makestep(makestep (b, (Figure sha dir (f1,f2 + blockSize,f3):rest), (sp, 0), s))))) (dt + ti - tact) tact
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

handleTetris :: Event -> Gamestate -> Gamestate

handleTetris (EventKey (Char 'l') Down _ _) (a,(Figure sha dir (b,c,z):rest),d,e) = moveRight (a,(Figure sha dir (b,c,z):rest),d,e)
handleTetris (EventKey (Char 'l') Up _ _) t = t

handleTetris (EventKey (Char 'j') Down _ _)  (a,(Figure sha dir (b,c,z):rest),d,e)  = moveLeft (a,(Figure sha dir (b,c,z):rest),d,e)
handleTetris (EventKey (Char 'j') Up _ _)  t  = t

handleTetris(EventKey (SpecialKey KeySpace) Down _ _ ) (a,(Figure sha dir (b,c,z):rest),d,e)  = dropit (a,(Figure sha dir (b,c,z):rest),d,e) (screenHeight-c)
handleTetris(EventKey (SpecialKey KeySpace) Up _ _ ) t = t

handleTetris (EventKey (Char 'k') Down _ _ ) (a,(Figure sha dir (b,c,z):rest),d,e) = turn (a, (Figure sha dir (b ,c,z):rest),d,e)
handleTetris (EventKey (Char 'k') Up _ _ ) t = t

handleTetris (EventKey (Char 'p') Down _ _ ) (a,(Figure sha dir (b,c,z):rest),(sp, ti),e) = (a,(Figure sha dir (b,c,z):rest),(- sp, ti),e)
handleTetris (EventKey (Char 'p') Up _ _ ) t = t

handleTetris  _ t = t  
--------------
