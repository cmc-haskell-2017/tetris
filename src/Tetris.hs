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
-- * Типы
-- =========================================


-- | Размер блока
blockSize :: Int
blockSize = 30

-- | Инициализация такта
init_tact::Time
init_tact = 0.7


-- | Доска - список координат упавших блоков
type Board = [Coord]

-- | Вариант развития событий для хода ИИ (профит, смещение, количество поворотов)
type Variant = (Int, Int, Int)

-- | Счет
type Score = Int

-- | Coord - (x,y,clr). Координаты блока x, y и его цвет clr
type Coord = (Int, Int, Int)

-- | Время
type Time = Float


-- | Состояние игры в текущий момент(разделили доску и фигуру, чтобы при полете фигуры не мигала вся доска, также, чтобы было более оптимизировано) 
--   [Figure] - бесконечный список фигур, в текущем состоянии берем первый элемент списка
type Gamestate = (Board,  [Figure], (Speed, Time), Score)

------------------------------------------------------------------------------------------------------------------------------------------------------------

-- | Скорость
type Speed = Float

-- | Для каждой фигуры свой тип, чтобы однозначно можно было  определить ее и тип операций над ней, например, фигуру I можно вращать произвольно только на расстоянии больше 4 клеток от края, а фигуру O на расстоянии больше 2 клеток от края

-- | Тип фигуры соответствует букве на которую фигура похожа
data FigureType = O | I | T | J | L | S | Z
                      deriving(Eq, Show)

-- | Направление фигуры
data Direction = DUp    -- ^ Фигура направелена вверх
               | DDown  -- ^ Фигура направелена вниз
               | DLeft  -- ^ Фигура направелена влево
               | DRight -- ^ Фигура направелена вправо
                      deriving(Eq, Show)
-- | Фигура определяется типом, направлением, координатами верхнего левого блока
data Figure = Figure FigureType Direction Coord 
                      deriving(Eq, Show)


-- | Ширина экрана.
screenWidth :: Int
screenWidth = 300

-- | Высота экрана.
screenHeight :: Int
screenHeight = 600

-- =========================================
-- * Generating
-- =========================================


-- | На вход принимается случайное число от 0 до 6, которое определяет фигуру
genFigure::Int -> Figure
genFigure a | a== 0  =  Figure O DUp (div screenWidth 2, blockSize * 2,0) 
            | a== 1  =  Figure I DUp (div screenWidth 2, blockSize * 2,1) 
            | a== 2  =  Figure T DUp (div screenWidth 2, blockSize * 2,2) 
            | a== 3  =  Figure J DUp (div screenWidth 2, blockSize * 2,3) 
            | a== 4  =  Figure L DUp (div screenWidth 2, blockSize * 2,4) 
            | a== 5  =  Figure S DUp (div screenWidth 2, blockSize * 2,5) 
            | a== 6  =  Figure Z DUp (div screenWidth 2, blockSize * 2,6) 
------------------------------------------------------------------------------------------------------------------------------------------

-- | Генерируем бесконечный список из случайных фигур
initFigures :: StdGen -> [Figure]
initFigures g = map genFigure
  (randomRs getrange g)

-- | диапазон генерации случайных чисел
getrange :: (Int, Int)
getrange = (0, 6)
  

-- | Генерируем пустую доску
genEmptyBoard::Board
genEmptyBoard = [(bs*9,bs*20,0),(bs*8,bs*20,0),(bs*7,bs*20,0),(bs*6,bs*20,0),(bs*5,bs*20,0),(bs*4,bs*20,0),
  (3*bs,bs*20,0),(2*bs,bs*20,0),(1*bs,bs*20,0)]
   where bs = blockSize

-- | Генерируем игровую вселенную (пустая доска, бесконечный список фигур, начальные скорость, время и счет)
genUniverse::StdGen -> Gamestate
genUniverse g = (genEmptyBoard,initFigures g,(init_tact, 0),0)

-------------------------------------------------------------------------------------------------------------------------------------

-- =========================================
-- * Moves
-- =========================================


-- | Координаты блоков фигуры
type BlockedFigure = (Coord, Coord, Coord, Coord)

-- | Поворачивает фигуру:  функция смотрит, какая ей дана фигура, и вычисляет расстояние до края доски и на основании этой информации поворачивает ее (если это можно сделать)
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

-- | Готовим фигуры к отрисовке
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

-- | Принимает пустую доску, моделирует всю игру, послеокончания возвращает счет
startGame::Board -> Score
startGame  _ =  0

-- | Шаг влево
moveLeft::Gamestate -> Gamestate
moveLeft (a,((Figure s t (b,c,z)):rest),d,e) | collide = (a, ((Figure s t (b,c,z)):rest),d,e)
        |otherwise = (a, ((Figure s t (b - blockSize,c,z)):rest),d,e)
  where 
    collide = collidesFigureSides (figureToDraw (Figure s t (b - blockSize,c,z))) a

-- | Шаг вправо 
moveRight::Gamestate -> Gamestate
moveRight (a,(Figure s t (b,c,z)):rest,d,e) | collide = (a, ((Figure s t (b,c,z)):rest),d,e)
        |otherwise = (a, ((Figure s t (b + blockSize,c,z)):rest),d,e)
  where 
    collide = collidesFigureSides (figureToDraw (Figure s t (b + blockSize,c,z))) a

-- | Проверка: пересекается ли блок
collidesBlock::Coord -> Bool
collidesBlock (a,b,z) | (a < 0) || (a  + blockSize > screenWidth) || (b < 0) || (b + blockSize > screenHeight) = True
       |otherwise = False

-- | Проверка: пересекается ли блок
collidesBlockSides::Coord -> Board -> Bool
collidesBlockSides (a,b,z) [] = (a < 0) || (a  + blockSize > screenWidth)
collidesBlockSides (a,b,z) ((brda, brdb,z1):[]) = (a < 0) || (a  + blockSize > screenWidth) || (a==brda) && (b==brdb)
collidesBlockSides (a,b,z) ((brda, brdb,z1):brds) | (a < 0) || (a  + blockSize > screenWidth) || (a==brda) && (b==brdb)  = True
                                             | otherwise = collidesBlockSides (a,b,z) brds

-- | Проверка: пересекается ли блок
collidesBlockDown::Coord -> Board-> Bool
collidesBlockDown (a,b,z) []  =   (b + blockSize > screenHeight)
collidesBlockDown (a,b,z) ((brda,brdb,z1):[])  =   ((b + blockSize > screenHeight) || (a==brda) && (b==brdb))
collidesBlockDown (a,b,z) ((brda,brdb,z1):brds)  | (b + blockSize > screenHeight) || (a==brda) && (b==brdb)  = True
                                            |  otherwise = collidesBlockDown (a,b,z) brds

-- | Проверка: пересекается ли блок
collidesBlockUp::Coord -> Board-> Bool
collidesBlockUp (a,b,z) []  =  b < 0
collidesBlockUp (a,b,z) ((brda,brdb,z1):[])  =   (b < 0 && (b==brdb))
collidesBlockUp (a,b,z) ((brda,brdb,z1):brds)  | b < 0 && (b==brdb)  = True
                                          |  otherwise = collidesBlockUp (a,b,z) brds

-- | Пересекает ли фигура доску или границы
collidesFigure::BlockedFigure -> Board -> Bool
collidesFigure (a,b,c,d) board = (collidesFigureSides (a,b,c,d) board) || (collidesFigureDown (a,b,c,d) board)

-- | Проверка: 
collidesFigureSides::BlockedFigure -> Board -> Bool
collidesFigureSides (a,b,c,d) board | (collidesBlockSides a board) || (collidesBlockSides b board) || (collidesBlockSides c board) || (collidesBlockSides d board) = True
        |otherwise = False

-- | Проверка: Фигура касается снизу доски или поля
collidesFigureDown::BlockedFigure -> Board -> Bool
collidesFigureDown (a,b,c,d) board | (collidesBlockDown a board) || (collidesBlockDown b board) || (collidesBlockDown c board) || (collidesBlockDown d board) = True
        |otherwise = False

-- | Проверка: закончилась ли игра
isGameOver::Gamestate -> Bool
isGameOver (a,(f1:f2:rest),d,e) = collidesFigureDown (figureToDraw f2) a

-- | Сортируем строки
sortRows :: Board -> Board
sortRows []     = []
sortRows ((brda,brdb,z):brds) = sortRows (filter (\(x,y,z) -> y > brdb) brds) ++ [(brda,brdb,z)] ++ sortRows (filter (\(x,y,z) -> y <= brdb) brds)

-- | Удалям заполненные строки
deleteRows :: Board -> Board
deleteRows [] = []
deleteRows ((brda,brdb,z):brds) | (length (filter (\(x,y,z) -> brdb == y) ((brda,brdb,z):brds)) == 10)  =  (deleteRows (map (\(x,y,z) -> (x, y + blockSize,z)) (filter (\(x,y,z) -> y < brdb) l)) ++ (filter (\(x,y,z) -> y > brdb) l))
                              | otherwise = (filter (\(x,y,z) -> brdb == y) ((brda,brdb,z):brds)) ++ (deleteRows  (filter (\(x,y,z) -> brdb /= y) ((brda,brdb,z):brds)))                  -----   ToDo:   Обработать левый операнд аппенда.  После функции проверить, что между У нет зазоров.
                         where l = (filter (\(x,y,z) -> brdb /= y) ((brda,brdb,z):brds))

-- | При нажатии клавиши "вниз" роняет фигуру 
dropit::Gamestate -> Int -> Gamestate
dropit (a,((Figure sha dir (b,c,z)):rest),d,e) pts  | collide = (a,((Figure sha dir (b,c,z)):rest),d,e+(div pts blockSize))                   
                                                  | otherwise = dropit (a,((Figure sha dir (b,c + blockSize,z)):rest),d,e) pts                                        
                                          where                                           
                                              collide = collidesFigureDown (figureToDraw (Figure sha dir (b,c + blockSize,z))) a

-- | Рисуем доску
drawBoard::Board  -> Picture
drawBoard s = pictures (map drawBlock s)

-- | Рисуем блок
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

-- | Рисуем фигуру 
drawFigure::Gamestate  ->  Picture
drawFigure (_,(f:_),_,_) = drawBlockedFigure  (figureToDraw f)

-- | Рисуем блоки фигуры
drawBlockedFigure::BlockedFigure -> Picture
drawBlockedFigure ((a, b, c, d)) =         pictures  [drawBlock   a ,
                                                     drawBlock    b ,
                                                     drawBlock     c ,
                                                     drawBlock     d ]

-- | Рисуем тетрис
drawTetris ::Gamestate-> Picture
drawTetris (b,fs,s,t) = pictures
  [ drawFigure (b,fs,s,t),
   drawBoard b ,
    drawScore t
  ] 

-- | Рисуем счет
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
-- * Updating
-- =========================================


--Проверяет, достигла ли нижняя часть фигуры нижней 
--границы доски или другой фигуры:
-- Пока она реализована в updateTetris

--Проверяет, не выходит ли правая или левая часть фигуры за правую или
-- левую часть доски соответственно
--пока реализована в обраюотчиках клавиш

--Делает пустые блоки доски, на в которых находится фигура заполненными,
--вызываем ее после падения фигуры

-- | Преобразует вектор координат в список
vectolist :: (Coord, Coord, Coord, Coord) -> [Coord]
vectolist (a,b,c,d) = [a,b,c,d]

-- | Добавляет в доску упавшую фигуру
updateBoard::Gamestate ->Board
updateBoard (a,(fig:rest),(_, _),_) = a ++ vectolist (figureToDraw fig)

-- | На основании прошедшего времени меняет скорость полета фигур
updateSpeed::Time -> Speed -> Speed
updateSpeed _ _ = 0

-- | Аргумент функции play, обновляет состояние тетриса. С каждым кадром двигает фигуру вниз и пока здесь же проверяет, не достигла ли фигура нижней границы
updateTetris :: Float -> Gamestate -> Gamestate
updateTetris dt (a,(Figure sha dir (b,c,cl):rest),(sp, ti),e) | gameover = (genEmptyBoard,rest,(init_tact, 0),0)
                                                              | otherwise = newLevel (newTact (a,(Figure sha dir (b,c,cl):rest),(sp, ti),e) dt sp)
                                                                 where
                                                                   gameover = isGameOver (a,(Figure sha dir (b,c,cl):rest),(sp, ti),e)

-- ===========================================
-- * AI
-- | Мы хотим максисизировать количество удаленных строк, минимизировать количество дырок, минимизировать высоту тетриса
-- =======================================

-- | Функция сравнения двух элементов доски (нужна для упорядочивания доски).
greater :: Coord -> Coord -> Bool
greater (x1,y1,_) (x2,y2,_) | x1 > x2 = True
                              | (x1 == x2) && (y1 < y2) = True
                              | otherwise = False

-- | Сортирует доску по столбцам. Столбцы также отсортировываются.
sortBoard :: Board -> Board
sortBoard []     = []
sortBoard (brd:brds) = sortBoard (filter (\x -> greater x brd ) brds) ++ [brd] ++ sortBoard (filter (\x -> not (greater x brd)) brds)

-- | Сравнивает, какой вариант лучше
best :: Variant -> Variant -> Variant
best (x1,y1,t1) (x2,y2,t2) | x1 >= x2 = (x1, y1, t1)
                      | otherwise = (x2, y2, t2)

-- | Выбирает наилучший вариант развития событий
bestVariant :: [Variant] -> Variant
bestVariant [] = (0, -1, 0)
bestVariant ((y,x,t):hs) = best (y,x,t) (bestVariant hs) 

-- | Высота доски. Имеется ввиду высочайшая точка доски. 
boardHeight :: Board -> Int
boardHeight brds  = minimum (map (\(_,y,_) -> y) brds)

-- | Средняя высота доски
avgBoardHeight :: Board -> Int
avgBoardHeight b = div (sumBoardHeight b) 10

-- | Сумма высот столбцов доски
sumBoardHeight :: Board -> Int
sumBoardHeight b = sumhofb (sortBoard b)

sumhofb :: Board -> Int
sumhofb [] = 0
sumhofb ((x1,y1,z1):hs) = y1  +  sumhofb (filter (\(x,_,_) -> not (x == x1)) ((x1,y1,z1):hs))

-- | Количество дырок в доске
numberHoles :: Board -> Int
numberHoles b = nh (sortBoard b)

nh :: Board -> Int
nh [] = 0
nh ((x1,y1,z1):hs) = nhcolumn (filter (\(x,_,_) -> x == x1) ((x1,y1,z1):hs))  + nh (filter (\(x,_,_) -> not (x == x1)) ((x1,y1,z1):hs))

nhcolumn :: [Coord] -> Int
nhcolumn [] = 0
nhcolumn ((x1,y1,z1):hs) = (div (screenHeight - y1) blockSize) - length ((x1,y1,z1):hs)

-- | Количество удаленных строк, после сделанного хода
numberDeletes :: Board -> Int
numberDeletes b = (boardHeight (deleteRows (sortRows b))) - (boardHeight b)

-- | Оценка состояния доски, после сделанного хода 
boardProfit :: Board -> Int
boardProfit b = 2000 + 10000 * (numberDeletes b) - 100 * (numberHoles b) - 10 * ((avgBoardHeight b) - (boardHeight b))

-- | Сортируем варианты по профиту, количеству поворотова, смещению
sortVariants :: [Variant] -> [Variant]
sortVariants []     = []
sortVariants (brd:brds) = sortVariants (filter (\x -> better x brd ) brds) ++ [brd] ++ sortVariants (filter (\x -> not (better x brd)) brds)

-- | Функция сравнения двух вариантов
better :: Variant -> Variant -> Bool
better (profit1, dx1, r1) (profit2, dx2, r2) | profit1 > profit2 = True
                                             | (profit1 == profit2) && (r1 < r2) = True
                                             | (profit1 == profit2) && (r1 == r2) && ((abs dx1) < (abs dx2)) = True
                                             | otherwise = False

-- | Анализирует Gamestate. Возвращает (Int, Int, Int) (профит, смещение от центра, количество поворотов). Отрицательное смещение - двигаемся влево. Иначе - вправо.
bestStep :: Gamestate -> Variant
bestStep (b, (Figure sha dir (f1,f2,f3):rest), (sp, ti), s) =
   bestVariant (sortVariants [ genVariant gs dx r | dx <- [-5..4], r <- [0..3] ])
      where
        gs = (b, (Figure sha dir (f1,f2,f3):rest), (sp, ti), s)

-- | Применяет функцию f n раз к сущности а
apply :: (a -> a) -> Int -> a -> a
apply _ 0 par = par
apply f num par = apply f  (num - 1) (f par)

-- | Генерирует вариант развития событий
genVariant:: Gamestate -> Int -> Int -> Variant
genVariant gs dx r = (boardProfit (updateBoard (dropit (move (rotate gs)) (screenHeight-f2))), dx, r)
  where
    (_, Figure _ _ (_, f2, _) : _, _, _) = gs
    rotate = apply turn r
    move
      | dx > 0    = apply moveRight dx
      | otherwise = apply moveLeft (abs dx)

-- | в newTact вызывается makeStep 4 раза. Т.е ИИ делает 4 хода в такт. 
makeStep:: Gamestate -> Gamestate 
makeStep (b, (Figure sha dir (f1,f2,f3):rest), (sp, ti), s)
  | needturn = turn (b, (Figure sha dir (f1,f2,f3):rest), (sp, ti), s)  
  | needleft = moveLeft (b, (Figure sha dir (f1,f2,f3):rest), (sp, ti), s)  
  | needright = moveRight (b, (Figure sha dir (f1,f2,f3):rest), (sp, ti), s)  
  | otherwise   = dropit (b, (Figure sha dir (f1,f2,f3):rest), (sp, ti), s) (screenHeight-f2)
    where
      needturn = (\(_,_,t) -> t) (bestStep (b, (Figure sha dir (f1,f2,f3):rest), (sp, ti), s)) > 0
      needleft = (\(_,y,_) -> y) (bestStep (b, (Figure sha dir (f1,f2,f3):rest), (sp, ti), s)) < 0
      needright = (\(_,y,_) -> y) (bestStep (b, (Figure sha dir (f1,f2,f3):rest), (sp, ti), s)) > 0


-- ===========================================
-- * Timing
-- =======================================
-- | Новый такт
newTact::Gamestate -> Float -> Float -> Gamestate
newTact (b, (Figure sha dir (f1,f2,f3):rest), (sp, ti), s) dt tact
  | paused = (b, (Figure sha dir (f1,f2,f3):rest), (sp, ti), s)
  | new && collides = (deleteRows (sortRows (updateBoard (b, (Figure sha dir (f1,f2,f3):rest), (sp, ti), s))), rest, (sp, ti), s + 1)
  | new = newTact (makeStep(makeStep(makeStep(makeStep (b, (Figure sha dir (f1,f2 + blockSize,f3):rest), (sp, 0), s))))) (dt + ti - tact) tact
  | collides = (b, (Figure sha dir (f1,f2,f3):rest), (sp, ti + dt + tact * 0.3), s)
  | otherwise = (b, (Figure sha dir (f1,f2,f3):rest), (sp, ti + dt), s)
                                        where
                                          new = ti + dt >= tact
                                          collides =  collidesFigureDown (figureToDraw (Figure sha dir (f1 ,f2 + blockSize,f3))) b
                                          paused = sp < 0

-- | Увеличивает скорость падения фигур, в зависимости от количества набранных очков
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

-- | Аргумент функции play, которая говорит, что делает каждая клавиша
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
