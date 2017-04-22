module Tetris where

import System.Random
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Geometry.Line
import Graphics.Gloss.Interface.Pure.Game
import GHC.Float

run :: IO ()

--run = putStrLn "This project is not yet implemented"
run = do
 g <- newStdGen

   --putStrLn "This project is not yet implemented"
 
 play display bgColor fps (genUniverse g ) drawTetris handleTetris updateTetris
   where
    display = InWindow "Tetris" (screenWidth, screenHeight) (200, 200)
    bgColor = black   -- цвет фона
    fps     = 100    -- кол-во кадров в секунду



-- =========================================
-- Types
-- =========================================





                               --data Shape = J | L | I | S | Z | O | T
                               --         deriving (Eq, Show, Enum)
--Клетка заполнена?
data Block = Free | Full
         deriving(Eq, Show)

--Строки нашей доски
type Row = [Block]

--Все поле
type Board = [Row]

--Счет
type Score = Integer

--Координаты фигуры, поворот однозначно определяется 
--их последовательностью
type Coord = (Int, Int)

--время
type Time = Float

--Состояние игры в текущий момент(разделили доску и фигуру,
--чтобы при полете фигуры не мигала вся доска, также, чтобы было более 
--оптимизировано)
--[Figure] - бесконечный список фигур, в текущем состоянии берем первый элемент списка
----------------------------------------------------------------------------------------------------------------------------------------------------------
type Gamestate = (Board,  [Figure], Speed, Time)
--data Gamestate = Gamestate
--    { board   :: Board  
--    , figure  :: Figure
--     , speed   :: Speed
--     , time    :: Time    
--     }
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
genFigure a | a== 0  =  Figure O DUp (0,0)
            | a== 1  =  Figure I DUp (0,0)
            | a== 2  =  Figure T DUp (0,0)
            | a== 3  =  Figure J DUp (0,0)
            | a== 4  =  Figure L DUp (0,0)
            | a== 5  =  Figure S DUp (0,0)
            | a== 6  =  Figure Z DUp (0,0)
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

genEmptyBoard::Board
genEmptyBoard = genRows width height
        where
          width = 10
          height = 20

genRows::Int->Int->[Row]
genRows _ 0 = []
genRows w h = (genRows w (h-1)) ++ [genRow w]


genRow::Int->Row
genRow 0 = []
genRow w = (genRow (w-1)) ++ [Free]


genUniverse::StdGen -> Gamestate
genUniverse g = (genEmptyBoard,initFigures g,0,0)


-------------------------------------------------------------------------------------------------------------------------------------
--Генерируем бесконечный список из случайных фигур
-- == initFigures
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
--        I Direction Coord | 
--        T Direction Coord |
--        J Direction Coord | L  Direction Coord | 
--        S Direction Coord | Z  Direction Coord
--          deriving(Eq, Show)

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
moveLeft::Gamestate -> Gamestate
--moveLeft _ =  Figure O DUp (0,0)
moveLeft (a,(Figure O DUp (b,c):rest),d,e) | ((b - 60 ) > - 1 )  = (a,(Figure O DUp (b - 60,c ):rest),d,e)
                                           | otherwise = (a,(Figure O DUp (b,c):rest),d,e)  
--Перемещает фигуру вправо
moveRight::Gamestate -> Gamestate
--moveRight _ =  Figure O DUp (0,0)
moveRight (a,(Figure O DUp (b,c):rest),d,e) | ((b + 60) < screenWidth) = (a,(Figure O DUp (b + 60,c ):rest),d,e)
                                                                                         | otherwise = (a,(Figure O DUp (b,c):rest),d,e)
--При нажатии клавиши "вниз" роняет фигуру 
dropit::Gamestate -> Gamestate
dropit  (a,(Figure O DUp (b,c):rest),d,e) =       (a,(Figure O DUp (b,screenHeight - 60 ):rest),d,e)


-- =========================================
-- Checking rows and deleting
-- =========================================




--Смотрит, нет ли строк, которые можно удалить
--checkRowsToDelete::Board -> [Bool]
--checkRowsToDelete _ =  [False]
--Смотрит, можно ли удаоить строку
--checkRow::Row -> Bool
--checkRow _ =  False
--Удаляет строку
--deleteRow::Int -> Board -> Board
--deleteRow _ _=  [[Free]]
--проверяет, конец игру т.е. приземлилась ли фигура до появления на
--экране, т.е. конец игры


-----------------------------------------------------------------------------------------------------------------
--Смотрит, нет ли строк, которые можно удалить
checkRowsToDelete::Board -> [Bool]
checkRowsToDelete (r:[]) =  (checkRow r):[]
checkRowsToDelete (r:rs) = (checkRow r) : (checkRowsToDelete rs)

--Смотрит, можно ли удаоить строку
checkRow::Row -> Bool
checkRow (Free:[]) = False
checkRow (Full:[]) = True
checkRow (c:cs)  | c == Free = False
                 | otherwise =  checkRow cs
--Удаляет строку
deleteRow::[Bool] -> Board -> Board
deleteRow (b:bs) (r:rs)  | b == False = r:(deleteRow bs rs)
    | otherwise = (deleteRow bs rs)                                  

--------------------------------------------------------------------------------------------------------------
gameover :: Gamestate -> Bool
gameover _ =  False



-- =========================================
-- Drawing
-- =========================================





--Рисуем доску
--заглушка
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
--заглушка
drawFigure::Gamestate  ->  Picture
drawFigure (a,(Figure O DUp (b,c):rest),d,e) = pictures[ drawBlock (a,(Figure O DUp (b,c):rest),d,e),
                                                         drawBlock (a,(Figure O DUp (b,c + 30):rest),d,e),
                                                         drawBlock (a,(Figure O DUp (b + 30,c):rest),d,e),
                                                         drawBlock (a,(Figure O DUp (b + 30 ,c + 30):rest),d,e)
    
                                                       ]
drawFigure (a,(Figure I DUp (b,c):rest),d,e) =  pictures[ drawBlock (a,(Figure O DUp (b,c):rest),d,e),
                                                         drawBlock (a,(Figure O DUp (b,c + 30):rest),d,e),
                                                         drawBlock (a,(Figure O DUp (b,c + 60):rest),d,e),
                                                         drawBlock (a,(Figure O DUp (b,c + 90):rest),d,e)
    
                                                       ]
drawFigure (a,(Figure T DUp (b,c):rest),d,e) =  pictures[ drawBlock (a,(Figure O DUp (b + 30,c):rest),d,e),
                                                         drawBlock (a,(Figure O DUp (b + 30,c + 30):rest),d,e),
                                                         drawBlock (a,(Figure O DUp (b + 60,c):rest),d,e),
                                                         drawBlock (a,(Figure O DUp (b ,c):rest),d,e)
    
                                                       ]
drawFigure (a,(Figure J DUp (b,c):rest),d,e) =  pictures[ drawBlock (a,(Figure O DUp (b+ 30,c):rest),d,e),
                                                         drawBlock (a,(Figure O DUp (b+ 30,c + 30):rest),d,e),
                                                         drawBlock (a,(Figure O DUp (b+ 30,c + 60):rest),d,e),
                                                         drawBlock (a,(Figure O DUp (b,c + 60):rest),d,e)
    
                                                       ]
drawFigure (a,(Figure L DUp (b,c):rest),d,e) =  pictures[ drawBlock (a,(Figure O DUp (b,c):rest),d,e),
                                                         drawBlock (a,(Figure O DUp (b,c + 30):rest),d,e),
                                                         drawBlock (a,(Figure O DUp (b,c + 60):rest),d,e),
                                                         drawBlock (a,(Figure O DUp (b + 30 ,c + 60):rest),d,e)
    
                                                       ]
drawFigure (a,(Figure S DUp (b,c):rest),d,e) =  pictures[ drawBlock (a,(Figure O DUp (b+ 30,c):rest),d,e),
                                                         drawBlock (a,(Figure O DUp (b+ 30,c + 30):rest),d,e),
                                                         drawBlock (a,(Figure O DUp (b + 60,c):rest),d,e),
                                                         drawBlock (a,(Figure O DUp (b,c + 30):rest),d,e)
    
                                                       ]
drawFigure (a,(Figure Z DUp (b,c):rest),d,e) =  pictures[ drawBlock (a,(Figure O DUp (b+ 30,c):rest),d,e),
                                                         drawBlock (a,(Figure O DUp (b+ 30,c + 30):rest),d,e),
                                                         drawBlock (a,(Figure O DUp (b,c):rest),d,e),
                                                         drawBlock (a,(Figure O DUp (b + 60 ,c + 30):rest),d,e)
                                                       ]



--Рисуем тетрис
--Пока только рисует квадрат
drawTetris ::Gamestate-> Picture
drawTetris u = pictures
  [ drawFigure u
  ]





--drawTetris _ = color white . pictures . map drawBox . [ ((double2Float 0.0, double2Float 0.0), (double2Float 0.0, double2Float (-4.0)))
--                                                    , ((double2Float 4.0, double2Floatl (-4.0)), (double2Float 4.0,double2Float 0.0))
--                                                   ]
--  where
--    drawBox ((l, b), (r, t)) = polygon
--      [ (l, b), (r, b), (r, t), (l, t) ]




--   , color orange (polygon [ (0, 0), (0, -1.9), (5.9, -1.9), (5.9, 0) ])    -- чёрные внутренности
--   , translate 2 (-1.5) (scale 0.01 0.01 (color red (text (show 0))))  -- красный счёт
  
--polygon
--      [ (l, b), (r, b), (r, t), (l, t) ]



---------------------------------------------------------------------------------------------------------------------------------------------------
--Рисуем блок
drawBlock :: Gamestate-> Picture
drawBlock (a,(Figure O DUp (b,c):rest),d,e) =  pictures [ translate (-w) h (scale  1 1 (pictures
 [ color white  (polygon [ ( fromIntegral b, fromIntegral (-c)), (fromIntegral b, fromIntegral (-c - 30)), (fromIntegral  (b + 30),fromIntegral (-c - 30)), (fromIntegral  (b + 30),fromIntegral (- c)) ])            -- белая рамка
   ]))]
  where
  w = fromIntegral screenWidth  / 2
  h = fromIntegral screenHeight / 2
drawBlock (a,(Figure I DUp (b,c):rest),d,e) =  pictures [ translate (-w) h (scale  1 1 (pictures
 [ color white  (polygon [ ( fromIntegral b, fromIntegral (-c)), (fromIntegral b, fromIntegral (-c - 30)), (fromIntegral  (b + 30),fromIntegral (-c - 30)), (fromIntegral  (b + 30),fromIntegral (- c)) ])            -- белая рамка
   ]))]
  where
  w = fromIntegral screenWidth  / 2
  h = fromIntegral screenHeight / 2
drawBlock (a,(Figure T DUp (b,c):rest),d,e) =  pictures [ translate (-w) h (scale  1 1 (pictures
 [ color white  (polygon [ ( fromIntegral b, fromIntegral (-c)), (fromIntegral b, fromIntegral (-c - 30)), (fromIntegral  (b + 30),fromIntegral (-c - 30)), (fromIntegral  (b + 30),fromIntegral (- c)) ])            -- белая рамка
   ]))]
  where
  w = fromIntegral screenWidth  / 2
  h = fromIntegral screenHeight / 2
drawBlock (a,(Figure J DUp (b,c):rest),d,e) =  pictures [ translate (-w) h (scale  1 1 (pictures
 [ color white  (polygon [ ( fromIntegral b, fromIntegral (-c)), (fromIntegral b, fromIntegral (-c - 30)), (fromIntegral  (b + 30),fromIntegral (-c - 30)), (fromIntegral  (b + 30),fromIntegral (- c)) ])            -- белая рамка
   ]))]
  where
  w = fromIntegral screenWidth  / 2
  h = fromIntegral screenHeight / 2
drawBlock (a,(Figure L DUp (b,c):rest),d,e) =  pictures [ translate (-w) h (scale  1 1 (pictures
 [ color white  (polygon [ ( fromIntegral b, fromIntegral (-c)), (fromIntegral b, fromIntegral (-c - 30)), (fromIntegral  (b + 30),fromIntegral (-c - 30)), (fromIntegral  (b + 30),fromIntegral (- c)) ])            -- белая рамка
   ]))]
  where
  w = fromIntegral screenWidth  / 2
  h = fromIntegral screenHeight / 2
drawBlock (a,(Figure S DUp (b,c):rest),d,e) =  pictures [ translate (-w) h (scale  1 1 (pictures
 [ color white  (polygon [ ( fromIntegral b, fromIntegral (-c)), (fromIntegral b, fromIntegral (-c - 30)), (fromIntegral  (b + 30),fromIntegral (-c - 30)), (fromIntegral  (b + 30),fromIntegral (- c)) ])            -- белая рамка
   ]))]
  where
  w = fromIntegral screenWidth  / 2
  h = fromIntegral screenHeight / 2 
drawBlock (a,(Figure Z DUp (b,c):rest),d,e) =  pictures [ translate (-w) h (scale  1 1 (pictures
 [ color white  (polygon [ ( fromIntegral b, fromIntegral (-c)), (fromIntegral b, fromIntegral (-c - 30)), (fromIntegral  (b + 30),fromIntegral (-c - 30)), (fromIntegral  (b + 30),fromIntegral (- c)) ])            -- белая рамка
   ]))]
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
updateBoard::Figure -> Board ->Board
updateBoard _ _ =  [[Free]]
--На основании прошедшего времени меняет скорость полета фигур
updateSpeed::Time -> Speed -> Speed
updateSpeed _ _ = 0


--Аргумент функции play, обновляет состояние тетриса
--С каждым кадром двигает фигуру вниз и пока здесь же проверяет, не достигла ли фигура нижней границы

updateTetris :: Float -> Gamestate -> Gamestate
updateTetris _  (a,(Figure O DUp (b,c):rest),d,e) | c < screenHeight - 60   =  (a,(Figure O DUp (b ,c +1):rest),d,e)
                                                  | otherwise = (a,(Figure O DUp (b,c):rest),d,e)
updateTetris _  (a,(Figure I DUp (b,c):rest),d,e) | c < screenHeight - 60   =  (a,(Figure I DUp (b ,c +1):rest),d,e)
                                                  | otherwise = (a,(Figure I DUp (b,c):rest),d,e)
updateTetris _  (a,(Figure T DUp (b,c):rest),d,e) | c < screenHeight - 60   =  (a,(Figure T DUp (b ,c +1):rest),d,e)
                                                  | otherwise = (a,(Figure T DUp (b,c):rest),d,e)
updateTetris _  (a,(Figure J DUp (b,c):rest),d,e) | c < screenHeight - 60   =  (a,(Figure J DUp (b ,c +1):rest),d,e)
                                                  | otherwise = (a,(Figure J DUp (b,c):rest),d,e)
updateTetris _  (a,(Figure L DUp (b,c):rest),d,e) | c < screenHeight - 60   =  (a,(Figure L DUp (b ,c +1):rest),d,e)
                                                  | otherwise = (a,(Figure L DUp (b,c):rest),d,e)
updateTetris _  (a,(Figure S DUp (b,c):rest),d,e) | c < screenHeight - 60   =  (a,(Figure S DUp (b ,c +1):rest),d,e)
                                                  | otherwise = (a,(Figure S DUp (b,c):rest),d,e)
updateTetris _  (a,(Figure Z DUp (b,c):rest),d,e) | c < screenHeight - 60   =  (a,(Figure Z DUp (b ,c +1):rest),d,e)
                                                  | otherwise = (a,(Figure Z DUp (b,c):rest),d,e)                                                                                                                                                                                                                                                                  


--Обновить весь тетрис
updateTheWholeTetris:: Time -> Speed -> Gamestate -> Gamestate
updateTheWholeTetris _ _ (a,(b:rest),c,d) = (a,(b:rest),c,d)
-- ===========================================
-- timing
-- =======================================





--Обновляет общее состояние тетриса
newTact::Figure -> Board -> Speed -> Gamestate
newTact _ _ _ =  ([[Free]],[Figure O DUp (0,0)],0,0)
--Застявляет фигуру постоянно падать, вызываем эту фунцию на каждом такте
newMove::Board -> Gamestate
newMove _ =  ([[Free]],[Figure O DUp (0,0)],0,0)


--Аргумент функции play, которая говорит, что длает каждая клавиша
handleTetris :: Event -> Gamestate -> Gamestate
handleTetris (EventKey (SpecialKey KeyRight) Down _ _) (a,(Figure O DUp (b,c):rest),d,e) = moveRight (a,(Figure O DUp (b,c):rest),d,e)
handleTetris (EventKey (SpecialKey KeyRight) Up _ _) t = t
             
handleTetris (EventKey (SpecialKey KeyLeft) Down _ _)  (a,(Figure O DUp (b,c):rest),d,e)  = moveLeft (a,(Figure O DUp (b,c):rest),d,e) 
handleTetris (EventKey (SpecialKey KeyLeft) Up _ _)  t  = t

handleTetris(EventKey (SpecialKey KeyDown) Down _ _ ) (a,(Figure O DUp (b,c):rest),d,e)  = dropit (a,(Figure O DUp (b,c):rest),d,e) 
handleTetris(EventKey (SpecialKey KeyDown) Up _ _ ) t = t

handleTetris (EventKey (SpecialKey KeyUp) Down _ _ ) (a,(Figure O DUp (b,c):rest),d,e) = (a,(Figure O DUp (b ,c - 60):rest),d,e)
handleTetris (EventKey (SpecialKey KeyUp) Up _ _ ) t = t
handleTetris  _ t = t  

