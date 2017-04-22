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
    fps     = 10    -- кол-во кадров в секунду



-- =========================================
-- Types
-- =========================================



blockSize :: Int
blockSize = 30



                               --data Shape = J | L | I | S | Z | O | T
                               --         deriving (Eq, Show, Enum)
--Клетка заполнена?
data Block = Free | Full
         deriving(Eq, Show)

--Строки нашей доски
type Row = [Block]

--Все поле
type Board = [Coord]

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
genFigure a | a== 0  =  Figure O DUp (div screenWidth 2, blockSize * 2)
            | a== 1  =  Figure I DUp (div screenWidth 2, blockSize * 2)
            | a== 2  =  Figure T DUp (div screenWidth 2, blockSize * 2)
            | a== 3  =  Figure J DUp (div screenWidth 2, blockSize * 2)
            | a== 4  =  Figure L DUp (div screenWidth 2, blockSize * 2)
            | a== 5  =  Figure S DUp (div screenWidth 2, blockSize * 2)
            | a== 6  =  Figure Z DUp (div screenWidth 2, blockSize * 2)
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
genEmptyBoard = []

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

type BlockedFigure = (Coord, Coord, Coord, Coord)


turn::Gamestate -> Gamestate
turn (a,(Figure t DUp c):rest,d,e) | collide1 = (a,(Figure t DUp c):rest,d,e)
                                   | otherwise = (a,(Figure t DRight c):rest,d,e)
                            where 
                                collide1 = collidesFigure (figureToDraw (Figure t DRight c))
turn (a,(Figure t DRight c):rest,d,e) | collide2 = (a,(Figure t DRight c):rest,d,e)
                                      | otherwise = (a,(Figure t DDown c):rest,d,e)
                            where 
                                collide2 = collidesFigure (figureToDraw (Figure t DDown c))
turn (a,(Figure t DDown c):rest,d,e) | collide3 = (a,(Figure t DDown c):rest,d,e)
                                     | otherwise = (a,(Figure t DLeft c):rest,d,e)
                            where 
                                collide3 = collidesFigure (figureToDraw (Figure t DLeft c))
turn (a,(Figure t DLeft c):rest,d,e) | collide4 = (a,(Figure t DLeft c):rest,d,e)
                                     | otherwise = (a,(Figure t DUp c):rest,d,e)
                            where 
                                collide4 = collidesFigure (figureToDraw (Figure t DUp c))

figureToDraw::Figure->BlockedFigure
figureToDraw (Figure O d c) = figureToDrawO (Figure O d c)
figureToDraw (Figure I d c) = figureToDrawI (Figure I d c)
figureToDraw (Figure T d c) = figureToDrawT (Figure T d c)
figureToDraw (Figure J d c) = figureToDrawJ (Figure J d c)
figureToDraw (Figure L d c) = figureToDrawL (Figure L d c)
figureToDraw (Figure S d c) = figureToDrawS (Figure S d c)
figureToDraw (Figure Z d c) = figureToDrawZ (Figure Z d c)


figureToDrawO::Figure -> BlockedFigure
figureToDrawO (Figure O _ (x, y)) = ((x, y), (x+blockSize, y), (x, y-blockSize), (x+blockSize, y-blockSize))


figureToDrawI::Figure -> BlockedFigure
figureToDrawI (Figure I d (x, y)) | (d == DUp) || (d == DDown) = ((x, y+blockSize), (x, y), (x, y-blockSize), (x, y-2*blockSize))
                  | otherwise = ((x-blockSize, y), (x, y), (x+blockSize, y), (x+2*blockSize, y))

figureToDrawZ::Figure -> BlockedFigure
figureToDrawZ (Figure Z d (x, y)) | (d == DUp) || (d == DDown) = ((x-blockSize, y-blockSize), (x-blockSize, y), (x, y), (x, y+blockSize))
                    | otherwise = ((x-blockSize, y), (x, y), (x, y-blockSize), (x+blockSize, y-blockSize))

figureToDrawS::Figure -> BlockedFigure
figureToDrawS (Figure S d (x, y)) | (d == DUp) || (d == DDown) = ((x-blockSize, y+blockSize), (x-blockSize, y), (x, y), (x, y-blockSize))
                    | otherwise = ((x-blockSize, y), (x, y), (x, y+blockSize), (x+blockSize, y+blockSize))


figureToDrawJ::Figure -> BlockedFigure
figureToDrawJ (Figure J d (x,y)) | d == DDown = ((x-blockSize, y-blockSize), (x, y-blockSize), (x, y), (x, y+blockSize))
                 | d == DUp = ((x, y-blockSize), (x, y), (x, y+blockSize), (x+blockSize, y+blockSize))
                 | d == DRight = ((x-blockSize, y), (x, y), (x+blockSize, y), (x+blockSize, y-blockSize))
                 | otherwise = ((x-blockSize, y+blockSize), (x-blockSize, y), (x, y), (x+blockSize, y))


figureToDrawL::Figure -> BlockedFigure
figureToDrawL (Figure L d (x,y)) | d == DDown = ((x, y+blockSize), (x, y), (x, y-blockSize), (x+blockSize, y-blockSize))
                 | d == DUp = ((x, y-blockSize), (x, y), (x, y+blockSize), (x-blockSize, y+blockSize))
                 | d == DRight = ((x-blockSize, y), (x, y), (x+blockSize, y), (x+blockSize, y+blockSize))
                 | otherwise = ((x-blockSize, y-blockSize), (x-blockSize, y), (x, y), (x+blockSize, y))

figureToDrawT::Figure -> BlockedFigure
figureToDrawT (Figure T d (x,y)) | d == DDown = ((x-blockSize, y), (x, y), (x+blockSize, y), (x, y-blockSize))
                 | d == DUp = ((x-blockSize, y), (x, y), (x+blockSize, y), (x, y+blockSize))
                 | d == DRight = ((x, y+blockSize), (x, y), (x, y-blockSize), (x+blockSize, y))
                 | otherwise = ((x, y+blockSize), (x, y), (x, y-blockSize), (x-blockSize, y))

--Принимает пустую доску, моделирует всю игру, после
--окончания возвращает счет
startGame::Board -> Score
startGame  _ =  0
--Переещает фигуру влево  



moveLeft::Gamestate -> Gamestate
moveLeft (a,((Figure s t (b,c)):rest),d,e) | collide = (a, ((Figure s t (b,c)):rest),d,e)
        |otherwise = (a, ((Figure s t (b - blockSize,c)):rest),d,e)
  where 
    collide = collidesFigureSides (figureToDraw (Figure s t (b - blockSize,c))) a

moveRight::Gamestate -> Gamestate
moveRight (a,(Figure s t (b,c)):rest,d,e) | collide = (a, ((Figure s t (b,c)):rest),d,e)
        |otherwise = (a, ((Figure s t (b + blockSize,c)):rest),d,e)
  where 
    collide = collidesFigureSides (figureToDraw (Figure s t (b + blockSize,c))) a


collidesBlock::Coord -> Bool
collidesBlock (a,b) | (a < 0) || (a  + blockSize > screenWidth) || (b < 0) || (b + blockSize > screenHeight) = True
       |otherwise = False


collidesBlockSides::Coord -> Board -> Bool
collidesBlockSides (a,b) [] = (a < 0) || (a  + blockSize > screenWidth)
collidesBlockSides (a,b) ((brda, brdb):[]) = (a < 0) || (a  + blockSize > screenWidth) || (a==brda) && (b==brdb)
collidesBlockSides (a,b) ((brda, brdb):brds) | (a < 0) || (a  + blockSize > screenWidth) || (a==brda) && (b==brdb)  = True
                                             | otherwise = collidesBlockSides (a,b) brds


collidesBlockDown::Coord -> Board-> Bool
collidesBlockDown (a,b) []  =   (b + blockSize > screenHeight)
collidesBlockDown (a,b) ((brda,brdb):[])  =   ((b + blockSize > screenHeight) || (a==brda) && (b==brdb))
collidesBlockDown (a,b) ((brda,brdb):brds)  | (b + blockSize > screenHeight) || (a==brda) && (b==brdb)  = True
                                            |  otherwise = collidesBlockDown (a,b) brds



collidesBlockUp::Coord -> Board-> Bool
collidesBlockUp (a,b) []  =  b < 0
collidesBlockUp (a,b) ((brda,brdb):[])  =   (b < 0 && (b==brdb))
collidesBlockUp (a,b) ((brda,brdb):brds)  | b < 0 && (b==brdb)  = True
                                          |  otherwise = collidesBlockUp (a,b) brds
--collidesBlockDown (a,b) ((brda,brdb):brds) = True


collidesFigure::BlockedFigure -> Bool
collidesFigure (a,b,c,d ) | (collidesBlock a) || (collidesBlock b) || (collidesBlock c) || (collidesBlock d) = True
        |otherwise = False


collidesFigureSides::BlockedFigure -> Board -> Bool
collidesFigureSides (a,b,c,d) board | (collidesBlockSides a board) || (collidesBlockSides b board) || (collidesBlockSides c board) || (collidesBlockSides d board) = True
        |otherwise = False


collidesFigureDown::BlockedFigure -> Board -> Bool
collidesFigureDown (a,b,c,d) board | (collidesBlockDown a board) || (collidesBlockDown b board) || (collidesBlockDown c board) || (collidesBlockDown d board) = True
        |otherwise = False


isGameOver::Gamestate -> Bool
isGameOver (a,(f1:f2:rest),d,e) = collidesFigureDown (figureToDraw f2) a


deleteRows :: Board -> Board
deleteRows [] = []
deleteRows ((brda,brdb):brds) | (length (filter (\(x,y) -> brdb == y) ((brda,brdb):brds)) == 10)  =  (deleteRows (map (\(x,y) -> (x, y + blockSize)) (filter (\(x,y) -> y < brdb) l)) ++ (filter (\(x,y) -> y > brdb) l))
                              | otherwise = (filter (\(x,y) -> brdb == y) ((brda,brdb):brds)) ++ (deleteRows  (filter (\(x,y) -> brdb /= y) ((brda,brdb):brds)))
                         where l = (filter (\(x,y) -> brdb /= y) ((brda,brdb):brds))

--При нажатии клавиши "вниз" роняет фигуру 


dropit::Gamestate -> Gamestate
dropit (a,((Figure sha dir (b,c)):rest),d,e) | collide = (a,((Figure sha dir (b,c)):rest),d,e)                                             
                                             | otherwise = dropit (a,((Figure sha dir (b,c + blockSize)):rest),d,e)                                         
                                          where                                           
                                              collide = collidesFigureDown (figureToDraw (Figure sha dir (b,c + blockSize))) a

-- dropit::Gamestate -> Gamestate
-- dropit (a,((Figure sha dir (b,c)):rest),d,e) | collide = (a,((Figure sha dir (b,c + blockSize)):rest),d,e)
--                                              | otherwise = dropit (a,((Figure sha dir (b,c + blockSize)):rest),d,e)
--                                           where
--                                             collide = collidesFigureDown (figureToDraw (Figure sha dir (b,c + 2*blockSize)))


-----------------------------------------------------------------------------------------------------------------
--Смотрит, нет ли строк, которые можно удалить
--checkRowsToDelete::Board -> [Bool]
--checkRowsToDelete (r:[]) =  (checkRow r):[]
--checkRowsToDelete (r:rs) = (checkRow r) : (checkRowsToDelete rs)

--Смотрит, можно ли удаоить строку
--checkRow::Row -> Bool
--checkRow (Free:[]) = False
--checkRow (Full:[]) = True
--checkRow (c:cs)  | c == Free = False
       --          | otherwise =  checkRow cs
--Удаляет строку
--deleteRow::[Bool] -> Board -> Board
--deleteRow (b:bs) (r:rs)  | b == False = r:(deleteRow bs rs)
  --  | otherwise = (deleteRow bs rs)                                  

--------------------------------------------------------------------------------------------------------------
--gameover :: Gamestate -> Bool
--gameover _ =  False



-- =========================================
-- Drawing
-- =========================================





--Рисуем доску
--заглушка
--drawBoard::Board  -> Picture
--drawBoard _ =  translate (-w) h (scale 30 30 (pictures
--  [ color white (polygon [ (0, 0), (0, -2), (6, -2), (6, 0) ])            -- белая рамка
--  , color black (polygon [ (0, 0), (0, -1.9), (5.9, -1.9), (5.9, 0) ])    -- чёрные внутренности
--  , translate 2 (-1.5) (scale 0.01 0.01 (color red (text (show 0))))  -- красный счёт
--  ]))
--  where
--    w = fromIntegral screenWidth  / 2
--    h = fromIntegral screenHeight / 2

drawBoard::Board  -> Picture
drawBoard s = pictures (map drawBlock s)

drawBlock :: Coord-> Picture
drawBlock (b,c) =  pictures [ translate (-w) h (scale  1 1 (pictures
 [ color white  (polygon [ ( fromIntegral b, fromIntegral (-c)), (fromIntegral b, fromIntegral (-c - 30)), (fromIntegral  (b + 30),fromIntegral (-c - 30)), (fromIntegral  (b + 30),fromIntegral (- c)) ])            -- белая рамка
   ]))]
  where
  w = fromIntegral screenWidth  / 2
  h = fromIntegral screenHeight / 2


drawFigure::Gamestate  ->  Picture
drawFigure (b,(f:fs),s,t) = drawBlockedFigure (figureToDraw f)

drawBlockedFigure::BlockedFigure -> Picture
drawBlockedFigure ((a, b, c, d)) = pictures (map drawBlock [a,b,c,d])


--Рисуем тетрис
--Пока только рисует квадрат
drawTetris ::Gamestate-> Picture
drawTetris (b,fs,s,t) = pictures
  [ drawFigure (b,fs,s,t),
    drawBoard b
  ] 

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

updateBoard::Figure -> Board ->Board
updateBoard (Figure sha dir (b ,c)) a = a ++ vectolist (figureToDraw (Figure sha dir (b ,c)))

--На основании прошедшего времени меняет скорость полета фигур
updateSpeed::Time -> Speed -> Speed
updateSpeed _ _ = 0


--Аргумент функции play, обновляет состояние тетриса
--С каждым кадром двигает фигуру вниз и пока здесь же проверяет, не достигла ли фигура нижней границы


updateTetris :: Float -> Gamestate -> Gamestate
updateTetris _  (a,(Figure sha dir (b,c):rest),d,e) | gameover = (genEmptyBoard,rest,d,e)
                                                    | collide =  (deleteRows (updateBoard (Figure sha dir (b ,c)) a), rest, d, e)
                                                    | otherwise = (a,(Figure sha dir (b,c + blockSize):rest),d,e)
                                                       where
                                                       collide =  collidesFigureDown (figureToDraw (Figure sha dir (b ,c + blockSize)))   a
                                                       gameover = isGameOver (a,(Figure sha dir (b,c):rest),d,e)
--Обновить весь тетрис
updateTheWholeTetris:: Time -> Speed -> Gamestate -> Gamestate
updateTheWholeTetris _ _ (a,(b:rest),c,d) = (a,(b:rest),c,d)
-- ===========================================
-- timing
-- =======================================


--Обновляет общее состояние тетриса
--newTact::Figure -> Board -> Speed -> Gamestate
--newTact _ _ _ =  ([[Free]],[Figure O DUp (0,0)],0,0)
--Застявляет фигуру постоянно падать, вызываем эту фунцию на каждом такте
--newMove::Board -> Gamestate
--newMove _ =  ([[Free]],[Figure O DUp (0,0)],0,0)


--Аргумент функции play, которая говорит, что длает каждая клавиша


handleTetris :: Event -> Gamestate -> Gamestate

handleTetris (EventKey (SpecialKey KeyRight) Down _ _) (a,(Figure sha dir (b,c):rest),d,e) = moveRight (a,(Figure sha dir (b,c):rest),d,e)
handleTetris (EventKey (SpecialKey KeyRight) Up _ _) t = t

handleTetris (EventKey (SpecialKey KeyLeft) Down _ _)  (a,(Figure sha dir (b,c):rest),d,e)  = moveLeft (a,(Figure sha dir (b,c):rest),d,e)
handleTetris (EventKey (SpecialKey KeyLeft) Up _ _)  t  = t

handleTetris(EventKey (SpecialKey KeyDown) Down _ _ ) (a,(Figure sha dir (b,c):rest),d,e)  = dropit (a,(Figure sha dir (b,c):rest),d,e) 
handleTetris(EventKey (SpecialKey KeyDown) Up _ _ ) t = t

handleTetris (EventKey (SpecialKey KeyUp) Down _ _ ) (a,(Figure sha dir (b,c):rest),d,e) = turn (a, (Figure sha dir (b ,c):rest),d,e)
handleTetris (EventKey (SpecialKey KeyUp) Up _ _ ) t = t

handleTetris  _ t = t  

