{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
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
    display = InWindow "Tetris" (screenWidthreal, screenHeightreal) (0, 0)
    bgColor = black   -- цвет фона
    fps     = glob_fps   -- кол-во кадров в секунду



-- =========================================
-- Types
-- =========================================
screenWidthreal :: Int
screenWidthreal = 800

screenHeightreal :: Int
screenHeightreal = 800

blockSize :: Int
blockSize = 30

init_tact::Time
init_tact = 0.7

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
type Score = Int

--Координаты фигуры, поворот однозначно определяется 
--их последовательностью
-- x y цвет 
type Coord = (Int, Int,Int)

--время
type Time = Float


--type TetrisType = Int
data TetrisType = TetrisRect | TetrisRound
    deriving(Eq, Show)
data TetrisMove = TetrisStepped | TetrisSmooth
   deriving(Eq, Show)
--Состояние игры в текущий момент(разделили доску и фигуру,
--чтобы при полете фигуры не мигала вся доска, также, чтобы было более 
--оптимизировано)
--[Figure] - бесконечный список фигур, в текущем состоянии берем первый элемент списка
-- доска фигуры скорость время счет круговой(1) или прямоугольный(0) плавный(1) или чтупенчатый(0) init_tack
----------------------------------------------------------------------------------------------------------------------------------------------------------
type Gamestate = (Board,  [Figure], (Speed, Time), Score,TetrisType,TetrisMove,Time)


data GameState = GameState
 {  board   :: Board
  , figure :: [Figure]
  , speedandtime   :: (Speed, Time)
  , score   :: Score
  ,typerepres::TetrisType
  ,typemoving :: TetrisMove
  ,tactgamestate :: Time
  } 

--instance Show GameState where
  --show GameState{..} = 
    --show board ++ " " ++
    --show (head figure) ++ " " ++ 
    --show speed ++ " " ++
    --show time ++ " " ++
    --show score ++ "end "

fromGS :: GameState -> Gamestate
fromGS GameState{..} = (board, figure, speedandtime, score, typerepres,typemoving,tactgamestate)


toGS :: Gamestate -> GameState
toGS (board, figure, (speed, time), score, typerepres,typemoving,tactgamestate) = GameState board figure (speed, time) score  typerepres typemoving tactgamestate
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

genEmptyBoard::Board
genEmptyBoard = []

genRows::Int->Int->[Row]
genRows _ 0 = []
genRows w h = (genRows w (h-1)) ++ [genRow w]


genRow::Int->Row
genRow 0 = []
genRow w = (genRow (w-1)) ++ [Free]


genUniverse::StdGen -> GameState
genUniverse g = GameState{ board   = genEmptyBoard  
                          , figure  = initFigures g
                          , speedandtime   = (init_tact, 0)
                          , score    = 0
                          ,typerepres =    TetrisRect
                          ,typemoving =  TetrisStepped
                          ,tactgamestate     = 0.7
                          }


-------------------------------------------------------------------------------------------------------------------------------------
--Генерируем бесконечный список из случайных фигур
-- == initFigures
generateRandomFigureList:: StdGen -> [Figure]
generateRandomFigureList _ =  [Figure O DUp (0,0,0)]



-- =========================================
-- Moves
-- =========================================

--Поворачивает фигуру: положение фигуры в пространстве опредляется 
--двумя числами, функция смотрит, какая ей дана фигура, и вычисляет 
--расстояние до края доски и на основании этой информации поворачивает ее
--(если это можно сделать), т.е. изменяет 3 координату

type BlockedFigure = (Coord, Coord, Coord, Coord)


turn::Gamestate -> GameState
turn (a,(Figure t DUp c):rest,d,e,v,p,k) | collide1 = (toGS(a,(Figure t DUp c):rest,d,e,v,p,k))
                                   | otherwise = (toGS(a,(Figure t DRight c):rest,d,e,v,p,k))
                            where 
                                collide1 = collidesFigure (figureToDraw (Figure t DRight c)) a
turn (a,(Figure t DRight c):rest,d,e,v,p,k) | collide2 = (toGS(a,(Figure t DRight c):rest,d,e,v,p,k))
                                      | otherwise = (toGS(a,(Figure t DDown c):rest,d,e,v,p,k))
                            where 
                                collide2 = collidesFigure (figureToDraw (Figure t DDown c)) a
turn (a,(Figure t DDown c):rest,d,e,v,p,k) | collide3 = (toGS(a,(Figure t DDown c):rest,d,e,v,p,k))
                                     | otherwise = (toGS(a,(Figure t DLeft c):rest,d,e,v,p,k))
                            where 
                                collide3 = collidesFigure (figureToDraw (Figure t DLeft c)) a
turn (a,(Figure t DLeft c):rest,d,e,v,p,k) | collide4 = (toGS(a,(Figure t DLeft c):rest,d,e,v,p,k))
                                     | otherwise = (toGS(a,(Figure t DUp c):rest,d,e,v,p,k))
                            where 
                                collide4 = collidesFigure (figureToDraw (Figure t DUp c)) a




turn (a,(Figure t DUp c):rest,d,e,v,TetrisSmooth,k) | collide1 = (toGS(a,(Figure t DUp c):rest,d,e,v,TetrisSmooth,k))
                                   | otherwise = (toGS(a,(Figure t DRight c):rest,d,e,v,TetrisSmooth,k))
                            where 
                                collide1 = collidesFigureSmooth (figureToDraw (Figure t DRight c)) a
turn (a,(Figure t DRight c):rest,d,e,v,TetrisSmooth,k) | collide2 = (toGS(a,(Figure t DRight c):rest,d,e,v,TetrisSmooth,k))
                                      | otherwise = (toGS(a,(Figure t DDown c):rest,d,e,v,TetrisSmooth,k))
                            where 
                                collide2 = collidesFigureSmooth (figureToDraw (Figure t DDown c)) a
turn (a,(Figure t DDown c):rest,d,e,v,TetrisSmooth,k) | collide3 = (toGS(a,(Figure t DDown c):rest,d,e,v,TetrisSmooth,k))
                                     | otherwise = (toGS(a,(Figure t DLeft c):rest,d,e,v,TetrisSmooth,k))
                            where 
                                collide3 = collidesFigureSmooth (figureToDraw (Figure t DLeft c)) a
turn (a,(Figure t DLeft c):rest,d,e,v,TetrisSmooth,k) | collide4 = (toGS(a,(Figure t DLeft c):rest,d,e,v,TetrisSmooth,k))
                                     | otherwise = (toGS(a,(Figure t DUp c):rest,d,e,v,TetrisSmooth,k))
                            where 
                                collide4 = collidesFigureSmooth (figureToDraw (Figure t DUp c)) a


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



moveLeft::GameState -> GameState
moveLeft u |(typemoving u)==TetrisStepped = moveLeftStepped u 
            |otherwise = moveLeftSmooth u
moveLeftStepped ::GameState -> GameState
moveLeftStepped u   | collidewall = u{   figure =cons (mul8 (getf(figure u))) (rest  (figure u))}
                    | collide = u
                    |otherwise = u{ figure = cons  (minbl (getf(figure u))) (rest  (figure u))}

                    where 
    collide = collidesFigureSides (figureToDraw (minbl (getf(figure u)))) (board u)
    collidewall = collidesFigureSidesWallLeft (figureToDraw (minbl (getf(figure u)))) (board u)
mul8::Figure->Figure
mul8  (Figure s t (b,c,z ))   = (Figure s t (8*blockSize,c,z ))              
minbl::Figure->Figure
minbl  (Figure s t (b,c,z ))   = (Figure s t (b - blockSize,c,z ))  
rest ::[Figure]->[Figure]
rest (f:fs) = fs
cons ::Figure->[Figure]->[Figure]
cons  a f = a:f

moveLeftSmooth ::GameState -> GameState
moveLeftSmooth u   | collidewall = u{   figure =cons (mul8 (getf(figure u))) (rest  (figure u))}
                    | collide = u
                    |otherwise = u{ figure = cons  (minbl (getf(figure u))) (rest  (figure u))}

                    where 
    collide = collidesFigureSidesSmooth (figureToDraw (minbl (getf(figure u)))) (board u)
    collidewall = collidesFigureSidesWallLeft (figureToDraw (minbl (getf(figure u)))) (board u)      

--moveLeft (a,((Figure s t (b,c,z)):rest),d,e,v,TetrisStepped,k) |collidewall =(toGS (a, ((Figure s t (8*blockSize,c,z )):rest),d,e,v,TetrisStepped,k))
  --      | collide =(toGS (a, ((Figure s t (b,c,z)):rest),d,e,v,TetrisStepped,k))
    --    |otherwise = (toGS(a, ((Figure s t (b - blockSize,c,z)):rest),d,e,v,TetrisStepped,k))
--  where 
  --  collide = collidesFigureSides (figureToDraw (Figure s t (b - blockSize,c,z))) a
    --collidewall = collidesFigureSidesWallLeft (figureToDraw (Figure s t (b - blockSize,c,z))) a
--moveLeft (a,((Figure s t (b,c,z)):rest),d,e,v,TetrisSmooth,k) |collidewall = (toGS(a, ((Figure s t (8*blockSize,c,z )):rest),d,e,v,TetrisSmooth,k))
  --   | collide =(toGS (a, ((Figure s t (b,c,z)):rest),d,e,v,TetrisSmooth,k))
    --    |otherwise =(toGS (a, ((Figure s t (b - blockSize,c,z)):rest),d,e,v,TetrisSmooth,k))
 -- where 
   -- collide = collidesFigureSidesSmooth (figureToDraw (Figure s t (b - blockSize,c,z))) a    
   -- collidewall = collidesFigureSidesWallLeft (figureToDraw (Figure s t (b - blockSize,c,z))) a


moveRight::GameState -> GameState
moveRight u |(typemoving u)==TetrisStepped = moveRightStepped u 
            |otherwise = moveRightSmooth u
moveRightStepped ::GameState -> GameState
moveRightStepped u   | collidewall = u{   figure =cons (bl (getf(figure u))) (rest  (figure u))}
                    | collide = u
                    |otherwise = u{ figure = cons  (plbl (getf(figure u))) (rest  (figure u))}

                    where 
    collide = collidesFigureSides (figureToDraw (plbl (getf(figure u)))) (board u)
    collidewall = collidesFigureSidesWallRight (figureToDraw (plbl (getf(figure u)))) (board u)
bl::Figure->Figure
bl  (Figure s t (b,c,z ))   = (Figure s t (blockSize,c,z ))              
plbl::Figure->Figure
plbl  (Figure s t (b,c,z ))   = (Figure s t (b + blockSize,c,z ))  


moveRightSmooth ::GameState -> GameState
moveRightSmooth u   | collidewall = u{   figure =cons (bl (getf(figure u))) (rest  (figure u))}
                    | collide = u
                    |otherwise = u{ figure = cons  (plbl (getf(figure u))) (rest  (figure u))}

                    where 
    collide = collidesFigureSidesSmooth (figureToDraw (plbl (getf(figure u)))) (board u)
    collidewall = collidesFigureSidesWallRight (figureToDraw (plbl (getf(figure u)))) (board u)   






--moveRight::Gamestate -> GameState
--moveRight (a,(Figure s t (b,c,z)):rest,d,e,v,TetrisStepped,k) | collidewall  =(toGS (a, ((Figure s t (blockSize,c,z )):rest),d,e,v,TetrisStepped,k))
--         | collide = (toGS(a, ((Figure s t (b,c,z)):rest),d,e,v,TetrisStepped,k))
--        |otherwise = (toGS(a, ((Figure s t (b + blockSize,c,z)):rest),d,e,v,TetrisStepped,k))
--  where 
--    collide = collidesFigureSides (figureToDraw (Figure s t (b + blockSize,c,z))) a
--    collidewall = collidesFigureSidesWallRight (figureToDraw (Figure s t (b + blockSize,c,z))) a    
--moveRight (a,(Figure s t (b,c,z)):rest,d,e,v,TetrisSmooth,k)| collidewall  = (toGS(a, ((Figure s t (blockSize,c,z )):rest),d,e,v,TetrisSmooth,k))
--       | collide = (toGS(a, ((Figure s t (b,c,z)):rest),d,e,v,TetrisSmooth,k))
--        |otherwise = (toGS(a, ((Figure s t (b + blockSize,c,z)):rest),d,e,v,TetrisSmooth,k))
--  where 
--    collide = collidesFigureSidesSmooth (figureToDraw (Figure s t (b + blockSize,c,z))) a
--    collidewall = collidesFigureSidesWallRight (figureToDraw (Figure s t (b + blockSize,c,z))) a
collidesBlock::Coord -> Bool
collidesBlock (a,b,z) | (a < 0) || (a  + blockSize > screenWidth) || (b < 0) || (b + blockSize > screenHeight) = True
       |otherwise = False


collidesBlockSides::Coord -> Board -> Bool
collidesBlockSides (a,b,z) [] = (a < 0) || (a  + blockSize > screenWidth)
collidesBlockSides (a,b,z) ((brda, brdb,z1):[]) = (a < 0) || (a  + blockSize > screenWidth) || (a==brda) && (b==brdb)
collidesBlockSides (a,b,z) ((brda, brdb,z1):brds) | (a < 0) || (a  + blockSize > screenWidth) || (a==brda) && (b==brdb)  = True
                                             | otherwise = collidesBlockSides (a,b,z) brds
collidesBlockSidesWhallLeft::Coord -> Board -> Bool
collidesBlockSidesWhallLeft (a,b,z) _ = (a  - blockSize <(-30))
collidesBlockSidesWhallRight::Coord -> Board -> Bool
collidesBlockSidesWhallRight (a,b,z) _ =  (a  + blockSize > screenWidth)

collidesBlockSidesSmooth::Coord -> Board -> Bool
collidesBlockSidesSmooth (a,b,z) [] = (a < 0) || (a  + blockSize > screenWidth)
collidesBlockSidesSmooth (a,b,z) ((brda, brdb,z1):[]) = (a < 0) || (a  + blockSize > screenWidth) || (a==brda) && (b==brdb)||((a==brda) &&(b>(brdb - blockSize) && b<(brdb + blockSize)))
collidesBlockSidesSmooth (a,b,z) ((brda, brdb,z1):brds) | (a < 0) || (a  + blockSize > screenWidth) || (a==brda) && (b==brdb)||((a==brda) &&(b>(brdb - blockSize) && b<(brdb + blockSize)))  = True
                                             | otherwise = collidesBlockSides (a,b,z) brds

collidesBlockDown::Coord -> Board-> Bool
collidesBlockDown (a,b,z) []  =   (b + blockSize > screenHeight)
collidesBlockDown (a,b,z) ((brda,brdb,z1):[])  =   ((b + blockSize > screenHeight) || (a==brda) && (b==brdb))
collidesBlockDown (a,b,z) ((brda,brdb,z1):brds)  | (b + blockSize > screenHeight) || (a==brda) && (b==brdb)  = True
                                            |  otherwise = collidesBlockDown (a,b,z) brds
collidesBlockDownSmooth::Coord -> Board-> Bool
collidesBlockDownSmooth (a,b,z) []  =   (b  > screenHeight)|| (b < 0)
collidesBlockDownSmooth (a,b,z) ((brda,brdb,z1):[])  =   ((b  > screenHeight) || (a ==brda) && ((b )==brdb))|| (b < 0)
collidesBlockDownSmooth (a,b,z) ((brda,brdb,z1):brds)  | (b > screenHeight) || (a ==brda) && ((b ) ==brdb)|| (b < 0)  = True
                                            |  otherwise = collidesBlockDownSmooth (a,b,z) brds

collidesBlockUp::Coord -> Board-> Bool
collidesBlockUp (a,b,z) []  =  b < 0
collidesBlockUp (a,b,z) ((brda,brdb,z1):[])  =   (b < 0 && (b==brdb))
collidesBlockUp (a,b,z) ((brda,brdb,z1):brds)  | b < 0 && (b==brdb)  = True
                                          |  otherwise = collidesBlockUp (a,b,z) brds


collidesFigure::BlockedFigure -> Board -> Bool
collidesFigure (a,b,c,d) board = (collidesFigureSides (a,b,c,d) board) || (collidesFigureDown (a,b,c,d) board)

collidesFigureSmooth::BlockedFigure -> Board -> Bool
collidesFigureSmooth (a,b,c,d) board = (collidesFigureSidesSmooth (a,b,c,d) board) || (collidesFigureDownSmooth (a,b,c,d) board)

collidesFigureSides::BlockedFigure -> Board -> Bool
collidesFigureSides (a,b,c,d) board | (collidesBlockSides a board) || (collidesBlockSides b board) || (collidesBlockSides c board) || (collidesBlockSides d board) = True
        |otherwise = False
collidesFigureSidesWallLeft    ::BlockedFigure -> Board -> Bool
collidesFigureSidesWallLeft  (a,b,c,d) board | (collidesBlockSidesWhallLeft a board) || (collidesBlockSidesWhallLeft b board) || (collidesBlockSidesWhallLeft c board) || (collidesBlockSidesWhallLeft d board) = True
        |otherwise = False
collidesFigureSidesWallRight::BlockedFigure -> Board -> Bool
collidesFigureSidesWallRight (a,b,c,d) board | (collidesBlockSidesWhallRight a board) || (collidesBlockSidesWhallRight b board) || (collidesBlockSidesWhallRight c board) || (collidesBlockSidesWhallRight d board) = True
        |otherwise = False
collidesFigureSidesSmooth::BlockedFigure -> Board -> Bool
collidesFigureSidesSmooth (a,b,c,d) board | (collidesBlockSidesSmooth a board) || (collidesBlockSidesSmooth b board) || (collidesBlockSidesSmooth c board) || (collidesBlockSidesSmooth d board) = True
        |otherwise = False        


collidesFigureDown::BlockedFigure -> Board -> Bool
collidesFigureDown (a,b,c,d) board | (collidesBlockDown a board) || (collidesBlockDown b board) || (collidesBlockDown c board) || (collidesBlockDown d board) = True
        |otherwise = False
collidesFigureDownSmooth::BlockedFigure -> Board -> Bool
collidesFigureDownSmooth (a,b,c,d) board | (collidesBlockDownSmooth a board) || (collidesBlockDownSmooth b board) || (collidesBlockDownSmooth c board) || (collidesBlockDownSmooth d board) = True
        |otherwise = False
isGameOver::Gamestate -> Bool
isGameOver (a,(f1:f2:rest),d,e,v,TetrisStepped,k) = collidesFigureDown (figureToDraw f2) a
isGameOver (a,(f1:f2:rest),d,e,v,TetrisSmooth,k) = collidesFigureDownSmooth (figureToDraw f1) a




sortRows :: Board -> Board
sortRows []     = []
sortRows ((brda,brdb,z):brds) = sortRows (filter (\(x,y,z) -> y > brdb) brds) ++ [(brda,brdb,z)] ++ sortRows (filter (\(x,y,z) -> y <= brdb) brds)


deleteRows :: Board -> Board
deleteRows [] = []
deleteRows ((brda,brdb,z):brds) | (length (filter (\(x,y,z) -> brdb == y) ((brda,brdb,z):brds)) == 10)  =  (deleteRows (map (\(x,y,z) -> (x, y + blockSize,z)) (filter (\(x,y,z) -> y < brdb) l)) ++ (filter (\(x,y,z) -> y > brdb) l))
                              | otherwise = (filter (\(x,y,z) -> brdb == y) ((brda,brdb,z):brds)) ++ (deleteRows  (filter (\(x,y,z) -> brdb /= y) ((brda,brdb,z):brds)))                  -----   ToDo:   Обработать левый операнд аппенда.  После функции проверить, что между У нет зазоров.
                         where l = (filter (\(x,y,z) -> brdb /= y) ((brda,brdb,z):brds))

--При нажатии клавиши "вниз" роняет фигуру 


dropit::Gamestate -> Int -> GameState
dropit (a,((Figure sha dir (b,c,z)):rest),d,e,v,TetrisStepped,k) pts  | collide = (toGS(a,((Figure sha dir (b,c,z)):rest),d,e+(div pts blockSize),v,TetrisStepped,k))                   
                                                  | otherwise = dropit (a,((Figure sha dir (b,c + blockSize,z)):rest),d,e,v,TetrisStepped,k) pts                                        
                                          where                                           
                                              collide = collidesFigureDown (figureToDraw (Figure sha dir (b,c + blockSize,z))) a
dropit (a,((Figure sha dir (b,c,z)):rest),d,e,v,TetrisSmooth,k) pts  | collide = (toGS(a,((Figure sha dir (b,c,z)):rest),d,e+(div pts blockSize),v,TetrisSmooth,k))                   
                                                  | otherwise = dropit (a,((Figure sha dir (b,c + 1,z)):rest),d,e,v,TetrisSmooth,k) pts                                        
                                          where                                           
                                              collide = collidesFigureDownSmooth (figureToDraw (Figure sha dir (b,c + blockSize,z))) a                                              


drawBoard::Board  -> Picture
drawBoard s = pictures (map drawBlock s)
drawBoardCircle :: Board -> Picture
drawBoardCircle s = pictures (map drawBlockCircle s)
--(b==(324)||b==323||b == 322||b == 325||b == 326||b==321||b == 327||b==288||b==0  )

angle :: Float
angle = 36

blockSizeFloat :: Float
blockSizeFloat = 30

specangel :: Float
specangel = 324


offset2 :: Float
offset2 = 100

myscale :: Float
myscale = 2.3

sizefit ::Float
sizefit = 5

sizefitInt ::Int
sizefitInt = 5

offsedge ::Int
offsedge = 15



drawBlockCircle :: Coord-> Picture
drawBlockCircle  (b,c,1) |(b==0)=   pictures [ translate (-w) (h - offset2) (scale  myscale myscale (pictures
 [
  color blue  (thickArc (fromIntegral (0  )) ( (angle  )) (fromIntegral (c + sizefitInt) /sizefit -2 ) (fromIntegral 6) ) ,
  color magenta  (thickArc (fromIntegral (0  )) ( (angle  )) (fromIntegral (c + sizefitInt +offsedge) / sizefit -2 ) (fromIntegral 1) ),
  color magenta  (thickArc (fromIntegral (0  )) ( (angle  )) (fromIntegral (c + sizefitInt - offsedge) / sizefit -2 ) (fromIntegral 1) ),
  color magenta  (thickArc (fromIntegral (0  )) (fromIntegral (1  )) (fromIntegral (c + sizefitInt ) / sizefit -2 ) (fromIntegral 6) ) ,
  color magenta  (thickArc ( (angle - 1  )) ( (angle  )) (fromIntegral (c + sizefitInt ) / sizefit -2 ) (fromIntegral 6) )             -- белая рамка
   

   ]))
    ]
                    |(b==270) = 
  pictures [ translate (-w) (h - offset2) (scale  myscale myscale  ( pictures[ 
    (rotate (-specangel) (color blue   (thickArc (0) (angle) (fromIntegral (c + sizefitInt) / sizefit  - 2) (fromIntegral 6) ))),
   (rotate (-specangel) (color magenta  (thickArc (fromIntegral (0  )) ( (angle  )) (fromIntegral (c + sizefitInt +offsedge) / sizefit -2 ) (fromIntegral 1) ))),
   (rotate (-specangel) (color magenta  (thickArc (fromIntegral (0  )) ( (angle  )) (fromIntegral (c + sizefitInt-offsedge) / sizefit -2 ) (fromIntegral 1) )) ),
   (rotate (-specangel) (color magenta   (thickArc (0) (1) (fromIntegral (c + sizefitInt) / sizefit  - 2) (fromIntegral 6) ))),
    (rotate (-specangel) (color magenta   (thickArc (angle - 1) (angle) (fromIntegral (c + sizefitInt) / sizefit  - 2) (fromIntegral 6) ))) ]))
    ]                 
                   |otherwise = pictures [ translate (-w) (h - offset2) (scale  myscale myscale  (pictures
 [ 
  color blue   (thickArc (((fromIntegral b)/blockSizeFloat)*angle) (((fromIntegral b)/blockSizeFloat)*angle + angle) (fromIntegral (c + sizefitInt) / sizefit  - 2) (fromIntegral 6) ) ,            -- белая рамка
   color magenta   (thickArc (((fromIntegral b)/blockSizeFloat)*angle) (((fromIntegral b)/blockSizeFloat)*angle + angle) (fromIntegral (c + sizefitInt+offsedge) / sizefit  - 2) (fromIntegral 1) ) ,
   color magenta   (thickArc (((fromIntegral b)/blockSizeFloat)*angle) (((fromIntegral b)/blockSizeFloat)*angle + angle) (fromIntegral (c + sizefitInt-offsedge) / sizefit  - 2) (fromIntegral 1) ) ,

   color magenta   (thickArc (((fromIntegral b)/blockSizeFloat)*angle ) (((fromIntegral b)/blockSizeFloat)*angle + 1) (fromIntegral (c + sizefitInt) / sizefit  - 2) (fromIntegral 6) ) ,
   color magenta   (thickArc (((fromIntegral b)/blockSizeFloat)*angle +35) (((fromIntegral b)/blockSizeFloat)*angle + angle) (fromIntegral (c + sizefitInt) / sizefit  - 2) (fromIntegral 6) )
   ]))
    ]
  where
  w = fromIntegral 0
  h = fromIntegral 0
drawBlockCircle  (b,c,2) |(b==0)=   pictures [ translate (-w) (h - offset2) (scale  myscale myscale (pictures
 [
  color yellow  (thickArc (fromIntegral (0  )) ( (angle  )) (fromIntegral (c + sizefitInt) / sizefit -2 ) (fromIntegral 6) ) ,
  color magenta  (thickArc (fromIntegral (0  )) ( (angle  )) (fromIntegral (c + sizefitInt +offsedge) / sizefit -2 ) (fromIntegral 1) ),
  color magenta  (thickArc (fromIntegral (0  )) ( (angle  )) (fromIntegral (c + sizefitInt -offsedge) / sizefit -2 ) (fromIntegral 1) ),
  color magenta  (thickArc (fromIntegral (0  )) (fromIntegral (1  )) (fromIntegral (c + sizefitInt ) /sizefit -2 ) (fromIntegral 6) ) ,
  color magenta  (thickArc ( (angle - 1  )) ( (angle  )) (fromIntegral (c + sizefitInt ) / sizefit -2 ) (fromIntegral 6) )             -- белая рамка
   

   ]))
    ]
                    |(b==270) = 
  pictures [ translate (-w) (h - offset2) (scale  myscale myscale  ( pictures[ 
    (rotate (-specangel) (color yellow   (thickArc (0) (angle) (fromIntegral (c + sizefitInt) / sizefit  - 2) (fromIntegral 6) ))),
   (rotate (-specangel) (color magenta  (thickArc (fromIntegral (0  )) ( (angle  )) (fromIntegral (c + sizefitInt +offsedge) / sizefit -2 ) (fromIntegral 1) ))),
   (rotate (-specangel) (color magenta  (thickArc (fromIntegral (0  )) ( (angle  )) (fromIntegral (c + sizefitInt-offsedge) / sizefit -2 ) (fromIntegral 1) )) ),
   (rotate (-specangel) (color magenta   (thickArc (0) (1) (fromIntegral (c + sizefitInt) / sizefit  - 2) (fromIntegral 6) ))),
    (rotate (-specangel) (color magenta   (thickArc (angle - 1) (angle) (fromIntegral (c + sizefitInt) / sizefit  - 2) (fromIntegral 6) ))) ]))
    ]                 
                   |otherwise = pictures [ translate (-w) (h - offset2) (scale  myscale myscale  (pictures
 [ 
  color yellow   (thickArc (((fromIntegral b)/blockSizeFloat)*angle) (((fromIntegral b)/blockSizeFloat)*angle + angle) (fromIntegral (c + sizefitInt) /sizefit  - 2) (fromIntegral 6) ) ,            -- белая рамка
   color magenta   (thickArc (((fromIntegral b)/blockSizeFloat)*angle) (((fromIntegral b)/blockSizeFloat)*angle + angle) (fromIntegral (c + sizefitInt+offsedge) / sizefit  - 2) (fromIntegral 1) ) ,
   color magenta   (thickArc (((fromIntegral b)/blockSizeFloat)*angle) (((fromIntegral b)/blockSizeFloat)*angle + angle) (fromIntegral (c + sizefitInt-offsedge) / sizefit  - 2) (fromIntegral 1) ) ,

   color magenta   (thickArc (((fromIntegral b)/blockSizeFloat)*angle ) (((fromIntegral b)/blockSizeFloat)*angle + 1) (fromIntegral (c + sizefitInt) / sizefit  - 2) (fromIntegral 6) ) ,
   color magenta   (thickArc (((fromIntegral b)/blockSizeFloat)*angle +angle - 1) (((fromIntegral b)/blockSizeFloat)*angle + angle) (fromIntegral (c + sizefitInt) / sizefit  - 2) (fromIntegral 6) )
   ]))
    ]
  where
  w = fromIntegral 0
  h = fromIntegral 0
drawBlockCircle  (b,c,3) |(b==0)=   pictures [ translate (-w) (h - offset2) (scale  myscale myscale (pictures
 [ 
  color red  (thickArc (fromIntegral (0  )) ((angle  )) (fromIntegral (c + sizefitInt) / sizefit -2 ) (fromIntegral 6) ) ,
  color magenta  (thickArc (fromIntegral (0  )) ( (angle  )) (fromIntegral (c + sizefitInt +offsedge) / sizefit -2 ) (fromIntegral 1) ),
  color magenta  (thickArc (fromIntegral (0  )) ( (angle  )) (fromIntegral (c + sizefitInt - offsedge) / sizefit -2 ) (fromIntegral 1) ),
  color magenta  (thickArc (fromIntegral (0  )) (fromIntegral (1  )) (fromIntegral (c + sizefitInt ) /sizefit -2 ) (fromIntegral 6) ) ,
  color magenta  (thickArc ( (angle - 1  )) ( (angle  )) (fromIntegral (c + sizefitInt ) / sizefit -2 ) (fromIntegral 6) )             -- белая рамка
   

   ]))
    ]
                    |(b==270) = 
  pictures [ translate (-w) (h - offset2) (scale  myscale myscale  ( pictures[ 
    (rotate (-specangel) (color red   (thickArc (0) (angle) (fromIntegral (c + sizefitInt) / sizefit  - 2) (fromIntegral 6) ))),
   (rotate (-specangel) (color magenta  (thickArc (fromIntegral (0  )) ( (angle  )) (fromIntegral (c + sizefitInt +offsedge) / sizefit -2 ) (fromIntegral 1) ))),
   (rotate (-specangel) (color magenta  (thickArc (fromIntegral (0  )) ( (angle  )) (fromIntegral (c + sizefitInt-offsedge) / sizefit -2 ) (fromIntegral 1) )) ),
   (rotate (-specangel) (color magenta   (thickArc (0) (1) (fromIntegral (c + sizefitInt) / sizefit  - 2) (fromIntegral 6) ))),
    (rotate (-specangel) (color magenta   (thickArc (angle - 1) (angle) (fromIntegral (c + sizefitInt) / sizefit  - 2) (fromIntegral 6) ))) ]))
    ]                 
                   |otherwise = pictures [ translate (-w) (h - offset2) (scale  myscale myscale  (pictures
 [ 
  color red   (thickArc (((fromIntegral b)/blockSizeFloat)*angle) (((fromIntegral b)/blockSizeFloat)*angle + angle) (fromIntegral (c + sizefitInt) / sizefit  - 2) (fromIntegral 6) ) ,            -- белая рамка
   color magenta   (thickArc (((fromIntegral b)/blockSizeFloat)*angle) (((fromIntegral b)/blockSizeFloat)*angle + angle) (fromIntegral (c + sizefitInt+offsedge) / sizefit  - 2) (fromIntegral 1) ) ,
   color magenta   (thickArc (((fromIntegral b)/blockSizeFloat)*angle) (((fromIntegral b)/blockSizeFloat)*angle + angle) (fromIntegral (c + sizefitInt-offsedge) /sizefit  - 2) (fromIntegral 1) ) ,

   color magenta   (thickArc (((fromIntegral b)/blockSizeFloat)*angle ) (((fromIntegral b)/blockSizeFloat)*angle + 1) (fromIntegral (c + sizefitInt) / sizefit  - 2) (fromIntegral 6) ) ,
   color magenta   (thickArc (((fromIntegral b)/blockSizeFloat)*angle +angle - 1) (((fromIntegral b)/blockSizeFloat)*angle + angle) (fromIntegral (c + sizefitInt) / sizefit  - 2) (fromIntegral 6) )
   ]))
    ]
  where
  w = fromIntegral 0
  h = fromIntegral 0
drawBlockCircle  (b,c,4) |(b==0)=   pictures [ translate (-w) (h - offset2) (scale  myscale myscale (pictures
 [ 
  color green  (thickArc (fromIntegral (0  )) ( (angle  )) (fromIntegral (c + sizefitInt) / sizefit -2 ) (fromIntegral 6) ) ,
  color magenta  (thickArc (fromIntegral (0  )) ( (angle  )) (fromIntegral (c + sizefitInt +offsedge) / sizefit -2 ) (fromIntegral 1) ),
  color magenta  (thickArc (fromIntegral (0  )) ( (angle  )) (fromIntegral (c + sizefitInt - offsedge) / sizefit -2 ) (fromIntegral 1) ),
  color magenta  (thickArc (fromIntegral (0  )) (fromIntegral (1  )) (fromIntegral (c + sizefitInt ) / sizefit -2 ) (fromIntegral 6) ) ,
  color magenta  (thickArc ( (angle - 1  )) ( (angle  )) (fromIntegral (c + sizefitInt ) /sizefit -2 ) (fromIntegral 6) )             -- белая рамка
   

   ]))
    ]
                    |(b==270) = 
  pictures [ translate (-w) (h - offset2) (scale  myscale myscale  ( pictures[ 
    (rotate (-specangel) (color green   (thickArc (0) (angle) (fromIntegral (c + sizefitInt) / sizefit  - 2) (fromIntegral 6) ))),
   (rotate (-specangel) (color magenta  (thickArc (fromIntegral (0  )) ( (angle  )) (fromIntegral (c + sizefitInt +offsedge) / sizefit -2 ) (fromIntegral 1) ))),
   (rotate (-specangel) (color magenta  (thickArc (fromIntegral (0  )) ( (angle  )) (fromIntegral (c + sizefitInt-offsedge) / sizefit -2 ) (fromIntegral 1) )) ),
   (rotate (-specangel) (color magenta   (thickArc (0) (1) (fromIntegral (c + sizefitInt) / sizefit  - 2) (fromIntegral 6) ))),
    (rotate (-specangel) (color magenta   (thickArc (angle - 1) (angle) (fromIntegral (c + sizefitInt) / sizefit  - 2) (fromIntegral 6) ))) ]))
    ]                 
                   |otherwise = pictures [ translate (-w) (h - offset2) (scale  myscale myscale  (pictures
 [
  color green   (thickArc (((fromIntegral b)/blockSizeFloat)*angle) (((fromIntegral b)/blockSizeFloat)*angle + angle) (fromIntegral (c + sizefitInt) / sizefit  - 2) (fromIntegral 6) ) ,            -- белая рамка
   color magenta   (thickArc (((fromIntegral b)/blockSizeFloat)*angle) (((fromIntegral b)/blockSizeFloat)*angle + angle) (fromIntegral (c + sizefitInt+offsedge) / sizefit  - 2) (fromIntegral 1) ) ,
   color magenta   (thickArc (((fromIntegral b)/blockSizeFloat)*angle) (((fromIntegral b)/blockSizeFloat)*angle + angle) (fromIntegral (c + sizefitInt-offsedge) / sizefit  - 2) (fromIntegral 1) ) ,

   color magenta   (thickArc (((fromIntegral b)/blockSizeFloat)*angle ) (((fromIntegral b)/blockSizeFloat)*angle + 1) (fromIntegral (c + sizefitInt) /sizefit  - 2) (fromIntegral 6) ) ,
   color magenta   (thickArc (((fromIntegral b)/blockSizeFloat)*angle +35) (((fromIntegral b)/blockSizeFloat)*angle + angle) (fromIntegral (c + sizefitInt) / sizefit  - 2) (fromIntegral 6) )
   ]))
    ]
  where
  w = fromIntegral 0
  h = fromIntegral 0
drawBlockCircle  (b,c,5) |(b==0)=   pictures [ translate (-w) (h - offset2) (scale  myscale myscale (pictures
 [
  color orange  (thickArc (fromIntegral (0  )) ( (angle  )) (fromIntegral (c + sizefitInt) / sizefit -2 ) (fromIntegral 6) ) ,
  color magenta  (thickArc (fromIntegral (0  )) ( (angle  )) (fromIntegral (c + sizefitInt +offsedge) / sizefit -2 ) (fromIntegral 1) ),
  color magenta  (thickArc (fromIntegral (0  )) ( (angle  )) (fromIntegral (c + sizefitInt - offsedge) / sizefit -2 ) (fromIntegral 1) ),
  color magenta  (thickArc (fromIntegral (0  )) (fromIntegral (1  )) (fromIntegral (c + sizefitInt ) / sizefit -2 ) (fromIntegral 6) ) ,
  color magenta  (thickArc ( (angle - 1  )) ( (angle  )) (fromIntegral (c + sizefitInt ) / sizefit -2 ) (fromIntegral 6) )             -- белая рамка
   

   ]))
    ]
                    |(b==270) = 
  pictures [ translate (-w) (h - offset2) (scale  myscale myscale  ( pictures[ 
    (rotate (-specangel) (color orange   (thickArc (0) (angle) (fromIntegral (c + sizefitInt) / sizefit  - 2) (fromIntegral 6) ))),
   (rotate (-specangel) (color magenta  (thickArc (fromIntegral (0  )) ( (angle  )) (fromIntegral (c + sizefitInt +offsedge) / sizefit -2 ) (fromIntegral 1) ))),
   (rotate (-specangel) (color magenta  (thickArc (fromIntegral (0  )) ( (angle  )) (fromIntegral (c +sizefitInt-offsedge) / sizefit -2 ) (fromIntegral 1) )) ),
   (rotate (-specangel) (color magenta   (thickArc (0) (1) (fromIntegral (c + sizefitInt) /sizefit  - 2) (fromIntegral 6) ))),
    (rotate (-specangel) (color magenta   (thickArc (angle - 1) (angle) (fromIntegral (c + sizefitInt) / sizefit  - 2) (fromIntegral 6) ))) ]))
    ]                 
                   |otherwise = pictures [ translate (-w) (h - offset2) (scale  myscale myscale  (pictures
 [
  color orange   (thickArc (((fromIntegral b)/blockSizeFloat)*angle) (((fromIntegral b)/blockSizeFloat)*angle + angle) (fromIntegral (c + sizefitInt) / sizefit  - 2) (fromIntegral 6) ) ,            -- белая рамка
   color magenta   (thickArc (((fromIntegral b)/blockSizeFloat)*angle) (((fromIntegral b)/blockSizeFloat)*angle + angle) (fromIntegral (c + sizefitInt+offsedge) / sizefit  - 2) (fromIntegral 1) ) ,
   color magenta   (thickArc (((fromIntegral b)/blockSizeFloat)*angle) (((fromIntegral b)/blockSizeFloat)*angle + angle) (fromIntegral (c + sizefitInt-offsedge) / sizefit  - 2) (fromIntegral 1) ) ,

   color magenta   (thickArc (((fromIntegral b)/blockSizeFloat)*angle ) (((fromIntegral b)/blockSizeFloat)*angle + 1) (fromIntegral (c + sizefitInt) / sizefit  - 2) (fromIntegral 6) ) ,
   color magenta   (thickArc (((fromIntegral b)/blockSizeFloat)*angle +angle - 1) (((fromIntegral b)/blockSizeFloat)*angle + angle) (fromIntegral (c + sizefitInt) /sizefit  - 2) (fromIntegral 6) )
   ]))
    ]
  where
  w = fromIntegral 0
  h = fromIntegral 0
drawBlockCircle  (b,c,_) |(b==0)=   pictures [ translate (-w) (h - offset2) (scale  myscale myscale (pictures
 [
  color white  (thickArc (fromIntegral (0  )) ( (angle  )) (fromIntegral (c + sizefitInt) /sizefit -2 ) (fromIntegral 6) ) ,
  color magenta  (thickArc (fromIntegral (0  )) ( (angle  )) (fromIntegral (c + sizefitInt +offsedge) / sizefit -2 ) (fromIntegral 1) ),
  color magenta  (thickArc (fromIntegral (0  )) ( (angle  )) (fromIntegral (c + sizefitInt - offsedge) / sizefit -2 ) (fromIntegral 1) ),
  color magenta  (thickArc (fromIntegral (0  )) ( (1  )) (fromIntegral (c + sizefitInt ) / sizefit -2 ) (fromIntegral 6) ) ,
  color magenta  (thickArc ( (angle - 1  )) ( (angle  )) (fromIntegral (c + sizefitInt ) / sizefit -2 ) (fromIntegral 6) )             -- белая рамка
   

   ]))
    ]
                    |(b==270) = 
  pictures [ translate (-w) (h - offset2) (scale  myscale myscale  ( pictures[ 
    (rotate (-specangel) (color white   (thickArc (0) (angle) (fromIntegral (c + sizefitInt) / sizefit  - 2) (fromIntegral 6) ))),
   (rotate (-specangel) (color magenta  (thickArc (fromIntegral (0  )) ( (angle  )) (fromIntegral (c + sizefitInt +offsedge) / sizefit -2 ) (fromIntegral 1) ))),
   (rotate (-specangel) (color magenta  (thickArc (fromIntegral (0  )) ( (angle  )) (fromIntegral (c + sizefitInt-offsedge) /sizefit -2 ) (fromIntegral 1) )) ),
   (rotate (-specangel) (color magenta   (thickArc (0) (1) (fromIntegral (c + sizefitInt) / sizefit  - 2) (fromIntegral 6) ))),
    (rotate (-specangel) (color magenta   (thickArc (angle - 1) (angle) (fromIntegral (c + sizefitInt) / sizefit  - 2) (fromIntegral 6) ))) ]))
    ]                 
                   |otherwise = pictures [ translate (-w) (h - offset2) (scale  myscale myscale  (pictures
 [ 
  color white   (thickArc (((fromIntegral b)/blockSizeFloat)*angle) (((fromIntegral b)/blockSizeFloat)*angle + angle) (fromIntegral (c + sizefitInt) / sizefit  - 2) (fromIntegral 6) ) ,            -- белая рамка
   color magenta   (thickArc (((fromIntegral b)/blockSizeFloat)*angle) (((fromIntegral b)/blockSizeFloat)*angle + angle) (fromIntegral (c + sizefitInt+offsedge) / sizefit  - 2) (fromIntegral 1) ) ,
   color magenta   (thickArc (((fromIntegral b)/blockSizeFloat)*angle) (((fromIntegral b)/blockSizeFloat)*angle + angle) (fromIntegral (c + sizefitInt-offsedge) / sizefit  - 2) (fromIntegral 1) ) ,

   color magenta   (thickArc (((fromIntegral b)/blockSizeFloat)*angle ) (((fromIntegral b)/blockSizeFloat)*angle + 1) (fromIntegral (c + sizefitInt) / sizefit  - 2) (fromIntegral 6) ) ,
   color magenta   (thickArc (((fromIntegral b)/blockSizeFloat)*angle +angle - 1) (((fromIntegral b)/blockSizeFloat)*angle + angle) (fromIntegral (c + sizefitInt) / sizefit  - 2) (fromIntegral 6) )
   ]))
    ]
  where
  w = fromIntegral 0
  h = fromIntegral 0
drawBlock :: Coord-> Picture

drawBlock  (b,c,1) =  pictures [ translate (-w) h (scale  1 1 (pictures
 [ color blue  (polygon [ ( fromIntegral b, fromIntegral (-c)), (fromIntegral b, fromIntegral (-c - blockSize)), (fromIntegral  (b + blockSize),fromIntegral (-c - blockSize)), (fromIntegral  (b + blockSize),fromIntegral (- c)) ])            -- белая рамка
   ,color magenta  (polygon [ ( fromIntegral b, fromIntegral (-c)), (fromIntegral b, fromIntegral (-c - 2)), (fromIntegral  (b + blockSize),fromIntegral (-c - 2)), (fromIntegral  (b + blockSize),fromIntegral (- c)) ])
   ,color magenta  (polygon [ ( fromIntegral b, fromIntegral (-c)), (fromIntegral b, fromIntegral (-c - blockSize)), (fromIntegral  (b + 2),fromIntegral (-c-blockSize )), (fromIntegral  (b +2),fromIntegral (- c)) ])
   ,color magenta  (polygon [ ( fromIntegral b, fromIntegral (-c-(blockSize - 2))), (fromIntegral b, fromIntegral (-c - blockSize)), (fromIntegral  (b + blockSize),fromIntegral (-c - blockSize)), (fromIntegral  (b + blockSize),fromIntegral (- c-28)) ])
   ,color magenta  (polygon [ ( fromIntegral b+(blockSizeFloat - 2), fromIntegral (-c)), (fromIntegral b+(blockSizeFloat - 2), fromIntegral (-c - blockSize)), (fromIntegral  (b + blockSize),fromIntegral (-c - blockSize)), (fromIntegral  (b + blockSize),fromIntegral (- c)) ])

   ]))
    ]
  where
  w = fromIntegral screenWidth  / 2
  h = fromIntegral screenHeight / 2
drawBlock  (b,c,2) =  pictures [ translate (-w) h (scale  1 1 (pictures
 [ color yellow  (polygon [ ( fromIntegral b, fromIntegral (-c)), (fromIntegral b, fromIntegral (-c - blockSize)), (fromIntegral  (b + blockSize),fromIntegral (-c - blockSize)), (fromIntegral  (b + blockSize),fromIntegral (- c)) ])            -- белая рамка
   ,color magenta  (polygon [ ( fromIntegral b, fromIntegral (-c)), (fromIntegral b, fromIntegral (-c - 2)), (fromIntegral  (b + blockSize),fromIntegral (-c - 2)), (fromIntegral  (b + blockSize),fromIntegral (- c)) ])
   ,color magenta  (polygon [ ( fromIntegral b, fromIntegral (-c)), (fromIntegral b, fromIntegral (-c - blockSize)), (fromIntegral  (b + 2),fromIntegral (-c-blockSize )), (fromIntegral  (b +2),fromIntegral (- c)) ])
   ,color magenta  (polygon [ ( fromIntegral b, fromIntegral (-c-(blockSize - 2))), (fromIntegral b, fromIntegral (-c - blockSize)), (fromIntegral  (b +blockSize),fromIntegral (-c - blockSize)), (fromIntegral  (b + blockSize),fromIntegral (- c-28)) ])
   ,color magenta  (polygon [ ( fromIntegral b+(blockSizeFloat - 2), fromIntegral (-c)), (fromIntegral b+(blockSizeFloat - 2), fromIntegral (-c - blockSize)), (fromIntegral  (b + blockSize),fromIntegral (-c - blockSize)), (fromIntegral  (b + blockSize),fromIntegral (- c)) ])
   ]))]
  where
  w = fromIntegral screenWidth  / 2
  h = fromIntegral screenHeight / 2
drawBlock  (b,c,3) =  pictures [ translate (-w) h (scale  1 1 (pictures
 [ color red  (polygon [ ( fromIntegral b, fromIntegral (-c)), (fromIntegral b, fromIntegral (-c - blockSize)), (fromIntegral  (b + blockSize),fromIntegral (-c - blockSize)), (fromIntegral  (b + blockSize),fromIntegral (- c)) ])            -- белая рамка
   ,color magenta  (polygon [ ( fromIntegral b, fromIntegral (-c)), (fromIntegral b, fromIntegral (-c - 2)), (fromIntegral  (b + blockSize),fromIntegral (-c - 2)), (fromIntegral  (b + blockSize),fromIntegral (- c)) ])
   ,color magenta  (polygon [ ( fromIntegral b, fromIntegral (-c)), (fromIntegral b, fromIntegral (-c - blockSize)), (fromIntegral  (b + 2),fromIntegral (-c-blockSize )), (fromIntegral  (b +2),fromIntegral (- c)) ])
   ,color magenta  (polygon [ ( fromIntegral b, fromIntegral (-c-(blockSize - 2))), (fromIntegral b, fromIntegral (-c -blockSize)), (fromIntegral  (b + blockSize),fromIntegral (-c - blockSize)), (fromIntegral  (b + blockSize),fromIntegral (- c-28)) ])
   ,color magenta  (polygon [ ( fromIntegral b+(blockSizeFloat - 2), fromIntegral (-c)), (fromIntegral b+(blockSizeFloat - 2), fromIntegral (-c - blockSize)), (fromIntegral  (b + blockSize),fromIntegral (-c - blockSize)), (fromIntegral  (b + blockSize),fromIntegral (- c)) ])
   ]))]
  where
  w = fromIntegral screenWidth  / 2
  h = fromIntegral screenHeight / 2
drawBlock  (b,c,4) =  pictures [ translate (-w) h (scale  1 1 (pictures
 [ color green  (polygon [ ( fromIntegral b, fromIntegral (-c)), (fromIntegral b, fromIntegral (-c - blockSize)), (fromIntegral  (b + blockSize),fromIntegral (-c - blockSize)), (fromIntegral  (b + blockSize),fromIntegral (- c)) ])            -- белая рамка
   ,color magenta  (polygon [ ( fromIntegral b, fromIntegral (-c)), (fromIntegral b, fromIntegral (-c - 2)), (fromIntegral  (b + blockSize),fromIntegral (-c - 2)), (fromIntegral  (b + blockSize),fromIntegral (- c)) ])
   ,color magenta  (polygon [ ( fromIntegral b, fromIntegral (-c)), (fromIntegral b, fromIntegral (-c - blockSize)), (fromIntegral  (b + 2),fromIntegral (-c-blockSize )), (fromIntegral  (b +2),fromIntegral (- c)) ])
   ,color magenta  (polygon [ ( fromIntegral b, fromIntegral (-c-(blockSize - 2))), (fromIntegral b, fromIntegral (-c - blockSize)), (fromIntegral  (b + blockSize),fromIntegral (-c - blockSize)), (fromIntegral  (b + blockSize),fromIntegral (- c-28)) ])
   ,color magenta  (polygon [ ( fromIntegral b+(blockSizeFloat - 2), fromIntegral (-c)), (fromIntegral b+(blockSizeFloat - 2), fromIntegral (-c - blockSize)), (fromIntegral  (b + blockSize),fromIntegral (-c - blockSize)), (fromIntegral  (b + blockSize),fromIntegral (- c)) ])
   ]))]
  where
  w = fromIntegral screenWidth  / 2
  h = fromIntegral screenHeight / 2
drawBlock  (b,c,5) =  pictures [ translate (-w) h (scale  1 1 (pictures
 [ color orange  (polygon [ ( fromIntegral b, fromIntegral (-c)), (fromIntegral b, fromIntegral (-c - blockSize)), (fromIntegral  (b + blockSize),fromIntegral (-c - blockSize)), (fromIntegral  (b + blockSize),fromIntegral (- c)) ])            -- белая рамка
   ,color magenta  (polygon [ ( fromIntegral b, fromIntegral (-c)), (fromIntegral b, fromIntegral (-c - 2)), (fromIntegral  (b + blockSize),fromIntegral (-c - 2)), (fromIntegral  (b + blockSize),fromIntegral (- c)) ])
   ,color magenta  (polygon [ ( fromIntegral b, fromIntegral (-c)), (fromIntegral b, fromIntegral (-c - blockSize)), (fromIntegral  (b + 2),fromIntegral (-c-blockSize )), (fromIntegral  (b +2),fromIntegral (- c)) ])
   ,color magenta  (polygon [ ( fromIntegral b, fromIntegral (-c-(blockSize - 2))), (fromIntegral b, fromIntegral (-c - blockSize)), (fromIntegral  (b + blockSize),fromIntegral (-c - blockSize)), (fromIntegral  (b + blockSize),fromIntegral (- c-28)) ])
   ,color magenta  (polygon [ ( fromIntegral b+(blockSizeFloat - 2), fromIntegral (-c)), (fromIntegral b+(blockSizeFloat - 2), fromIntegral (-c - blockSize)), (fromIntegral  (b + blockSize),fromIntegral (-c - blockSize)), (fromIntegral  (b + blockSize),fromIntegral (- c)) ])
   ]))]
  where
  w = fromIntegral screenWidth  / 2
  h = fromIntegral screenHeight / 2


drawBlock  (b,c,_) =  pictures [ translate (-w) h (scale  1 1 (pictures
 [ color white  (polygon [ ( fromIntegral b, fromIntegral (-c)), (fromIntegral b, fromIntegral (-c - blockSize)), (fromIntegral  (b + blockSize),fromIntegral (-c - blockSize)), (fromIntegral  (b + blockSize),fromIntegral (- c)) ])            -- белая рамка
   ,color magenta  (polygon [ ( fromIntegral b, fromIntegral (-c)), (fromIntegral b, fromIntegral (-c - 2)), (fromIntegral  (b + blockSize),fromIntegral (-c - 2)), (fromIntegral  (b + blockSize),fromIntegral (- c)) ])
   ,color magenta  (polygon [ ( fromIntegral b, fromIntegral (-c)), (fromIntegral b, fromIntegral (-c - blockSize)), (fromIntegral  (b + 2),fromIntegral (-c-blockSize )), (fromIntegral  (b +2),fromIntegral (- c)) ])
   ,color magenta  (polygon [ ( fromIntegral b, fromIntegral (-c-(blockSize - 2))), (fromIntegral b, fromIntegral (-c - blockSize)), (fromIntegral  (b + blockSize),fromIntegral (-c - blockSize)), (fromIntegral  (b + blockSize),fromIntegral (- c-28)) ])
   ,color magenta  (polygon [ ( fromIntegral b+(blockSizeFloat - 2), fromIntegral (-c)), (fromIntegral b+(blockSizeFloat - 2), fromIntegral (-c - blockSize)), (fromIntegral  (b + blockSize),fromIntegral (-c - blockSize)), (fromIntegral  (b + blockSize),fromIntegral (- c)) ])
   ]))]
  where
  w = fromIntegral screenWidth  / 2
  h = fromIntegral screenHeight / 2

drawFigure::GameState  ->  Picture
drawFigure u = drawBlockedFigure  (figureToDraw (getf (figure u) ))



drawFigureCircle::GameState  ->  Picture
drawFigureCircle u = drawBlockedFigureCircle(figureToDraw (getf (figure u) ))

getf ::[Figure]->Figure
getf (f:fs) = f

drawBlockedFigureCircle :: BlockedFigure -> Picture
drawBlockedFigureCircle ((a, b, c, d)) =         pictures  [drawBlockCircle   a ,
                                                     drawBlockCircle    b ,
                                                     drawBlockCircle     c ,
                                                     drawBlockCircle     d ]
drawBlockedFigure::BlockedFigure -> Picture

 
drawBlockedFigure ((a, b, c, d)) =         pictures  [drawBlock   a ,
                                                     drawBlock    b ,
                                                     drawBlock     c ,
                                                     drawBlock     d ]

--Рисуем тетрис
--Пока только рисует квадрат


rect :: Point -> Point -> Picture
rect (l, b) (r, t) = polygon [ (l, b), (l, t), (r, t), (r, b) ]

-- | Прямоугольник с закруглёнными краями и границей заданной толщины.
roundedRect
  :: Color    -- ^ Цвет заливки.
  -> Color    -- ^ Цвет границы.
  -> Float    -- ^ Ширина прямоугольника.
  -> Float    -- ^ Высота прямоугольника.
  -> Float    -- ^ Радиус закругления.
  -> Float    -- ^ Толщина границы.
  -> Picture
roundedRect innerColor borderColor w h r d = pictures
  [ color innerColor inner
  , color borderColor border
  ]
  where
    border = pictures
      [ rect (-w/2 - d/2, -h/2 + r) (-w/2 + d/2, h/2 - r)
      , rect ( w/2 - d/2, -h/2 + r) ( w/2 + d/2, h/2 - r)
      , rect ( w/2 - r, -h/2 + d/2) (-w/2 + r, -h/2 - d/2)
      , rect ( w/2 - r,  h/2 + d/2) (-w/2 + r,  h/2 - d/2)
      , translate (-w/2 + r) ( h/2 - r) (rotate 270 cornerBorder)
      , translate (-w/2 + r) (-h/2 + r) (rotate 180 cornerBorder)
      , translate ( w/2 - r) (-h/2 + r) (rotate 90 cornerBorder)
      , translate ( w/2 - r) ( h/2 - r) cornerBorder
      ]

    inner = pictures
      [ rect (-w/2, -h/2 + r) (-w/2 + r,  h/2 - r)
      , rect ( w/2, -h/2 + r) ( w/2 - r,  h/2 - r)
      , rect (-w/2 + r, -h/2) ( w/2 - r, -h/2 + r)
      , rect (-w/2 + r,  h/2) ( w/2 - r,  h/2 - r)
      , rect (-w/2 + r, -h/2 + r) (w/2 - r, h/2 - r)
      , translate (-w/2 + r) ( h/2 - r) (rotate 270 corner)
      , translate (-w/2 + r) (-h/2 + r) (rotate 180 corner)
      , translate ( w/2 - r) (-h/2 + r) (rotate 90 corner)
      , translate ( w/2 - r) ( h/2 - r) corner
      ]

    corner = thickArc 0 90 (r/2) r
    cornerBorder = thickArc 0 90 r d



fieldHeight :: Float
fieldHeight = 40

fieldWidth :: Float
fieldWidth = 150


drawTetris ::GameState-> Picture
drawTetris u | ((typerepres u)==TetrisRound) =  pictures
  [ drawFigureCircle  u,
    drawBoardCircle (board u),
    drawScore (score u),
    drawCircleBackGr,
    drawRectangleMenu,
    drawmenuCircle,
    drawmenuSmooth,
    drawtextCircle,
    drawtextSmooth 
  
  
  ] 
    |otherwise = pictures
  [drawFigure  u,
   drawBoard (board u) ,
   drawScore (score u),
   drawRectangleMenu,
   drawmenuCircle,
   drawmenuSmooth,
   drawtextCircle,
   drawtextSmooth  ,
   drawRectanglBackGr
  
  
  
  ] 
drawRectanglBackGr::Picture
drawRectanglBackGr = pictures [ translate ((-(fromIntegral screenWidth  / 2))) (fromIntegral screenHeight / 2) (scale  1 1    (color cyan (line [(0,0) , (0,-600),(300,-600),(300,0),(0,0)])))]
drawCircleBackGr::Picture
drawCircleBackGr = translate (0) ((0) - 100) (scale 1.3 1.3 (color cyan (circle  ( 205))))

drawRectangleMenu ::Picture
drawRectangleMenu = translate (-0.3 * fieldWidth/2 + 113) (fieldHeight/2 + (fromIntegral screenHeight /2) - 53)
  (roundedRect (withAlpha 0.7 white) (greyN 0.7) ( fieldWidth/2 + 43) ((fromIntegral screenHeight / 10) + fieldHeight / 15) (0.1 * fieldWidth) (0.02 * fieldWidth))
drawmenuSmooth = (color orange (polygon [ (101, 290), (101, 250), (148, 250), (148, 290) ]))

drawmenuCircle :: Picture
drawmenuCircle =  (color yellow (polygon [ (34, 290), (34, 250), (100, 250), (100, 290) ]))

drawtextSmooth :: Picture
drawtextSmooth = translate (-(fromIntegral screenWidth  / 2) + 225) (fromIntegral screenHeight / 2 -18) (scale 13 13 (pictures [translate (2) (-1.5) (scale 0.008 0.008 (color red (text  "Smooth")))]))

drawtextCircle :: Picture
drawtextCircle = translate (-(fromIntegral screenWidth  / 2) + 123) (fromIntegral screenHeight / 2 + 4) (scale 30 30 (pictures [translate (2) (-1.5) (scale 0.008 0.008 (color red (text  "Type")))]))

drawScore :: Score -> Picture
drawScore score = translate (-w) h (scale 30 30 (pictures
  [ --color yellow (polygon [ (0, 0), (0, -2), (6, -2), (6, 0) ]),            -- белая рамка
   --color black (polygon [ (0, 0), (0, -1.9), (5.9, -1.9), (5.9, 0) ])    -- чёрные внутренности
   translate 2 (-1.5) (scale 0.01 0.01 (color green (text (show score))))  -- красный счёт
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

updateBoard::Figure -> Board ->Board
updateBoard (Figure sha dir (b ,c,z)) a = a ++ vectolist (figureToDraw (Figure sha dir (b ,c,z)))

--На основании прошедшего времени меняет скорость полета фигур
updateSpeed::Time -> Speed -> Speed
updateSpeed _ _ = 0


--Аргумент функции play, обновляет состояние тетриса
--С каждым кадром двигает фигуру вниз и пока здесь же проверяет, не достигла ли фигура нижней границы

updateTetris :: Float -> GameState -> GameState
updateTetris dt u = updateTetrisHelp dt (fromGS u)
                                                                  
updateTetrisHelp :: Float -> Gamestate -> GameState
updateTetrisHelp dt (a,(Figure sha dir (b,c,cl):rest),(sp, ti),e,v,p,k)|p==TetrisStepped = updateTetrisStepped dt (a,(Figure sha dir (b,c,cl):rest),(sp, ti),e,v,p,k)            
                                                                  |otherwise = updateTetrisSmooth dt (a,(Figure sha dir (b,c,cl):rest),(sp, ti),e,v,p,k)


updateTetrisStepped :: Float -> Gamestate -> GameState
updateTetrisStepped dt (a,(Figure sha dir (b,c,cl):rest),(sp, ti),e,v,p,k) | gameover = (toGS(genEmptyBoard,rest,(0.7, 0),0,v,p,0.7))
                                                              -- | collide =  (deleteRows (sortRows (updateBoard (Figure sha dir (b ,c,cl)) a)), rest, (sp, ti), e + 1)
                                                                            | otherwise = (toGS(newLevel (newTact (a,(Figure sha dir (b,c,cl):rest),(sp, ti),e,v,p,k) dt sp)))
                                                                              where
                                                                   -- collide =  collidesFigureDown (figureToDraw (Figure sha dir (b ,c + blockSize,cl)))   a
                                                                              gameover = isGameOver (a,(Figure sha dir (b,c,cl):rest),(sp, ti),e,v,p,k)
updateTetrisSmooth:: Float -> Gamestate -> GameState
updateTetrisSmooth dt (a,(Figure sha dir (b,c,cl):rest),(sp, ti),e,v,p,k) | gameover = (toGS(genEmptyBoard,rest,(0.01, 0),0,v,p,0.01))
                                                              -- | collide =  (deleteRows (sortRows (updateBoard (Figure sha dir (b ,c,cl)) a)), rest, (sp, ti), e + 1)
                                                                            | otherwise = (toGS(newLevel (newTact (a,(Figure sha dir (b,c,cl):rest),(sp, ti),e,v,p,k) dt sp)))
                                                                              where
                                                                   -- collide =  collidesFigureDown (figureToDraw (Figure sha dir (b ,c + blockSize,cl)))   a
                                                                              gameover = isGameOver (a,(Figure sha dir (b,c,cl):rest),(sp, ti),e,v,p,k)
-- ===========================================
-- timing
-- =======================================

newTact::Gamestate -> Float -> Float -> Gamestate
newTact (b, (Figure sha dir (f1,f2,f3):rest), (sp, ti), s,v,TetrisStepped,k) dt tact
  | paused = (b, (Figure sha dir (f1,f2,f3):rest), (sp, ti), s,v,TetrisStepped,k)
  | new && collides = (deleteRows (sortRows (updateBoard (Figure sha dir (f1,f2,f3)) b)), rest, (sp, ti), s + 1,v,TetrisStepped,k)
  | new = newTact (b, (Figure sha dir (f1,f2 + blockSize,f3):rest), (sp, 0), s,v,TetrisStepped,k) (dt + ti - tact) tact
  | collides = (b, (Figure sha dir (f1,f2,f3):rest), (sp, ti + dt + tact * 0.3), s,v,TetrisStepped,k)
  | otherwise = (b, (Figure sha dir (f1,f2,f3):rest), (sp, ti + dt), s,v,TetrisStepped,k)
                                        where
                                          new = ti + dt >= tact
                                          collides =  collidesFigureDown (figureToDraw (Figure sha dir (f1 ,f2 + blockSize,f3))) b
                                          paused = sp < 0
newTact (b, (Figure sha dir (f1,f2,f3):rest), (sp, ti), s,v,TetrisSmooth,k) dt tact
  | paused = (b, (Figure sha dir (f1,f2,f3):rest), (sp, ti), s,v,TetrisSmooth,k)
  | new && collides = (deleteRows (sortRows (updateBoard (Figure sha dir (f1,f2,f3)) b)), rest, (sp, ti), s + 1,v,TetrisSmooth,k)
  | new = newTact (b, (Figure sha dir (f1,f2 + 1,f3):rest), (sp, 0), s,v,TetrisSmooth,k) (dt + ti - tact) tact
  | collides = (b, (Figure sha dir (f1,f2,f3):rest), (sp, ti + dt + tact * 0.3), s,v,TetrisSmooth,k)
  | otherwise = (b, (Figure sha dir (f1,f2,f3):rest), (sp, ti + dt), s,v,TetrisSmooth,k)
                                        where
                                          new = ti + dt >= tact
                                          collides =  collidesFigureDownSmooth (figureToDraw (Figure sha dir (f1 ,f2 + blockSize,f3))) b
                                          paused = sp < 0
newLevel::Gamestate -> Gamestate
newLevel (b, (Figure sha dir (f1,f2,f3)):rest, (sp, ti), s,v,p,k)
  | l5 = (b, (Figure sha dir (f1,f2,f3)):rest, (signum(sp) * 0.1, ti), s,v,p,k)
  | l4 = (b, (Figure sha dir (f1,f2,f3)):rest, (signum(sp) * 0.15, ti), s,v,p,k)
  | l3 = (b, (Figure sha dir (f1,f2,f3)):rest, (signum(sp) * 0.2, ti), s,v,p,k)
  | l2 = (b, (Figure sha dir (f1,f2,f3)):rest, (signum(sp) * 0.25, ti), s,v,p,k)
  | l2 = (b, (Figure sha dir (f1,f2,f3)):rest, (signum(sp) * 0.3, ti), s,v,p,k)
  | l1 = (b, (Figure sha dir (f1,f2,f3)):rest, (signum(sp) * 0.4, ti), s,v,p,k)
  | otherwise = (b, (Figure sha dir (f1,f2,f3)):rest, (sp, ti), s,v,p,k)
        where 
          l5 = s >= 5000
          l4 = s >= 3000 && s <= 5000
          l3 = s >= 2000 && s <= 3000
          l2 = s >= 1500 && s <= 2000
          l1 = s >= 1000 && s <= 1500

--Аргумент функции play, которая говорит, что длает каждая клавиша


handleTetris :: Event -> GameState -> GameState

handleTetris (EventKey (Char 'l') Down _ _) u = moveRight u
handleTetris (EventKey (Char 'l') Up _ _) t = t

handleTetris (EventKey (Char 'j') Down _ _)  u  = moveLeft  u
handleTetris (EventKey (Char 'j') Up _ _)  t  = t

handleTetris(EventKey (SpecialKey KeySpace) Down _ _ ) u  = dropithelp (fromGS u)
handleTetris(EventKey (SpecialKey KeySpace) Up _ _ ) t = t

handleTetris (EventKey (Char 'k') Down _ _ ) u = turn (fromGS u)
handleTetris (EventKey (Char 'k') Up _ _ ) t = t

handleTetris (EventKey (Char 'p') Down _ _ ) u = tetrispause (fromGS u)
handleTetris (EventKey (Char 'p') Up _ _ ) t = t

handleTetris (EventKey (MouseButton LeftButton) Up _ mouse) u =  (mouseToCell mouse  (fromGS u ))
handleTetris  _ t = t  


dropithelp ::Gamestate -> GameState
dropithelp (a,(Figure sha dir (b,c,z):rest),d,e,v,p,k) = dropit (a,(Figure sha dir (b,c,z):rest),d,e,v,p,k) (screenHeight-c)
tetrispause :: Gamestate->GameState
tetrispause (a,(Figure sha dir (b,c,z):rest),(sp, ti),e,v,p,k) =(toGS (a,(Figure sha dir (b,c,z):rest),(- sp, ti),e,v,p,k))
tetrTypbuttonx1 :: Float
tetrTypbuttonx1 = 34
tetrTypbuttonx2 :: Float
tetrTypbuttonx2 = 100

tetrTypbuttony1 :: Float
tetrTypbuttony1 = 250
tetrTypbuttony2 :: Float
tetrTypbuttony2 = 290



tetrMovebuttonx1 :: Float
tetrMovebuttonx1 = 101
tetrMovebuttonx2 :: Float
tetrMovebuttonx2 = 148

tetrMovebuttony1 :: Float
tetrMovebuttony1 = 250
tetrMovebuttony2 :: Float
tetrMovebuttony2 = 290



mouseToCell :: Point->Gamestate -> GameState
mouseToCell (x, y) (a,(Figure sha dir (b,c,z):rest),(sp, ti),e,v,p,k)  | (x> tetrTypbuttonx1 && x<tetrTypbuttonx2 && y > tetrTypbuttony1 && y < tetrTypbuttony2 ) =  (toGS(a,(Figure sha dir (b,c,z):rest),(sp, ti),e,switchTetrisType v,p,k))
                                                                       
                                                                       | (x> tetrMovebuttonx1 && x<tetrMovebuttonx2 && y > tetrMovebuttony1 && y < tetrMovebuttony2 && p==TetrisStepped) =  (toGS(genEmptyBoard,rest,(0.01, 0),0,v,switchTetrisMove p,0.01))
                                                                       | (x> tetrMovebuttonx1 && x<tetrMovebuttonx2 && y > tetrMovebuttony1 && y < tetrMovebuttony2 && p==TetrisSmooth) =  (toGS(genEmptyBoard,rest,(0.7, 0),0,v,switchTetrisMove p,0.7))
                                                           |otherwise =  (toGS(a,(Figure sha dir (b,c,z):rest),(sp, ti),e,v,p,k))

switchTetrisType :: TetrisType -> TetrisType
switchTetrisType TetrisRect = TetrisRound
switchTetrisType TetrisRound = TetrisRect


switchTetrisMove :: TetrisMove -> TetrisMove
switchTetrisMove TetrisStepped = TetrisSmooth
switchTetrisMove TetrisSmooth = TetrisStepped

