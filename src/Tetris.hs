{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
module Tetris where

import System.Random
import Graphics.Gloss.Interface.Pure.Game

-- |Сколько кадров в секунду отрисовывается.
glob_fps::Int
glob_fps = 60

-- | Главная функция.
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






-- | Ширина экрана.
screenWidthreal :: Int
screenWidthreal = 800

-- | Высота экрана.
screenHeightreal :: Int
screenHeightreal = 800

-- | Координаты кнопки type x1,x2 ,y1,y2
tetrTypbutton::(Float,Float,Float,Float)
tetrTypbutton = (34,100,250,290)


-- | Дллина блока в круговом тетрисе.
angle :: Float
angle = 36

-- | Размер блока во Float.
blockSizeFloat :: Float
blockSizeFloat = 30

-- | Угол, который приходиться рисовать отдельно, т.к. функция
-- ThickArc рисует его с другой стороны.
specangel :: Float
specangel = 324

-- | Смещение.
offset2 :: Float
offset2 = 100

-- | Масштаб для риования.
myscale :: Float
myscale = 2.3

-- | Подходящий размер.
sizefit ::Float
sizefit = 5

-- | Подходящий размер в Int.
sizefitInt ::Int
sizefitInt = 5

-- | Другое смещение
offsedge ::Int
offsedge = 15

-- | Ширина рамки кругового тетриса 2
thicknessOne::Float
thicknessOne = 6

-- | Ширина рамки кругового тетриса 2
thicknessTwo::Float
thicknessTwo = 2

-- | Первый такт ступенчатого тетриса
inintTactStepped::Float
inintTactStepped = 0.7

-- | Первый такт плавного тетриса
inintTactSmooth::Float
inintTactSmooth = 0.01

-- | Координаты кнопки move x1,x2 ,y1,y2
tetrMoveButton ::(Float,Float,Float,Float)
tetrMoveButton = (101,148,250,290)

-- | Размер блока в фигуре.
blockSize :: Int
blockSize = 30

-- | Начальная скорость падения фигуры.
init_tact::Time
init_tact = 0.7

-- | Для плавного тетриса
conSmooth:: Float
conSmooth = 225

-- | Для фона кругового тетриса
conCircleB:: Float
conCircleB = 205

-- | Для кругового тетриса
conCircle:: Float
conCircle = 123

-- | Для меню тетриса ступенчатого
conRecMenu:: Float
conRecMenu =113

-- | Масштаб счета
scaleSc::Float
scaleSc = 0.01

-- | На сколько делить ширину экрана
transl::Float
transl = 2

-- | На сколько увеличить рамку позади
scaleBackGr::Float
scaleBackGr = 1.3

-- | Масштаб текста
scaleT::Float
scaleT = 0.008

-- | Масштаб текста в гладком тетрисе
scaleTSmooth::Float
scaleTSmooth = 13

-- | Масштаб рамки позади
transtBackGround::Float
transtBackGround = -100

-- | Масштаб текста
translateT::Float
translateT = -1.5

-- | Прозрачность менб
alpha::Float
alpha = 0.7

-- | Клетка заполнена?
data Block = Free  -- ^ Пустая.
            | Full  -- ^ Заполнена.
         deriving(Eq, Show)

-- | Строки нашей доски.
type Row = [Block]

-- | Все поле.
type Board = [Coord]

-- | Счет.
type Score = Int

-- | Координаты фигуры, поворот однозначно определяется 
-- их последовательностью.
-- x y цвет .
type Coord1 = (Int, Int,Int)

-- | Координаты блока x, y и его цвет clr.
data Coord = Coord 
  { x   :: Int  -- ^ Координата x.
  , y   :: Int  -- ^ Координата y.
  , clr :: Int  -- ^ Цвет блока.
  }

-- | Время.
type Time = Float


-- | Тип тетриса- Прямоугольный или Круговой.
data TetrisType = TetrisRect   -- ^ Прямоугольный.
                | TetrisRound  -- ^ Круговой.
    deriving(Eq, Show)

-- | Тип тетриса - Ступенчатый или плавный.
data TetrisMove = TetrisStepped -- ^ Ступенчатый.
                 | TetrisSmooth -- ^ Плавный.
   deriving(Eq, Show)
-- | Состояние игры в текущий момент(разделили доску и фигуру,
-- чтобы при полете фигуры не мигала вся доска, также, чтобы было более 
-- оптимизировано)
-- [Figure] - бесконечный список фигур, в текущем состоянии берем первый элемент списка.
-- доска фигуры скорость время счет круговой(1) или прямоугольный(0) плавный(1) или чтупенчатый(0) init_tack.
----------------------------------------------------------------------------------------------------------------------------------------------------------
type Gamestate = (Board,  [Figure], (Speed, Time), Score,TetrisType,TetrisMove,Time)

-- | Состояние игры.
data GameState = GameState
 {  board   :: Board            -- ^ Доска.
  , figure :: [Figure]            -- ^ Список фигур.
  , speedandtime   :: (Speed, Time) -- ^ Время и скорость.
  , score   :: Score            -- ^ Счет.
  ,typerepres::TetrisType          -- ^ Тип представления.
  ,typemoving :: TetrisMove       -- ^ Тип движения.
  ,tactgamestate :: Time           -- ^ Время.
  } 

-- | Перевод в старое состояние(для отлаживания).
fromGS :: GameState -> Gamestate
fromGS GameState{..} = (board, figure, speedandtime, score, typerepres,typemoving,tactgamestate)

-- | Перевод в новое состояние(для отлаживания).
toGS :: Gamestate -> GameState
toGS (board, figure, (speed, time), score, typerepres,typemoving,tactgamestate) 
        = GameState board figure (speed, time) score  typerepres typemoving tactgamestate

-- | Перевод в старые координаты.
fromCoord :: Coord -> Coord1
fromCoord Coord{..} = (x,y,clr)

-- | Перевод в новые координаты.
toCoord :: Coord1 -> Coord
toCoord (x,y,clr) = Coord x y clr




-- | Скорость.
type Speed = Float

--Для каждой фигуры свой тип, чтобы однозначно можно было 
--определить ее и тип операций над ней, например, фигуру I можно вращать
--произвольно только на расстоянии больше 4 клеток от края,
--а фигуру O на расстоянии больше 2 клеток от края.

-- | Тип фигура.
data FigureType = O -- ^ Квадрат.
                 | I -- ^ Палка.
                 | T -- ^ Т образная.
                 | J -- ^ J образная.
                 | L -- ^ L образная.
                 | S -- ^ S образная.
                 | Z-- ^ Z образная.
                      deriving(Eq, Show)

-- | Тип направление фигуры.
data Direction = DUp -- ^ Вверх.
               | DDown  -- ^ Вниз.
               | DLeft -- ^ Влево.
               | DRight  -- ^ Вправо.
                      deriving(Eq, Show)

-- | Тип фигуры для игры.
data Figure = Figure FigureType Direction Coord 
                      
-- | Тип фигуры из блоков.
type BlockedFigure = (Coord, Coord, Coord, Coord)

-- | Ширина экрана.
screenWidth :: Int
screenWidth = 300

-- | Высота экрана.
screenHeight :: Int
screenHeight = 600





-- =========================================
-- Generating
-- =========================================





-- | На вход принимается случайное число от 0 до 6, которое определяет фигуру.
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
-- список чисел от 0 до 6 которые соответствуют фигурам.
initFigures :: StdGen -> [Figure]
initFigures g = map genFigure
  (randomRs getrange g)

-- | Диапазон генерации случайных чисел.
getrange :: (Int, Int)
getrange = (0, 6)
  


-- | Пустая доска.
genEmptyBoard :: Board
genEmptyBoard = [ ]
  
-- | Заполняем строки.
genRows::Int->Int->[Row]
genRows _ 0 = []
genRows w h = (genRows w (h-1)) ++ [genRow w]

-- | Заполняем строку.
genRow::Int->Row
genRow 0 = []
genRow w = (genRow (w-1)) ++ [Free]

-- | Генрируем вселенную.
genUniverse::StdGen -> GameState
genUniverse g = GameState{ board   = genEmptyBoard  
                          , figure  = initFigures g
                          , speedandtime   = (init_tact, 0)
                          , score    = 0
                          ,typerepres =    TetrisRect
                          ,typemoving =  TetrisStepped
                          ,tactgamestate     = 0.7
                          }





-- =========================================
-- Turning
-- =========================================






-- | Поворачиваем фигуру.
turn::GameState -> GameState
turn u   |(typerepres u) == TetrisRound =  turnRound u
         |(typemoving u)== TetrisStepped = turnStepped u 
        |otherwise = turnSmooth u
-- | Поворачиваем фигуру в круговом
turnRound :: GameState -> GameState
turnRound u= u{figure = cons  (chRotation (getf(figure u)))   (rest  (figure u))}
-- | Поворачиваем фигуру в ступенчатом тетрисе.
turnStepped::GameState -> GameState
turnStepped u = u{figure = cons  (chRotation (getf(figure u)))   (rest  (figure u))}
       

-- | Изменяем состояние поворот.
chRotation:: Figure->Figure
chRotation  (Figure t DUp c) = (Figure t DRight c)
chRotation  (Figure t DRight c) = (Figure t DDown c)
chRotation (Figure t DDown c) = (Figure t DLeft c)
chRotation (Figure t DLeft c) = (Figure t DUp c)


-- | Поворачиваем фигуру в палвном тетрисе.
turnSmooth::GameState -> GameState
turnSmooth u = u{figure = cons  (chRotation (getf(figure u))) (rest  (figure u))}
      


-- =========================================
-- Preparing for Drawing
-- =========================================





-- | Готовим фигуры к отрисовке.
figureToDraw :: Figure -> BlockedFigure
figureToDraw (Figure O d c) = figureToDrawO (Figure O d c)
figureToDraw (Figure I d c) = figureToDrawI (Figure I d c)
figureToDraw (Figure T d c) = figureToDrawT (Figure T d c)
figureToDraw (Figure J d c) = figureToDrawJ (Figure J d c)
figureToDraw (Figure L d c) = figureToDrawL (Figure L d c)
figureToDraw (Figure S d c) = figureToDrawS (Figure S d c)
figureToDraw (Figure Z d c) = figureToDrawZ (Figure Z d c)


-- | Готовим квадрат к отрисовке. Возвращаем координаты 4 блоков.
figureToDrawO :: Figure -> BlockedFigure
figureToDrawO (Figure _ _ c) 
  = (c, c {x = x c + bs}, c {y = y c - bs}, c {x = x c + bs, y = y c - bs})
  where
    bs = blockSize

-- | Готовим палку к отрисовке. Возвращаем координаты 4 блоков.
figureToDrawI :: Figure -> BlockedFigure
figureToDrawI (Figure _ d c) 
  | (d == DUp) || (d == DDown) = (c {y = y c + bs}, c, c {y = y c - bs}, c {y = y c - 2*bs})
  | otherwise                  = (c {x = x c - bs}, c, c {x = x c + bs}, c {x = x c + 2*bs})
  where
    bs = blockSize

-- | Готовим левый зигзаг к отрисовке. Возвращаем координаты 4 блоков.
figureToDrawZ :: Figure -> BlockedFigure
figureToDrawZ (Figure _ d c) 
  | (d == DUp) || (d == DDown) = (c {x = x c - bs, y = y c - bs},c {x = x c - bs}, c,c {y = y c + bs})
  | otherwise                  = (c {x = x c - bs},c,c {y = y c - bs}, c {x = x c + bs, y = y c - bs})
  where
    bs = blockSize

-- | Готовим правый зигзаг к отрисовке. Возвращаем координаты 4 блоков.
figureToDrawS :: Figure -> BlockedFigure
figureToDrawS (Figure _ d c) 
  | (d == DUp) || (d == DDown) = (c {x = x c - bs, y = y c + bs}, c {x = x c - bs}, c, c {y = y c - bs})
  | otherwise                  = (c {x = x c - bs},c, c {y = y c + bs}, c {x = x c + bs, y = y c + bs})
  where
    bs = blockSize

-- | Готовим Г-образную фигуру к отрисовке. Возвращаем координаты 4 блоков.
figureToDrawJ :: Figure -> BlockedFigure
figureToDrawJ (Figure _ d c) 
  | d == DDown  = (c {x = x c - bs, y = y c - bs}, c {y = y c - bs}, c, c {y = y c + bs})
  | d == DUp    = (c {y = y c - bs},c,c {y = y c + bs}, c {x = x c + bs, y = y c + bs})
  | d == DRight = (c {x = x c - bs},c,c {x = x c + bs}, c {x = x c + bs, y = y c - bs})
  | otherwise   = (c {x = x c - bs, y = y c + bs}, c {x = x c - bs}, c,      c {x = x c + bs})
  where
    bs = blockSize

-- | Готовим L-образную фигуру к отрисовке. Возвращаем координаты 4 блоков.
figureToDrawL :: Figure -> BlockedFigure
figureToDrawL (Figure _ d c) 
  | d == DDown  = (c {y = y c + bs},c,c {y = y c - bs}, c {x = x c + bs, y = y c - bs})
  | d == DUp    = (c {y = y c - bs},c,c {y = y c + bs}, c {x = x c - bs, y = y c + bs})
  | d == DRight = (c {x = x c - bs},c,c {x = x c + bs}, c {x = x c + bs, y = y c + bs})
  | otherwise   = (c {x = x c - bs, y = y c - bs}, c {x = x c - bs}, c,      c {x = x c + bs})
  where
    bs = blockSize

-- | Готовим Т-образную фигуру к отрисовке. Возвращаем координаты 4 блоков.
figureToDrawT :: Figure -> BlockedFigure
figureToDrawT (Figure _ d c) 
  | d == DDown  = (c {x = x c - bs}, c, c {x = x c + bs}, c {y = y c - bs})
  | d == DUp    = (c {x = x c - bs}, c, c {x = x c + bs}, c {y = y c + bs})
  | d == DRight = (c {y = y c + bs}, c, c {y = y c - bs}, c {x = x c + bs})
  | otherwise   = (c {y = y c + bs}, c, c {y = y c - bs}, c {x = x c - bs})
  where
    bs = blockSize






-- =========================================
-- Moving
-- =========================================







-- | Переещает фигуру влево в ступенчатом.
moveLeft::GameState -> GameState
moveLeft u |((typemoving u)==TetrisStepped && (typerepres u)== TetrisRect) = moveLeftSteppedRect u 
           |((typemoving u)==TetrisStepped && (typerepres u)== TetrisRound) = moveLeftSteppedRound u
           | ((typemoving u)==TetrisSmooth && (typerepres u)== TetrisRect) = moveLeftSmoothRect u

           |otherwise = moveLeftSmoothRound u


-- | Переещает фигуру влево в ступенчатом в прямоугольном.
moveLeftSteppedRect ::GameState -> GameState
moveLeftSteppedRect u   | collidewall = u{   figure =cons (mul8or9 (getf(figure u))) (rest  (figure u))}
                    | collide = u
                    |otherwise = u{ figure = cons  (minbl (getf(figure u))) (rest  (figure u))}

                    where 
    collide = collidesFigureSides (figureToDraw (minbl (getf(figure u)))) (board u)
    collidewall = collidesFigureSidesWallLeft (figureToDraw (minbl (getf(figure u)))) (board u)



-- | Переещает фигуру влево в ступенчатом в круговом.
moveLeftSteppedRound ::GameState -> GameState
moveLeftSteppedRound u    = u{ figure = cons  (minbl (getf(figure u))) (rest  (figure u))}

                  




-- | В зависимости от типа фигуры решаем, на сколько перемещать
-- до начала, если пересекли границe.
mul8or9::Figure->Figure
mul8or9  (Figure s t u)   |( s==O )|| (s==Z && (t /= DUp && t/=DDown))
                                               ||(s==S && (t/= DUp && t/=DDown))
                                               ||(s==J && t/= DDown)
                                               ||(s==L && t /=DUp)||(s==T && t/=DLeft) 
                                                = (Figure s t u{x = 8*blockSize})
                          |(s==I && (t /=DDown && t/=DUp)) = (Figure s t u{x = 7*blockSize})
                          |otherwise = (Figure s t u{x = 9*blockSize})            
-- | Отнимаем блок из координаты.
minbl::Figure->Figure
minbl  (Figure s t u)   = (Figure s t u{x = (x u) - blockSize})  


-- | Возвращаем конец списка.
rest ::[Figure]->[Figure]
rest (_:fs) = fs
rest [] = []


-- | Присоединяем фигуру к списку фигур.
cons ::Figure->[Figure]->[Figure]
cons  a f = a:f

-- | Поворачиваем влево в плавном в прямоугольном.
moveLeftSmoothRect ::GameState -> GameState
moveLeftSmoothRect u   | collidewall = u{   figure =cons (mul8or9 (getf(figure u))) (rest  (figure u))}
                   | collide = u
                   |otherwise = u{ figure = cons  (minbl (getf(figure u))) (rest  (figure u))}

                    where 
    collide = collidesFigureSidesSmooth (figureToDraw (minbl (getf(figure u)))) (board u)
    collidewall = collidesFigureSidesWallLeft (figureToDraw (minbl (getf(figure u)))) (board u)      


-- | Поворачиваем влево в плавном в круговом.
moveLeftSmoothRound ::GameState -> GameState
moveLeftSmoothRound u   = u{ figure = cons  (minbl (getf(figure u))) (rest  (figure u))}

                   



-- | Поворачиваем вправо.
moveRight::GameState -> GameState
moveRight u |((typemoving u)==TetrisStepped && ((typerepres u) == TetrisRect)) = moveRightSteppedRect u 
            |((typemoving u)==TetrisStepped && ((typerepres u) == TetrisRound)) = moveRightSteppedRound u
            |((typemoving u)==TetrisSmooth && ((typerepres u) == TetrisRect)) = moveRightSmoothRect u
            |otherwise = moveRightSmoothRound u



-- | Поворачиваем вправо в ступенчатом в прямоугольном.
moveRightSteppedRect ::GameState -> GameState
moveRightSteppedRect u   | collidewall = u{   figure =cons (bl (getf(figure u))) (rest  (figure u))}
                     | collide = u
                     |otherwise = u{ figure = cons  (plbl (getf(figure u))) (rest  (figure u))}

                    where 
    collide = collidesFigureSides (figureToDraw (plbl (getf(figure u)))) (board u)
    collidewall = collidesFigureSidesWallRight (figureToDraw (plbl (getf(figure u)))) (board u)


-- | Поворачиваем вправо в ступенчатом в круговом.
moveRightSteppedRound ::GameState -> GameState
moveRightSteppedRound u    = u{ figure = cons  (plbl (getf(figure u))) (rest  (figure u))}

                    



-- | В зависимости от типа фигуры решаем, насколько до 
-- начала надо перемещать фигуру.
bl::Figure->Figure
bl  (Figure s t u)  | (s==O &&t == DDown)||(s== Z)||(s==S)||(s==J && t/=DUp)
                                         || (s==L && t/=DDown)||(s==T && t == DUp)
                                         ||(s==T && t /= DRight)||(s==I && t /= DUp && t/=DDown ) 
                                         = (Figure s t u{x = blockSize})
                    | otherwise =    (Figure s t u{x = 0})           
-- | К координате фигуры добовляем блок.
plbl::Figure->Figure
plbl  (Figure s t u)   = (Figure s t u{x = (x u) + blockSize})  

-- | Поворачиваем вправо в плавном в прямоугольном.
moveRightSmoothRect ::GameState -> GameState
moveRightSmoothRect u   | collidewall = u{   figure =cons (bl (getf(figure u))) (rest  (figure u))}
                    | collide = u
                    |otherwise = u{ figure = cons  (plbl (getf(figure u))) (rest  (figure u))}

                    where 
    collide = collidesFigureSidesSmooth (figureToDraw (plbl (getf(figure u)))) (board u)
    collidewall = collidesFigureSidesWallRight (figureToDraw (plbl (getf(figure u)))) (board u)   


-- | Поворачиваем вправо в плавном в круговом.
moveRightSmoothRound ::GameState -> GameState
moveRightSmoothRound u    = u{ figure = cons  (plbl (getf(figure u))) (rest  (figure u))}

                    


-- | Проверка, пересекает ли блок границы игрового окна.
collidesBlock::Coord -> Bool
collidesBlock Coord{..} | (x < 0) || (x  + blockSize > screenWidth) 
                              || (y < 0) 
                              || (y + blockSize > screenHeight) = True
                       |otherwise = False

-- | Проверка, пересекает ли блок боковые границы окна, либо доску в ступенчатом тетриче.
collidesBlockSides::Coord -> Board -> Bool
collidesBlockSides block [] = (x block < 0) || (x block  + blockSize > screenWidth)
collidesBlockSides block (c:[]) = (x block < 0)   || (x block + blockSize > screenWidth) 
                                                  || (x block == x c) && (y block == y c)

collidesBlockSides block (c:brds) | (x block < 0) || (x block + blockSize > screenWidth) 
                                                  || (x block == x c) && (y block == y c)  = True
                                  | otherwise = collidesBlockSides block brds

-- | Проверка выхода за границы поля слева.
collidesBlockSidesWhallLeft::Coord -> Board -> Bool
collidesBlockSidesWhallLeft u _= (x u) <0

-- | Проверка выхода за границы поля справа.
collidesBlockSidesWhallRight::Coord -> Board -> Bool
collidesBlockSidesWhallRight u _= ((x u) + blockSize) > screenWidth

-- | Проверка, пересекает ли блок боковые границы окна, либо доску в ступенчатом тетриче.
collidesBlockSidesSmooth::Coord -> Board -> Bool
collidesBlockSidesSmooth u [] = ((x u) < 0) || ((x u)  + blockSize > screenWidth)
collidesBlockSidesSmooth u (u1:[]) = ((x u) < 0) || ((x u)  + blockSize > screenWidth) 
                                  || ((x u) ==(x u1)) && ((y u)==(y u1))
                                  ||(((x u)==(x u1)) &&((y u)>((y u1) - blockSize) && (y u)<((y u1) + blockSize)))

collidesBlockSidesSmooth u (u1:brds) | ((x u) < 0) || ((x u)  + blockSize > screenWidth) 
                                       || ((x u) ==(x u1)) && ((y u)==(y u1))
                                       ||(((x u)==(x u1)) &&((y u)>((y u1) - blockSize) && (y u)<((y u1) + blockSize))) = True
                                     | otherwise = collidesBlockSides u brds

-- | Проверка, пересекает ли блок пол или доску в ступенчатом.
collidesBlockDown::Coord -> Board-> Bool
collidesBlockDown block []  =   (y block + blockSize > screenHeight)
collidesBlockDown block (c:[])  =   ((y block + blockSize > screenHeight) || ((x block == x c) && (y block == y c)) 
                                                  || (((x block) + 300) == x c && (y block == y c)) 
                                                  || (((x block) - 300)== x c && (y block == y c)))

collidesBlockDown block (c:brds)  | ((y block + blockSize > screenHeight) || (x block == x c) && (y block == y c)
                                                  || (((x block) + 300) == x c && (y block == y c))
                                                  || (((x block) - 300)== x c && (y block == y c)))  = True
                                  |  otherwise = collidesBlockDown block brds
                                
-- | Проверка, пересекает ли блок пол или доску в плавном.
collidesBlockDownSmooth::Coord -> Board-> Bool
collidesBlockDownSmooth u []  =   ((y u)  > screenHeight)        || ((y u) < 0)

collidesBlockDownSmooth u (u1:[])  = ((((y u)  > screenHeight)   || ((x u) ==(x u1)) && (((y u) )==(y u1)))
                                                      || ((y u) < 0) 
                                                      || (((x u) + 300) == x u1 && (y u == y u1)) 
                                                      || (((x u) - 300)== x u1 && (y u == y u1)))

collidesBlockDownSmooth u (u1:brds)  |  ((((y u)  > screenHeight)|| ((x u) ==(x u1)) && (((y u) )==(y u1)))
                                                      || ((y u) < 0)|| (((x u) + 300) == x u1 && (y u == y u1)) 
                                                      || (((x u) - 300)== x u1 && (y u == y u1))) =True
                                     |  otherwise = collidesBlockDownSmooth u brds
                           
-- | Проверка, пересекается ли блок потолок или доску.
collidesBlockUp :: Coord -> Board-> Bool
collidesBlockUp c []                    =  y c < 0
collidesBlockUp c (c1 : [])   = (y c < 0) && (y c == y c1)
collidesBlockUp c (c1 : brds)  
  | y c < 0 && (y c == y c1)  = True
  | otherwise                 = collidesBlockUp c brds

-- | Пересекает ли фигура доску или границы в ступенчатом?
collidesFigure::BlockedFigure -> Board -> Bool
collidesFigure (a,b,c,d) board = (collidesFigureSides (a,b,c,d) board) || (collidesFigureDown (a,b,c,d) board)

-- | Пересекает ли фигура доску или границы в плавном?
collidesFigureSmooth :: BlockedFigure -> Board -> Bool
collidesFigureSmooth (a, b, c, d) brd = or
  [ collidesFigureSidesSmooth (a, b, c, d) brd
  , collidesFigureDownSmooth  (a, b, c, d) brd ]

-- | Проверка, пересекает ли фигура боковые границы окна, либо доску.
collidesFigureSides::BlockedFigure -> Board -> Bool
collidesFigureSides (a,b,c,d) board | (collidesBlockSides a board) 
                                        || (collidesBlockSides b board) 
                                        || (collidesBlockSides c board) 
                                        || (collidesBlockSides d board) = True
                                    |otherwise = False

-- | Пересекает ли границу слева в ступенчатом(чтобы можно было ходить покругу).
collidesFigureSidesWallLeft    ::BlockedFigure -> Board -> Bool
collidesFigureSidesWallLeft  (a,b,c,d) board | (collidesBlockSidesWhallLeft a board)
                                              || (collidesBlockSidesWhallLeft b board) 
                                              || (collidesBlockSidesWhallLeft c board) 
                                              || (collidesBlockSidesWhallLeft d board) = True
                                             |otherwise = False

-- | Пересекает ли границу справа в ступенчатом(чтобы можно было ходить покругу).
collidesFigureSidesWallRight::BlockedFigure -> Board -> Bool
collidesFigureSidesWallRight (a,b,c,d) board | (collidesBlockSidesWhallRight a board) 
                                               || (collidesBlockSidesWhallRight b board) 
                                               || (collidesBlockSidesWhallRight c board) 
                                               || (collidesBlockSidesWhallRight d board) = True
                                             |otherwise = False

-- | Пересекает ли фигуры в плавном.
collidesFigureSidesSmooth::BlockedFigure -> Board -> Bool
collidesFigureSidesSmooth (a,b,c,d) board | (collidesBlockSidesSmooth a board)
                                            || (collidesBlockSidesSmooth b board) 
                                            || (collidesBlockSidesSmooth c board)
                                            || (collidesBlockSidesSmooth d board) = True
                                          |otherwise = False        

-- | Проверка, что фигура касается снизу доски или поля в ступенчотом.
collidesFigureDown::BlockedFigure -> Board -> Bool
collidesFigureDown (a,b,c,d) board | (collidesBlockDown a board) 
                                     || (collidesBlockDown b board) 
                                     || (collidesBlockDown c board) 
                                     || (collidesBlockDown d board) = True
                                   |otherwise = False

-- | Проверка, что фигура касается снизу доски или поля в плавном.
collidesFigureDownSmooth::BlockedFigure -> Board -> Bool
collidesFigureDownSmooth (a,b,c,d) board | (collidesBlockDownSmooth a board) 
                                           || (collidesBlockDownSmooth b board) 
                                           || (collidesBlockDownSmooth c board)
                                           || (collidesBlockDownSmooth d board) = True
                                        |otherwise = False



-- | Игра окончена?
isGameOver::GameState -> Bool
isGameOver u |((typemoving u) == TetrisStepped) = collidesFigureDown (figureToDraw 
                                                  (getf(rest  (figure u))) ) (board u)  
             |otherwise = collidesFigureDownSmooth (figureToDraw (getf(figure u))) (board u)



-- | Сортируем строки.
sortRows :: Board -> Board
sortRows []     = []
sortRows (c : brds) = sortRows (filter (\c1 -> y c1 > y c) brds) ++ [c] ++ sortRows 
                (filter (\c1 -> y c1 <= y c) brds)

-- | Удалям заполненные строки.
deleteRows :: Board -> Board
deleteRows [] = []
deleteRows (c : brds)
  | isFullRow (row brd (y c)) = deleteRows . boardMoveDown $ (upperRows brd (y c)) ++ (lowerRows brd (y c))
  | otherwise = (row brd (y c)) ++ (deleteRows (upperRows brd (y c)))
  where 
    brd = (c : brds)

-- | Строки выше заданной строки.
upperRows :: Board -> Int -> Board
upperRows brd scope = (filter (\c1 -> y c1 < scope) brd)

-- | Строки ниже заданной строки.
lowerRows :: Board -> Int -> Board
lowerRows brd scope = (filter (\c1 -> y c1 > scope) brd)

-- | Сдвигаем строки доски вниз.
boardMoveDown :: Board -> Board
boardMoveDown [] = []
boardMoveDown (c : brd) = c {y = (y c) + blockSize} : boardMoveDown brd

-- | n-ая строка доски.
row :: Board -> Int -> [Coord]
row b n = (filter (\b1 -> n == y b1) b)

-- | Заполнена ли доска?
isFullRow :: [Coord] -> Bool
isFullRow r = (length r) >= 10


-- | При нажатии клавиши "вниз" роняет фигуру. 
dropit::GameState -> Int -> GameState
dropit u ptr |(typemoving u)  == TetrisStepped = dropitStepped u ptr
             |otherwise = dropitSmooth u ptr
-- | Роняет в ступенчатом.             
dropitStepped ::GameState -> Int -> GameState
dropitStepped u pts | collide = u{score = ((score u) + (div pts blockSize))}   
                    |otherwise = dropitStepped u{figure = cons  (plbly (getf(figure u)))   
                     (rest  (figure u))} pts
                    where                                           
                collide = collidesFigureDown (figureToDraw (plbly (getf (figure u)))) (board u)
-- | Прибавить к ординате размер блока.
plbly::Figure->Figure
plbly  (Figure s t u)   = (Figure s t u{y = (y u) + blockSize}) 

-- | Роняет в ступенчатом.  
dropitSmooth ::GameState -> Int -> GameState
dropitSmooth u pts | collide = u{score = (score u) + (div pts blockSize)}   
                    |otherwise = dropitSmooth u{figure = cons  (ploy (getf (figure u)))   (rest  (figure u))} pts
                    where                                           
                    collide = collidesFigureDownSmooth (figureToDraw (plbly (getf (figure u)))) (board u)
-- | Прибавить к ординате размер 1.
ploy::Figure->Figure
ploy  (Figure s t u)   = (Figure s t u{y  = (y u) + 1}) 







-- =========================================
-- Drawing
-- =========================================







-- | Рисуем доску в прямоугольном.
drawBoard::Board  -> Picture
drawBoard s = pictures (map drawBlock s)

-- | Рисуем доску в круговом.
drawBoardCircle :: Board -> Picture
drawBoardCircle s = pictures (map drawBlockCircleHelp s)

-- | вспомогательная для рисования в круговом.
drawBlockCircleHelp :: Coord -> Picture
drawBlockCircleHelp u = drawBlockCircle (fromCoord u)




-- | Рисуем блок в круговом тетрисе.
drawBlockCircle :: Coord1-> Picture
drawBlockCircle  (b,c,d) 
                    |(b==270|| b==570|| b==870||b== -30|| b== -330|| b== -630) = 
  pictures [ translate (-w) (h - offset2) (scale  myscale myscale  ( pictures[ 
    (rotate (-specangel) (color (numtocolor d)   (thickArc (0) (angle) 
                      (fromIntegral (c + sizefitInt) / sizefit  - thicknessTwo) ( thicknessOne) ))),
   (rotate (-specangel) (color magenta  (thickArc (fromIntegral (0  )) 
                      ( (angle  )) (fromIntegral (c + sizefitInt +offsedge) / sizefit -thicknessTwo ) (fromIntegral 1) ))),
   (rotate (-specangel) (color magenta  (thickArc (fromIntegral (0  )) ( (angle  )) 
                      (fromIntegral (c + sizefitInt-offsedge) / sizefit -thicknessTwo ) (fromIntegral 1) )) ),
   (rotate (-specangel) (color magenta   (thickArc (0) (1) 
                       (fromIntegral (c + sizefitInt) / sizefit  - thicknessTwo) ( thicknessOne) ))),
    (rotate (-specangel) (color magenta   (thickArc (angle - 1) (angle) 
                       (fromIntegral (c + sizefitInt) / sizefit  - thicknessTwo) ( thicknessOne) ))) ]))
    ]                 
    
        
     
                   |otherwise = pictures [ translate (-w) (h - offset2) (scale  myscale myscale  (pictures
 [ 
  color (numtocolor d)   (thickArc (((fromIntegral b)/blockSizeFloat)*angle) (((fromIntegral b)/blockSizeFloat)*angle + angle) 
                          (fromIntegral (c + sizefitInt) / sizefit  - thicknessTwo) ( thicknessOne) ) ,            
   color magenta   (thickArc (((fromIntegral b)/blockSizeFloat)*angle) (((fromIntegral b)/blockSizeFloat)*angle + angle) 
                          (fromIntegral (c + sizefitInt+offsedge) / sizefit  - thicknessTwo) (fromIntegral 1) ) ,
   color magenta   (thickArc (((fromIntegral b)/blockSizeFloat)*angle) (((fromIntegral b)/blockSizeFloat)*angle + angle) 
                          (fromIntegral (c + sizefitInt-offsedge) / sizefit  - thicknessTwo) (fromIntegral 1) ) ,

   color magenta   (thickArc (((fromIntegral b)/blockSizeFloat)*angle ) (((fromIntegral b)/blockSizeFloat)*angle + 1) 
                         (fromIntegral (c + sizefitInt) / sizefit  - thicknessTwo) ( thicknessOne) ) ,
   color magenta   (thickArc (((fromIntegral b)/blockSizeFloat)*angle +35) (((fromIntegral b)/blockSizeFloat)*angle + angle)
                       (fromIntegral (c + sizefitInt) / sizefit  - thicknessTwo) ( thicknessOne) )
   ]))
    ]
  where
  w = fromIntegral 0
  h = fromIntegral 0

-- | Фуксиновая рамка для блоков.
magframe :: Int -> Int -> [Picture]
magframe b c = 
  [ color magenta  (polygon [ (fromIntegral b,        fromIntegral (-c))
                            , (fromIntegral b,        fromIntegral (-c - 2))
                            , (fromIntegral (b + blockSize), fromIntegral (-c - 2))
                            , (fromIntegral (b + blockSize), fromIntegral (-c)) 
                            ])
  , color magenta  (polygon [ (fromIntegral b,        fromIntegral (-c))
                            , (fromIntegral b,        fromIntegral (-c - blockSize))
                            , (fromIntegral (b + 2),  fromIntegral (-c - blockSize))
                            , (fromIntegral (b + 2),  fromIntegral (-c))
                            ])
  , color magenta  (polygon [ (fromIntegral b,        fromIntegral (-c - blockSize - 2))
                            , (fromIntegral b,        fromIntegral (-c - blockSize))
                            , (fromIntegral (b + blockSize), fromIntegral (-c - blockSize))
                            , (fromIntegral (b + blockSize), fromIntegral (-c - blockSize - 2))
                            ])
  , color magenta  (polygon [ (fromIntegral (b + blockSize - 2), fromIntegral (-c))
                            , (fromIntegral (b + blockSize - 2), fromIntegral (-c - blockSize))
                            , (fromIntegral (b + blockSize), fromIntegral (-c - blockSize))
                            , (fromIntegral (b + blockSize), fromIntegral (-c)) 
                            ])
  ]

-- | Сопоставляем числам цвета.
numtocolor :: Int -> Color
numtocolor 0 = azure
numtocolor 1 = blue
numtocolor 2 = yellow
numtocolor 3 = red
numtocolor 4 = green
numtocolor 5 = orange
numtocolor _ = white

-- | Рисуем блок в прямоугольном тетрисе.
drawBlock :: Coord-> Picture
drawBlock  crd 
  =  pictures [ translate (-w) h (scale  1 1 (pictures (
                  [ color (numtocolor clr1) (polygon [ (fromIntegral b,         fromIntegral (-c))
                                                    , (fromIntegral b,         fromIntegral (-c - blockSize))
                                                    , (fromIntegral  (b + blockSize), fromIntegral (-c - blockSize))
                                                    , (fromIntegral  (b + blockSize), fromIntegral (- c))
                                                    ])
                  ] ++ (magframe b c)))) ]
  where
    w = fromIntegral screenWidth  / transl
    h = fromIntegral screenHeight / transl
    b = x crd
    c = y crd
    clr1 = clr crd

-- | Рисуем фигуру в прямоугольном.
drawFigure::GameState  ->  Picture
drawFigure u = drawBlockedFigure  (figureToDraw (getf (figure u) ))


-- | Рисуем фигуру в круговом.
drawFigureCircle::GameState  ->  Picture
drawFigureCircle u = drawBlockedFigureCircle(figureToDraw (getf (figure u) ))

-- | Получить первый элемент списка.
getf ::[Figure]->Figure
getf (f:_) = f
getf [] = Figure T DUp Coord {x = div screenWidth 2, y = blockSize * 2, clr = 0}
  
-- | Нарисовать фигуру ил блоков в крговом.  
drawBlockedFigureCircle :: BlockedFigure -> Picture
drawBlockedFigureCircle ((a, b, c, d)) =         pictures  [drawBlockCircle  (fromCoord a),
                                                            drawBlockCircle   (fromCoord b ),
                                                            drawBlockCircle   (fromCoord  c),
                                                            drawBlockCircle    (fromCoord d )]

-- | Нарисовать фигуру ил блоков в прямоугольном.  
drawBlockedFigure::BlockedFigure -> Picture
drawBlockedFigure ((a, b, c, d)) =         pictures  [drawBlock   a,
                                                      drawBlock   b,
                                                      drawBlock   c,
                                                      drawBlock   d ]



-- | Рисуем квадрат
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


-- | Высота поля.
fieldHeight :: Float
fieldHeight = 40

-- | Ширина поля.
fieldWidth :: Float
fieldWidth = 150

-- | Рисуем тетрис.
drawTetris ::GameState-> Picture
drawTetris u | ((typerepres u)==TetrisRound) =  pictures
  [ drawFigureCircle  u,
    drawBoardCircle (board u),
    drawScore (score u),
    drawCircleBackGr,
    drawRectangleMenu,
    drawmenuCircle tetrTypbutton,
    drawmenuSmooth tetrMoveButton,
    drawtextCircle,
    drawtextSmooth 
  
  
  ] 
    |otherwise = pictures
  [drawFigure  u,
   drawBoard (board u) ,
   drawScore (score u),
   drawRectangleMenu,
   drawmenuCircle tetrTypbutton,
   drawmenuSmooth tetrMoveButton,
   drawtextCircle ,
   drawtextSmooth  ,
   drawRectanglBackGr
  ] 

-- | Нарисовать фон квадрата.  
drawRectanglBackGr::Picture
drawRectanglBackGr = pictures [ translate ((-(fromIntegral screenWidth  / transl)))
                      (fromIntegral screenHeight / transl) (scale  scaleBackGrTwo scaleBackGrTwo   
                    (color cyan (line [(0,0) , (0,-screenHeightFloat),
                      (screenWidthFloat,-screenHeightFloat),(screenWidthFloat,0),(0,0)])))]


scaleBackGrTwo::Float
scaleBackGrTwo = 1

screenHeightFloat::Float
screenHeightFloat = 600

screenWidthFloat ::Float
screenWidthFloat = 300

scalCirclB ::Float
scalCirclB = 1.3

drawTextConSmooth::Float
drawTextConSmooth = -18

drawTextConCircle::Float
drawTextConCircle = 4
translMenu::Float
translMenu = 10

translMenu2::Float
translMenu2= 15
transCircleBack::Float
transCircleBack = 0
consCircBack::Float
consCircBack = 43
consCircBackTwo::Float
consCircBackTwo = 53
tranRectCon::Float
tranRectCon = -0.3
tranRectConTwo::Float
tranRectConTwo = 0.02

tranRectConThree::Float
tranRectConThree = 0.1

-- | Нарисовать фон круга.
drawCircleBackGr::Picture
drawCircleBackGr = translate (transCircleBack) ((transCircleBack)  + transtBackGround) 
              (scale scalCirclB scalCirclB (color cyan (circle  ( conCircleB))))

-- | Нарисовать меню.
drawRectangleMenu ::Picture
drawRectangleMenu = translate (tranRectCon * fieldWidth/transl + conRecMenu) 
               (fieldHeight/transl + (fromIntegral screenHeight /transl) - consCircBackTwo)
               (roundedRect (withAlpha alpha white) (greyN alpha) ( fieldWidth/transl + consCircBack) 
               ((fromIntegral screenHeight / translMenu) + fieldHeight / translMenu2) 
                                   (tranRectConThree * fieldWidth) (tranRectConTwo * fieldWidth))

-- | Нарисовать кнопку для выбора плавного режима.
drawmenuSmooth:: (Float,Float,Float,Float)->Picture
drawmenuSmooth (a,b,c,d) = (color orange (polygon [ (a, d), (a, c), (b, c), (b, d) ]))

-- | Нарисовать кнопку для выбора кругового режима.
drawmenuCircle :: (Float,Float,Float,Float)->Picture
drawmenuCircle (a,b,c,d) =  (color yellow (polygon [ (a, d), (a, c), (b, c), (b, d) ]))

-- | Нарисовать текст на кнопке с плавным режимом.
drawtextSmooth :: Picture
drawtextSmooth = translate (-(fromIntegral screenWidth  / transl) + conSmooth) 
                     (fromIntegral screenHeight / transl  + drawTextConSmooth) 
                      (scale scaleTSmooth scaleTSmooth
                    (pictures [translate (transl) (translateT) (scale scaleT scaleT (color red (text  "Smooth")))]))

-- | Нарисовать текст на кнопке с круговым режимом.
drawtextCircle :: Picture
drawtextCircle = translate (-(fromIntegral screenWidth  / transl) + conCircle) 
                  (fromIntegral screenHeight / transl + drawTextConCircle) 
                      (scale blockSizeFloat blockSizeFloat
                   (pictures [translate (transl) (translateT) (scale scaleT scaleT (color red (text  "Type")))]))


-- | Нарисовать счет.
drawScore :: Score -> Picture
drawScore score = translate (-w) h (scale blockSizeFloat blockSizeFloat (pictures
  [ translate transl (translateT) (scale scaleSc scaleSc (color green (text (show score)))) 
  ]))
  where
    w = fromIntegral screenWidth  / transl
    h = fromIntegral screenHeight / transl





-- =========================================
-- Updating
-- =========================================







-- | Кортеж в список.
vectolist :: (Coord, Coord, Coord, Coord) -> [Coord]
vectolist (a,b,c,d) = [a,b,c,d]

-- | Обновить доску.
updateBoard::Figure -> Board ->Board
updateBoard (Figure sha dir u) a = a ++ vectolist (figureToDraw (Figure sha dir u))




-- | Обновить тетрис.
--С каждым кадром двигает фигуру вниз и пока здесь же проверяет, не достигла ли фигура нижней границы.
updateTetris :: Float -> GameState -> GameState
updateTetris dt u |(typemoving u) == TetrisStepped = updateTetrisStepped dt u 
             |otherwise = updateTetrisSmooth dt u


-- | Обновить ступенчатый.
updateTetrisStepped :: Float -> GameState -> GameState 
updateTetrisStepped dt u |gameover = genNewUniverse u (inintTactStepped)  
                    |otherwise = newLevel (newTact u dt (extrSpeed (speedandtime u)) ) 
                               where
                              gameover = isGameOver u   

-- | Обновить плавный.
updateTetrisSmooth :: Float -> GameState -> GameState 
updateTetrisSmooth dt u |gameover = genNewUniverse u (inintTactSmooth)  
                    |otherwise = newLevel (newTact u dt (extrSpeed (speedandtime u)) )                                
                       where
                       gameover = isGameOver u   


genNewUniverse :: GameState ->Float -> GameState
genNewUniverse u fl = u{ board   = genEmptyBoard  
                          , figure  = (rest  (figure u))
                          , speedandtime   = (fl, 0)
                          , score    = 0                                    
                          ,tactgamestate     = fl
                          }  




-- ===========================================
-- timing
-- ===========================================







-- | Новый такст.  
newTact::GameState -> Float -> Float -> GameState
newTact u  dt tact |(typemoving u) == TetrisStepped = newTactStepped u dt tact
          |otherwise = newTactSmooth u dt tact

-- | Поставить время 0.
chZ :: (Speed,Time)    ->(Speed,Time)  
chZ (sp,_) = (sp,0) 

-- | Извлесь время.
extrTime:: (Speed,Time) -> Time
extrTime (_,ti) = ti

-- | Извлесь скорость.
extrSpeed:: (Speed,Time) -> Speed
extrSpeed (sp,_) = sp


-- | Изменить время и скорость.
chSpeedAndTime::(Speed,Time) ->Float->Float ->(Speed,Time)
chSpeedAndTime (sp,ti) tact dt = (sp, ti + dt + tact * 0.3)

-- | Изменить время и скорость.
pldtSpeedAndTime::(Speed,Time) ->Float->(Speed,Time)
pldtSpeedAndTime (sp, ti ) dt= (sp, ti + dt)

-- | Новый такст в ступенчатом.
newTactStepped :: GameState -> Float -> Float -> GameState
newTactStepped u dt tact  | paused =u  
  | new && collides = u{ board = (deleteRows (sortRows (updateBoard (getf(figure u)) (board u)))), 
                        figure = (rest  (figure u)), 
                        speedandtime = (speedandtime u),
                        score =  (score u) + 1
                        }
  | new = newTact u{figure = cons  (plbly (getf(figure u)))   (rest  (figure u)) ,
                      speedandtime = chZ (speedandtime u)} (dt + (extrTime (speedandtime u)) - tact) tact
  | collides = u{speedandtime = (chSpeedAndTime (speedandtime u) tact dt) }
  | otherwise = u{speedandtime = pldtSpeedAndTime (speedandtime u) dt}
                    where
                     new = (extrTime (speedandtime u)) + dt >= tact
                     collides =  collidesFigureDown (figureToDraw (plbly (getf(figure u)))) (board u)
                     paused = (extrSpeed(speedandtime u)) < 0

-- | Новый такст в плавном.
newTactSmooth :: GameState -> Float -> Float -> GameState
newTactSmooth u dt tact  | paused =u  
  | new && collides = u{ board = (deleteRows (sortRows (updateBoard (getf(figure u)) (board u)))), 
                        figure = (rest  (figure u)), 
                        speedandtime = (speedandtime u),
                        score =  (score u) + 1
                        }
  | new = newTact u{figure = cons  (ploy (getf(figure u)))   (rest  (figure u)) ,
                      speedandtime = chZ (speedandtime u)} (dt + (extrTime (speedandtime u)) - tact) tact
  | collides = u{speedandtime = (chSpeedAndTime (speedandtime u) tact dt) }
  | otherwise = u{speedandtime = pldtSpeedAndTime (speedandtime u) dt}
                where
                new = (extrTime (speedandtime u)) + dt >= tact
                collides =  collidesFigureDownSmooth (figureToDraw (plbly (getf(figure u)))) (board u)
                paused = (extrSpeed(speedandtime u)) < 0




-- | Новый уровень.
newLevel::GameState->GameState
newLevel u | l5 = u{speedandtime = ((signum(extrSpeed (speedandtime u )))*0.1 ,(extrTime (speedandtime u)))  }
  | l4 = u{speedandtime = ((signum(extrSpeed (speedandtime u )))*0.15 ,(extrTime (speedandtime u)))  }
  | l3 = u{speedandtime = ((signum(extrSpeed (speedandtime u )))*0.2 ,(extrTime (speedandtime u)))  }
  | l2 = u{speedandtime = ((signum(extrSpeed (speedandtime u )))*0.25 ,(extrTime (speedandtime u)))  }
  | l2 = u{speedandtime = ((signum(extrSpeed (speedandtime u )))*0.3 ,(extrTime (speedandtime u)))  }
  | l1 = u{speedandtime = ((signum(extrSpeed (speedandtime u )))*0.4 ,(extrTime (speedandtime u)))  }
  | otherwise = u
        where 
          l5 = (score u) >= 5000
          l4 = (score u) >= 3000 && (score u) <= 5000
          l3 = (score u) >= 2000 && (score u) <= 3000
          l2 = (score u) >= 1500 && (score u) <= 2000
          l1 = (score u) >= 1000 && (score u) <= 1500








-- =========================================
-- Handling
-- =========================================







-- | Управление  тетрисом.
handleTetris :: Event -> GameState -> GameState

handleTetris (EventKey (Char 'l') Down _ _) u = moveRight u
handleTetris (EventKey (Char 'l') Up _ _) t = t

handleTetris (EventKey (Char 'j') Down _ _)  u  = moveLeft  u
handleTetris (EventKey (Char 'j') Up _ _)  t  = t

handleTetris(EventKey (SpecialKey KeySpace) Down _ _ ) u  = dropit u (screenHeight -  (getc (getf (figure u))))
handleTetris(EventKey (SpecialKey KeySpace) Up _ _ ) t = t

handleTetris (EventKey (Char 'k') Down _ _ ) u = turn  u
handleTetris (EventKey (Char 'k') Up _ _ ) t = t

handleTetris (EventKey (Char 'p') Down _ _ ) u = tetrispause  u
handleTetris (EventKey (Char 'p') Up _ _ ) t = t

handleTetris (EventKey (MouseButton LeftButton) Up _ mouse) u =  (mouseToCell mouse   u )
handleTetris  _ t = t  


-- | сделать паузу.
tetrispause :: GameState->GameState
tetrispause u = u { speedandtime = pause (speedandtime u )}

-- | сделать паузу - вспомогательнрая.
pause :: (Speed,Time) -> (Speed,Time)
pause (sp, ti) = (-sp,ti)


 
-- | Вытащить y. 
getc :: Figure-> Int
getc  (Figure _ _ u) = (y u)





-- | Обрабатываем нажатие мышки на кнопки.
mouseToCell :: Point->GameState -> GameState
mouseToCell (x, y) u  
         | onTypeButton (x,y) tetrTypbutton =
                                        u{typerepres = switchTetrisType (typerepres u)}                                                               
         | onMoveButton (x,y) tetrMoveButton = chMoving u 
         |otherwise =  u
onTypeButton::Point -> (Float,Float,Float,Float) -> Bool
onTypeButton (x,y)  (x1,x2,y1,y2)=(x> x1 && x<x2 && y > y1 && y < y2 ) 

onMoveButton ::Point -> (Float,Float,Float,Float) -> Bool
onMoveButton (x,y)  (x1,x2,y1,y2)=(x> x1 && x<x2 && y > y1 && y < y2 )                               
-- | Изменить тип тетриса(прямоугольный или круговой).
switchTetrisType :: TetrisType -> TetrisType
switchTetrisType TetrisRect = TetrisRound
switchTetrisType TetrisRound = TetrisRect



-- | Сгенерировать новый тетрис в зависимости от типа тетриса.
chMoving:: GameState->GameState
chMoving u |(typemoving u)==TetrisStepped = genTetris u inintTactSmooth 
           |otherwise =  genNewUniverse u inintTactStepped 

genTetris::GameState-> Float->GameState
genTetris u df= u{ board   = genEmptyBoard  
                          , figure  = (rest  (figure u))
                          , speedandtime   = (df, 0)
                          , score    = 0
                          
                          ,typemoving =  switchTetrisMove (typemoving u)
                          ,tactgamestate     = df
                          }
-- | Изменить тип тетриса(планый или ступенчатый).
switchTetrisMove :: TetrisMove -> TetrisMove
switchTetrisMove TetrisStepped = TetrisSmooth
switchTetrisMove TetrisSmooth = TetrisStepped