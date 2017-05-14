module Tetris where

import System.Random
import Graphics.Gloss.Interface.Pure.Game

-- | Главная функция.
run :: IO ()
run = do
 g <- newStdGen
 play display bgColor fps (genUniverse g) drawTetris handleTetris updateTetris
   where
    display = InWindow "Tetris" (screenWidth, screenHeight) (200, 200)
    bgColor = black   -- цвет фона
    fps     = glob_fps   -- кол-во кадров в секунду

-- =========================================
-- * Типы
-- =========================================

-- | Сколько кадров в секунду отрисовывается.
glob_fps :: Int
glob_fps = 60

-- | Ширина и высота блока в пикселях.
blockSize :: Int
blockSize = 30

-- | Начальная скорость падения фигуры.
init_speed :: Speed
init_speed = 0.7

-- | Доска — список координат упавших блоков.
type Board = [Coord]

-- | Вариант развития событий для хода ИИ (профит, смещение, количество поворотов).
data Variant = Variant 
  { profit :: Int  -- ^ Выгода хода.
  , offset :: Int  -- ^ Смещение.
  , turns  :: Int  -- ^ Количество поворотов.
  }

-- | Счёт.
type Score = Int

-- | Координаты блока x, y и его цвет clr.
data Coord = Coord 
  { x   :: Int  -- ^ Координата x.
  , y   :: Int  -- ^ Координата y.
  , clr :: Int  -- ^ Цвет блока.
  }

-- | Время прошедшее с прошлого такта, сравнивается со скоростью для обновления такта.
type Time = Float

-- | Состояние игры в текущий момент.
-- Разделили доску и фигуру, чтобы при полете фигуры не мигала вся доска, также, чтобы было более оптимизировано.
-- '[Figure]' — бесконечный список фигур, в текущем состоянии берем первый элемент списка.
--
data Gamestate = Gamestate
  { board   :: Board    -- ^ Доска.
  , curfig  :: Figure   -- ^ Летящая фигура.
  , figures :: [Figure] -- ^ Список следующих фигур.
  , speed   :: Speed    -- ^ Скорость падения фигуры.
  , time    :: Time     -- ^ Время с последнего такта.
  , score   :: Score    -- ^ Счет игрока.
  }
  
-- | Скорость (время между тактами => чем меньше, тем быстрее игра).
type Speed = Float

-- | Тип фигуры соответствует букве на которую фигура похожа.
-- Для каждого типа фигуры свой конструктор, 
-- чтобы однозначно можно было определить ее и тип операций над ней, 
-- например, фигуру I можно вращать произвольно только на расстоянии больше 4 клеток от края,
-- а фигуру O на расстоянии больше 2 клеток от края.
data FigureType = O | I | T | J | L | S | Z
                      deriving(Eq, Show)

-- | Направление фигуры.
data Direction
  = DUp    -- ^ Фигура направелена вверх.
  | DDown  -- ^ Фигура направелена вниз.
  | DLeft  -- ^ Фигура направелена влево.
  | DRight -- ^ Фигура направелена вправо.
  deriving(Eq, Show)

-- | Фигура определяется типом, направлением, координатами верхнего левого блока.
data Figure = Figure FigureType Direction Coord
  --deriving(Show)

-- | Ширина экрана.
screenWidth :: Int
screenWidth = 300

-- | Высота экрана.
screenHeight :: Int
screenHeight = 600

-- =========================================
-- * Генерация
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

-- | Генерируем бесконечный список из случайных фигур.
initFigures :: StdGen -> [Figure]
initFigures g = map genFigure (randomRs range g)
  where
    range = (0, 6)

-- | Пустая доска.
genEmptyBoard :: Board
genEmptyBoard = [ (\x1 -> Coord {x = bs * x1, y = bs * 20, clr = 0}) dx | dx <- [1..9] ]
  where bs = blockSize

-- | Генерируем игровую вселенную (пустая доска, бесконечный список фигур, начальные скорость, время и счет).
genUniverse :: StdGen -> Gamestate
genUniverse g = Gamestate {board = genEmptyBoard, figures = tail . initFigures $ g, curfig = head . initFigures $ g, speed = init_speed, time = 0, score = 0}

-- =========================================
-- * Перемещения фигур.
-- =========================================

-- | Координаты блоков фигуры.
type BlockedFigure = (Coord, Coord, Coord, Coord)

-- | Поворачивает фигуру : функция смотрит, какая ей дана фигура,
-- и вычисляет расстояние до края доски и на основании этой информации поворачивает ее (если это можно сделать).
turn :: Gamestate -> Gamestate
turn gs
  | collideturn (board gs) (curfig gs) = gs
  | otherwise = gs {curfig = turnfigure . curfig $ gs}

-- | Будет ли фигура пересекать доску, если ее повернуть.
collideturn :: Board -> Figure -> Bool
collideturn b (Figure t d c) = collidesFigure (figureToDraw (Figure t (nextdirection d) c)) b

-- | Поворачиваем фигуру насильно.
turnfigure :: Figure -> Figure
turnfigure (Figure t d c) = Figure t (nextdirection d) c

-- | Следующее по часовой стрелке направление фигуры.
nextdirection :: Direction -> Direction
nextdirection DUp    = DRight
nextdirection DRight = DDown
nextdirection DDown  = DLeft
nextdirection DLeft  = DUp


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
  | (d == DUp) || (d == DDown) = (c {x = x c - bs, y = y c - bs}, c {x = x c - bs}, c,      c {y = y c + bs})
  | otherwise                  = (c {x = x c - bs},      c,      c {y = y c - bs}, c {x = x c + bs, y = y c - bs})
  where
    bs = blockSize

-- | Готовим правый зигзаг к отрисовке. Возвращаем координаты 4 блоков.
figureToDrawS :: Figure -> BlockedFigure
figureToDrawS (Figure _ d c) 
  | (d == DUp) || (d == DDown) = (c {x = x c - bs, y = y c + bs}, c {x = x c - bs}, c,      c {y = y c - bs})
  | otherwise                  = (c {x = x c - bs},      c,      c {y = y c + bs}, c {x = x c + bs, y = y c + bs})
  where
    bs = blockSize

-- | Готовим Г-образную фигуру к отрисовке. Возвращаем координаты 4 блоков.
figureToDrawJ :: Figure -> BlockedFigure
figureToDrawJ (Figure _ d c) 
  | d == DDown  = (c {x = x c - bs, y = y c - bs}, c {y = y c - bs}, c,      c {y = y c + bs})
  | d == DUp    = (c {y = y c - bs},      c,      c {y = y c + bs}, c {x = x c + bs, y = y c + bs})
  | d == DRight = (c {x = x c - bs},      c,      c {x = x c + bs}, c {x = x c + bs, y = y c - bs})
  | otherwise   = (c {x = x c - bs, y = y c + bs}, c {x = x c - bs}, c,      c {x = x c + bs})
  where
    bs = blockSize

-- | Готовим L-образную фигуру к отрисовке. Возвращаем координаты 4 блоков.
figureToDrawL :: Figure -> BlockedFigure
figureToDrawL (Figure _ d c) 
  | d == DDown  = (c {y = y c + bs},      c,      c {y = y c - bs}, c {x = x c + bs, y = y c - bs})
  | d == DUp    = (c {y = y c - bs},      c,      c {y = y c + bs}, c {x = x c - bs, y = y c + bs})
  | d == DRight = (c {x = x c - bs},      c,      c {x = x c + bs}, c {x = x c + bs, y = y c + bs})
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

-- | Шаг влево.
moveLeft :: Gamestate -> Gamestate
moveLeft gs
  | collide   = gs
  | otherwise = gs {curfig = moveLeftFigure . curfig $ gs}
  where
    collide = collidesFigureSides (figureToDraw (moveLeftFigure (curfig gs))) (board gs)

-- | Насильно двигаем фигуру налево.
moveLeftFigure :: Figure -> Figure
moveLeftFigure (Figure s t c) = (Figure s t c {x = x c - blockSize})

-- | Шаг вправо.
moveRight :: Gamestate -> Gamestate
moveRight gs
  | collide = gs
  | otherwise = gs {curfig = moveRightFigure . curfig $ gs}
  where
    collide = collidesFigureSides (figureToDraw (moveRightFigure (curfig gs))) (board gs)

-- | Насильно двигаем фигуру направо.
moveRightFigure :: Figure -> Figure
moveRightFigure (Figure s t c) = (Figure s t c {x = x c + blockSize})

-- | Проверка, пересекает ли блок границы игрового окна.
collidesBlock :: Coord -> Bool
collidesBlock c
  | (a < 0) || (a + blockSize > screenWidth) || (b < 0) || (b + blockSize > screenHeight) = True
  | otherwise = False
  where
    a = x c
    b = y c

-- | Проверка, пересекает ли блок боковые границы окна, либо доску.
collidesBlockSides :: Coord -> Board -> Bool
collidesBlockSides c []                     = (x c < 0) || (x c + blockSize > screenWidth)
collidesBlockSides c (c1 : []) = (x c < 0) || (x c + blockSize > screenWidth) || (x c == x c1) && (x c == y c1)
collidesBlockSides c (c1 : brds) 
  | (x c < 0) || (x c + blockSize > screenWidth) || (x c==x c1) && (y c==y c1)  = True
  | otherwise = collidesBlockSides c brds

-- | Проверка, пересекает ли блок пол или доску.
collidesBlockDown :: Coord -> Board-> Bool
collidesBlockDown c []                      = (y c + blockSize > screenHeight)
collidesBlockDown c (c1 : [])  = (y c + blockSize > screenHeight) || (x c==x c1) && (y c==y c1)
collidesBlockDown c (c1 : brds)  
  | (y c + blockSize > screenHeight) || (x c == x c1) && (y c == y c1)  = True
  |  otherwise = collidesBlockDown c brds

-- | Проверка, пересекается ли блок потолок или доску.
collidesBlockUp :: Coord -> Board-> Bool
collidesBlockUp c []                    =  y c < 0
collidesBlockUp c (c1 : [])   = (y c < 0) && (y c == y c1)
collidesBlockUp c (c1 : brds)  
  | y c < 0 && (y c == y c1)  = True
  | otherwise                 = collidesBlockUp c brds

-- | Пересекает ли фигура доску или границы?
collidesFigure :: BlockedFigure -> Board -> Bool
collidesFigure (a, b, c, d) brd = or
  [ collidesFigureSides (a, b, c, d) brd
  , collidesFigureDown  (a, b, c, d) brd ]

-- | Проверка, пересекает ли фигура боковые границы окна, либо доску.
collidesFigureSides :: BlockedFigure -> Board -> Bool
collidesFigureSides (a, b, c, d) brd 
  | (collidesBlockSides a brd) || (collidesBlockSides b brd) || (collidesBlockSides c brd) || (collidesBlockSides d brd) = True
  | otherwise = False

-- | Проверка, что фигура касается снизу доски или поля.
collidesFigureDown :: BlockedFigure -> Board -> Bool
collidesFigureDown (a, b, c, d) brd 
  | (collidesBlockDown a brd) || (collidesBlockDown b brd) || (collidesBlockDown c brd) || (collidesBlockDown d brd) = True
  | otherwise = False

-- | Проверка, закончилась ли игра. (Следующая фигура пересеклась с доской).
isGameOver :: Gamestate -> Bool
isGameOver gs = collidesFigureDown (figureToDraw (head . figures $ gs)) (board gs)

-- | Сортируем строки.
sortRows :: Board -> Board
sortRows []     = []
sortRows (c : brds) = sortRows (filter (\c1 -> y c1 > y c) brds) ++ [c] ++ sortRows (filter (\c1 -> y c1 <= y c) brds)

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
isFullRow r = (length r) == 10

-- | При нажатии клавиши "вниз" роняет фигуру.
dropit :: Gamestate -> Int -> Gamestate
dropit gs pts
  | collide   = gs {score = score gs + (div pts blockSize)}
  | otherwise = dropit gs {curfig = moveDownFigure . curfig $ gs} pts
  where
    collide = collidesFigureDown (figureToDraw (moveDownFigure . curfig $ gs)) (board gs)

-- | Насильно сдвигаем фигуру вниз.
moveDownFigure :: Figure -> Figure
moveDownFigure (Figure sha dir c) = (Figure sha dir c {y = (y c) + blockSize} )

-- | Рисуем доску.
drawBoard :: Board  -> Picture
drawBoard s = pictures (map drawBlock s)

-- | Фуксиновая рамка для блоков.
magframe :: Int -> Int -> [Picture]
magframe b c = 
  [ color magenta  (polygon [ (fromIntegral b,        fromIntegral (-c))
                            , (fromIntegral b,        fromIntegral (-c - 2))
                            , (fromIntegral (b + 30), fromIntegral (-c - 2))
                            , (fromIntegral (b + 30), fromIntegral (-c)) 
                            ])
  , color magenta  (polygon [ (fromIntegral b,        fromIntegral (-c))
                            , (fromIntegral b,        fromIntegral (-c - 30))
                            , (fromIntegral (b + 2),  fromIntegral (-c - 30))
                            , (fromIntegral (b + 2),  fromIntegral (-c))
                            ])
  , color magenta  (polygon [ (fromIntegral b,        fromIntegral (-c - 28))
                            , (fromIntegral b,        fromIntegral (-c - 30))
                            , (fromIntegral (b + 30), fromIntegral (-c - 30))
                            , (fromIntegral (b + 30), fromIntegral (-c - 28))
                            ])
  , color magenta  (polygon [ (fromIntegral (b + 28), fromIntegral (-c))
                            , (fromIntegral (b + 28), fromIntegral (-c - 30))
                            , (fromIntegral (b + 30), fromIntegral (-c - 30))
                            , (fromIntegral (b + 30), fromIntegral (-c)) 
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

-- | Рисуем блок.
drawBlock :: Coord-> Picture
drawBlock  crd 
  =  pictures [ translate (-w) h (scale  1 1 (pictures (
                  [ color (numtocolor clr1) (polygon [ (fromIntegral b,         fromIntegral (-c))
                                                    , (fromIntegral b,         fromIntegral (-c - 30))
                                                    , (fromIntegral  (b + 30), fromIntegral (-c - 30))
                                                    , (fromIntegral  (b + 30), fromIntegral (- c))
                                                    ])
                  ] ++ (magframe b c)))) ]
  where
    w = fromIntegral screenWidth  / 2
    h = fromIntegral screenHeight / 2
    b = x crd
    c = y crd
    clr1 = clr crd
-- | Рисуем фигуру.
drawFigure :: Gamestate  ->  Picture
drawFigure gs = drawBlockedFigure (figureToDraw . curfig $ gs)

-- | Рисуем блоки фигуры.
drawBlockedFigure :: BlockedFigure -> Picture
drawBlockedFigure ((a, b, c, d)) = pictures 
  [ drawBlock a
  , drawBlock b
  , drawBlock c
  , drawBlock d 
  ]

-- | Рисуем тетрис.
drawTetris :: Gamestate-> Picture
drawTetris gs = pictures
  [ drawFigure gs
  , drawBoard . board $ gs
  , drawScore . score $ gs
  ]

-- | Рисуем счет.
drawScore :: Score -> Picture
drawScore scr = translate (-w) h (scale 30 30 (pictures
  [ color yellow (polygon [ (0, 0), (0, -2), (6, -2), (6, 0) ])             -- белая рамка
  , color black (polygon [ (0, 0), (0, -1.9), (5.9, -1.9), (5.9, 0) ])      -- чёрные внутренности
  , translate 2 (-1.5) (scale 0.01 0.01 (color green (text (show scr))))    -- красный счёт
  ]))
  where
    w = fromIntegral screenWidth  / 2
    h = fromIntegral screenHeight / 2

-- =========================================
-- * Просчёт кадров (обновление)
-- =========================================

-- | Преобразует вектор координат в список.
vectolist :: (Coord, Coord, Coord, Coord) -> [Coord]
vectolist (a, b, c, d) = [a, b, c, d]

-- | Добавляет в доску упавшую фигуру.
updateBoard :: Gamestate -> Board
updateBoard gs = board gs ++ vectolist (figureToDraw . curfig $ gs)

-- | Аргумент функции 'play', обновляет состояние тетриса.
-- С каждым кадром двигает фигуру вниз и пока здесь же проверяет,
-- не достигла ли фигура нижней границы.
updateTetris :: Float -> Gamestate -> Gamestate
updateTetris dt gs
  | isGameOver gs = Gamestate {board = genEmptyBoard, curfig = head . figures $ gs, figures = tail . figures $ gs, speed = init_speed, score = 0, time = 0}
  | otherwise = newLevel (newTact gs dt (speed gs))

-- ===========================================
-- * Искусственный интеллект
-- =======================================

-- | Оценка состояния доски, после сделанного хода.
-- Мы хотим максисизировать количество удаленных строк, минимизировать количество дырок, минимизировать высоту тетриса.
--
-- Эта функция — ядро ИИ.
boardProfit :: Board -> Int
boardProfit b = 2000 + 10000 * (numberDeletes b) - 100 * (numberHoles b) - 10 * ((avgBoardHeight b) - (boardHeight b))

-- | Функция сравнения двух элементов доски (нужна для упорядочивания доски).
greater :: Coord -> Coord -> Bool
greater c1 c2
  | x1 > x2 = True
  | (x1 == x2) && (y1 < y2) = True
  | otherwise = False
  where
    x1 = x c1
    x2 = x c2
    y1 = y c1
    y2 = y c2
-- | Сортирует доску по столбцам. Столбцы также отсортировываются.
sortBoard :: Board -> Board
sortBoard []           = []
sortBoard (brd : brds) = sortBoard (filter (\x1 -> greater x1 brd ) brds)
                      ++ [brd]
                      ++ sortBoard (filter (\x1 -> not (greater x1 brd)) brds)

-- | Сравнивает, какой вариант лучше.
best :: Variant -> Variant -> Variant
best v1 v2
  | profit v1 >= profit v2 = v1
  | otherwise = v2

-- | Выбирает наилучший вариант развития событий.
bestVariant :: [Variant] -> Variant
bestVariant []        = Variant {profit = 0, offset = -1, turns = 0}
bestVariant (v1 : vs) = best v1 (bestVariant vs)

-- | Высота доски. Имеется ввиду высочайшая точка доски.
boardHeight :: Board -> Int
boardHeight brds  = minimum (map (\c1 -> y c1) brds)

-- | Средняя высота доски.
avgBoardHeight :: Board -> Int
avgBoardHeight b = div (sumBoardHeight b) 10

-- | Сумма высот столбцов доски (функция упорядочивает доску, а сама сумма считается в 'sumhofb').
sumBoardHeight :: Board -> Int
sumBoardHeight b = sumhofb (sortBoard b)

-- | Сумма высот столбцов доски (принимает упорядоченную доску, и считает сумму высот её столбцов).
sumhofb :: Board -> Int
sumhofb [] = 0
sumhofb (c : hs) = y1 + sumhofb (filter (\c1 -> not (x c1 == x1)) (c : hs))
  where
    x1 = x c
    y1 = y c

-- | Количество дырок в доске.
numberHoles :: Board -> Int
numberHoles b = nh (sortBoard b)

-- | Сумма дырок в столбцах.
nh :: Board -> Int
nh [] = 0
nh (c : hs) = nhcolumn (filter (\c1 -> x c1 == x1) (c : hs))  + nh (filter (\c1 -> not (x c1 == x1)) (c : hs))
  where
    x1 = x c

-- | Количество дырок в столбце.
nhcolumn :: [Coord] -> Int
nhcolumn [] = 0
nhcolumn (c1 : hs) = (div (screenHeight - (y c1)) blockSize) - length (c1 : hs)

-- | Количество удаленных строк, после сделанного хода.
numberDeletes :: Board -> Int
numberDeletes b = (boardHeight . deleteRows . sortRows $ b) - (boardHeight b)

-- | Сортируем варианты по профиту, количеству поворотова, смещению.
sortVariants :: [Variant] -> [Variant]
sortVariants []     = []
sortVariants (v : vs)     = sortVariants (filter (\x1 -> better x1 v) vs) ++ [v] ++ sortVariants (filter (\x1 -> not (better x1 v)) vs)

-- | Функция сравнения двух вариантов.
better :: Variant -> Variant -> Bool
better v1 v2 
  | p1 > p2 = True
  | (p1 == p2) && (r1 < r2) = True
  | (p1 == p2) && (r1 == r2) && ((abs dx1) < (abs dx2)) = True
  | otherwise = False
  where
    p1  = profit v1
    p2  = profit v2
    dx1 = offset v1 
    dx2 = offset v2
    r1  = turns v1
    r2  = turns v2

-- | Анализирует 'Gamestate'.
-- Возвращает 'Variant' (профит, смещение от центра, количество поворотов).
-- Отрицательное смещение - двигаемся влево. Иначе - вправо.
bestStep :: Gamestate -> Variant
bestStep gs = bestVariant (sortVariants [ genVariant gs dx r | dx <- [-5..4], r <- [0..3] ])

-- | Применяет функцию 'f' 'n' раз к сущности 'а'.
apply :: (a -> a) -> Int -> a -> a
apply _ 0 par = par
apply f num par = apply f  (num - 1) (f par)

-- | Генерирует вариант развития событий.
genVariant :: Gamestate -> Int -> Int -> Variant
genVariant gs dx r = Variant
  { profit = boardProfit (updateBoard (dropit (move (rot gs)) (screenHeight - (heightFigure (curfig gs)))))
  , offset = dx
  , turns = r
  } 
  where
    rot = apply turn r
    move
      | dx > 0    = apply moveRight dx
      | otherwise = apply moveLeft (abs dx)

-- | На какой высоте находится фигура.
heightFigure :: Figure -> Int
heightFigure (Figure _ _ c) = y c

-- | в 'newTact' вызывается 'makeStep' 4 раза. Т.е ИИ делает 4 хода в такт.
makeStep :: Gamestate -> Gamestate
makeStep gs
  | needturn  = turn      gs
  | needleft  = moveLeft  gs
  | needright = moveRight gs
  | otherwise = dropit    gs (screenHeight - (heightFigure . curfig $ gs))
    where
      needturn  = turns  (bestStep gs) > 0
      needleft  = offset (bestStep gs) < 0
      needright = offset (bestStep gs) > 0

-- ===========================================
-- * Время
-- =======================================

-- | Новый такт.
newTact :: Gamestate -> Float -> Float -> Gamestate
newTact gs dt tact
  | paused = gs
  | new && collides = gs {board = deleteRows . sortRows . updateBoard $ gs, curfig = head (figures gs), figures = tail . figures $ gs, score = score gs + 1 }
  | new = newTact (makeStep(makeStep(makeStep(makeStep gs {curfig = moveDownFigure (curfig gs), time = 0})))) (dt + (time gs) - tact) tact
  | collides = gs {time = time gs + dt + tact * 0.3}
  | otherwise = gs {time = time gs + dt}
  where
    new = time gs + dt >= tact
    collides =  collidesFigureDown (figureToDraw . moveDownFigure . curfig $ gs) (board gs)
    paused = speed gs < 0

-- | Увеличивает скорость падения фигур, в зависимости от количества набранных очков.
newLevel :: Gamestate -> Gamestate
newLevel gs
  | l5 = gs {speed = signum(speed gs) * 0.1} 
  | l4 = gs {speed = signum(speed gs) * 0.15}
  | l3 = gs {speed = signum(speed gs) * 0.2}
  | l2 = gs {speed = signum(speed gs) * 0.25}
  | l2 = gs {speed = signum(speed gs) * 0.3}
  | l1 = gs {speed = signum(speed gs) * 0.4}
  | otherwise = gs
  where
    l5 = score gs >= 5000
    l4 = score gs >= 3000 && (score gs) <= 5000
    l3 = score gs >= 2000 && (score gs) <= 3000
    l2 = score gs >= 1500 && (score gs) <= 2000
    l1 = score gs >= 1000 && (score gs) <= 1500

-- | Аргумент функции 'play', которая говорит, что делает каждая клавиша.
handleTetris :: Event -> Gamestate -> Gamestate

handleTetris (EventKey (Char 'l') Down _ _) gs = moveRight gs
handleTetris (EventKey (Char 'l') Up _ _)   t  = t

handleTetris (EventKey (Char 'j') Down _ _)  gs = moveLeft gs
handleTetris (EventKey (Char 'j') Up _ _)    t  = t

handleTetris(EventKey (SpecialKey KeySpace) Down _ _ ) gs = dropit gs (screenHeight - (heightFigure . curfig $ gs))
handleTetris(EventKey (SpecialKey KeySpace) Up _ _ )   t  = t

handleTetris (EventKey (Char 'k') Down _ _ ) gs = turn gs
handleTetris (EventKey (Char 'k') Up _ _ )   t  = t

handleTetris (EventKey (Char 'p') Down _ _ ) gs = gs {speed = - (speed gs)}
handleTetris (EventKey (Char 'p') Up _ _ )   t  = t

handleTetris  _ t = t

