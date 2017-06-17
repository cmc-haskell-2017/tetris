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
    bgColor = black     
    fps     = glob_fps  

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

-- | Ширина экрана.
screenWidth :: Int
screenWidth = 600

-- | Высота экрана.
screenHeight :: Int
screenHeight = 600

-- | Доска — список координат упавших блоков.
type Board = [Coord]

-- | Координаты блока x, y и его цвет clr.
data Coord = Coord 
  { x   :: Int  -- ^ Координата x.
  , y   :: Int  -- ^ Координата y.
  , clr :: Int  -- ^ Цвет блока.
  }

-- | Вариант развития событий для хода ИИ (профит, смещение, количество поворотов).
data Variant = Variant 
  { profit :: Int  -- ^ Выгода хода.
  , offset :: Int  -- ^ Смещение.
  , turns  :: Int  -- ^ Количество поворотов.
  }

-- | Скорость (время между тактами => чем меньше, тем быстрее игра).
type Speed = Float

-- | Время прошедшее с прошлого такта, сравнивается со скоростью для обновления такта.
type Time = Float

-- | Счёт.
type Score = Int

-- | Сложность ИИ.
type Difficulty = Float

-- | Состояние игры в текущий момент.
-- Разделили доску и фигуру, чтобы при полете фигуры не мигала вся доска, также, чтобы было более оптимизировано.
--
data Gamestate = Gamestate
  { board       :: Board      -- ^ Доска.
  , curfig      :: Figure     -- ^ Летящая фигура.
  , curfig1     :: Figure     -- ^ Летящая фигура № 2.
  , figures     :: [Figure]   -- ^ Список следующих фигур.
  , speed       :: Speed      -- ^ Скорость падения фигуры.
  , time        :: Time       -- ^ Время с последнего такта.
  , score       :: Score      -- ^ Счет игрока.
  , steps       :: [Int]      -- ^ Количество шагов ИИ в следующие 10 тактов.
  , difficulty  :: Difficulty -- ^ Сложность ИИ. Скорость совершения хода ИИ.
  }
  
-- | Тип фигуры соответствует букве на которую фигура похожа.
-- Для каждого типа фигуры свой конструктор, 
-- чтобы однозначно можно было определить ее и тип операций над ней.
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

-- | Координаты блоков фигуры.
type BlockedFigure = (Coord, Coord, Coord, Coord)

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
genUniverse g = Gamestate { board = genEmptyBoard, figures = tail . tail. initFigures $ g, curfig = shift2 . head . initFigures $ g
                          , curfig1 = shift1 . head . tail . initFigures $ g, speed = init_speed, time = 0, score = 0, steps = [], difficulty = 0}

shift1 :: Figure -> Figure
shift1 (Figure d t c) = (Figure d t c {x = x c + 3 * blockSize})

shift2 :: Figure -> Figure
shift2 (Figure d t c) = (Figure d t c {x = x c - 2 * blockSize})
-- =========================================
-- * Перемещения фигур.
-- =========================================

-- | Поворачивает фигуру : функция смотрит, какая ей дана фигура,
-- и вычисляет расстояние до края доски и на основании этой информации поворачивает ее (если это можно сделать).
turn :: Gamestate -> Gamestate
turn gs
  | collideturn (board gs) (curfig gs) = gs
  | collideAnother (figureToDraw . turnfigure . curfig $ gs) (figureToDraw . curfig1 $ gs) = gs
  | otherwise = gs {curfig = turnfigure . curfig $ gs}

turn1 :: Gamestate -> Gamestate
turn1 gs
  | collideturn (board gs) (curfig1 gs) = gs 
  | collideAnother (figureToDraw . turnfigure . curfig1 $ gs) (figureToDraw . curfig $ gs) = gs
  | otherwise = gs {curfig1 = turnfigure . curfig1 $ gs}

-- | Проверяет, будут ли две фигуры пересекатся
collideAnother :: BlockedFigure -> BlockedFigure -> Bool
collideAnother (a,b,c,d) c1 = or [isCrossing a c1, isCrossing b c1, isCrossing c c1, isCrossing d c1]

isCrossing :: Coord -> BlockedFigure -> Bool
isCrossing c1 (a,b,c,d) = or [overlap c1 a, overlap c1 b, overlap c1 c, overlap c1 d]

overlap :: Coord -> Coord -> Bool
overlap c1 c2 = (x c1 == x c2) && (y c1 == y c2)

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
  | collideAnother (figureToDraw . moveLeftFigure . curfig $ gs) (figureToDraw . curfig1 $ gs) = gs
  | otherwise = gs {curfig = moveLeftFigure . curfig $ gs}
  where
    collide = collidesFigureSides (figureToDraw (moveLeftFigure (curfig gs))) (board gs)

moveLeft1 :: Gamestate -> Gamestate
moveLeft1 gs
  | collide   = gs
  | collideAnother (figureToDraw .  moveLeftFigure . curfig1 $ gs) (figureToDraw . curfig $ gs) = gs
  | otherwise = gs {curfig1 = moveLeftFigure . curfig1 $ gs}
  where
    collide = collidesFigureSides (figureToDraw (moveLeftFigure (curfig1 gs))) (board gs)

-- | Насильно двигаем фигуру налево.
moveLeftFigure :: Figure -> Figure
moveLeftFigure (Figure s t c) = (Figure s t c {x = x c - blockSize})

-- | Шаг вправо.
moveRight :: Gamestate -> Gamestate
moveRight gs
  | collide = gs
  | collideAnother (figureToDraw . moveRightFigure . curfig $ gs) (figureToDraw . curfig1 $ gs) = gs
  | otherwise = gs {curfig = moveRightFigure . curfig $ gs}
  where
    collide = collidesFigureSides (figureToDraw (moveRightFigure (curfig gs))) (board gs)

moveRight1 :: Gamestate -> Gamestate
moveRight1 gs
  | collide = gs
  | collideAnother (figureToDraw . moveRightFigure . curfig1 $ gs) (figureToDraw . curfig $ gs) = gs
  | otherwise = gs {curfig1 = moveRightFigure . curfig1 $ gs}
  where
    collide = collidesFigureSides (figureToDraw (moveRightFigure (curfig1 gs))) (board gs)

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
collidesBlockSides c (c1 : []) = (x c < 0) || (x c + blockSize > screenWidth) || (x c == x c1) && (y c == y c1)
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

-- | Проверка, пересекает ли фfигура боковые границы окна, либо доску.
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
isGameOver gs = (collidesFigureDown (figureToDraw (head . figures $ gs)) (board gs)) || (collidesFigureDown (figureToDraw (shift1 . head . figures $ gs)) (board gs))

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
isFullRow r = (length r) == div (screenWidth) blockSize

-- | При нажатии клавиши "вниз" роняет фигуру.
dropit :: Gamestate -> Int -> Gamestate
dropit gs pts
  | collide1  = dropit1 gs pts
  | collide   = gs {score = score gs + (div pts blockSize)}
  | otherwise = dropit gs {curfig = moveDownFigure . curfig $ gs} pts
  where
    collide1 = collideAnother (figureToDraw . moveDownFigure . curfig $ gs) (figureToDraw . curfig1 $ gs)
    collide  = collidesFigureDown (figureToDraw (moveDownFigure . curfig $ gs)) (board gs)

dropit1 :: Gamestate -> Int -> Gamestate
dropit1 gs pts
  | collide1  = dropit gs pts
  | collide   = gs {score = score gs + (div pts blockSize)}
  | otherwise = dropit1 gs {curfig1 = moveDownFigure . curfig1 $ gs} pts
  where
    collide1 = collideAnother (figureToDraw . moveDownFigure . curfig1 $ gs) (figureToDraw . curfig $ gs)
    collide  = collidesFigureDown (figureToDraw (moveDownFigure . curfig1 $ gs)) (board gs)

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
drawFigure gs = pictures 
				[drawBlockedFigure (figureToDraw . curfig $ gs)
				, drawBlockedFigure (figureToDraw . curfig1 $ gs)]

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
  , drawBoard      . board           $ gs
  , drawScore      . score           $ gs
  , drawNext
  , drawNextFigure . head. figures   $ gs
  ]

-- | Рисуем счет.
drawScore :: Score -> Picture
drawScore scr = translate (-w) h (scale 30 30 (pictures
  [ color green (polygon [ (0, 0.1),   (0, -2),     (4, -2),     (4, 0.1)   ])  
  , color black  (polygon [ (0.1, 0.2), (0.1, -1.9), (3.9, -1.9), (3.9, 0.2) ])  
  , translate 0.1 (-1.5) (scale 0.01 0.01 (color green (text (show scr))))       
  ]))
  where
    w = fromIntegral screenWidth  / 2
    h = fromIntegral screenHeight / 2

-- | Рисуем следующую фигуру
drawNext :: Picture
drawNext = translate (-w) h (scale 30 30 (pictures 
  [ color green (polygon [ (16.5, 0.1),   (16.5, -5),     (20, -5),    (20, 0.1)  ])  
  , color black  (polygon [ (16.6, 0.2), (16.6, -4.9), (19.9, -4.9), (19.9, 0.2) ])  
  ]))
  where
    w = fromIntegral screenWidth  / 2
    h = fromIntegral screenHeight / 2

drawNextFigure :: Figure -> Picture
drawNextFigure (Figure sh d c)  = drawBlockedFigure( figureToDraw (Figure sh d c {x = 535, y = 60} ))


-- | Рисуем сложность ИИ.
drawDifficulty :: Difficulty -> Picture
drawDifficulty dif = translate (-w) h (scale 30 30 (pictures
  [ color green (polygon [ (7, 0.1),   (7, -2),     (10, -2),    (10, 0.1)  ])  
  , color black  (polygon [ (7.1, 0.2), (7.1, -1.9), (9.9, -1.9), (9.9, 0.2) ])  
  , translate 7.5 (-1.5) (scale 0.01 0.01 (color blue (text (show dif))))         
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

updateBoard1 :: Gamestate -> Board
updateBoard1 gs = board gs ++ vectolist (figureToDraw . curfig1 $ gs)

-- | Аргумент функции 'play', обновляет состояние тетриса.
-- С каждым кадром двигает фигуру вниз и пока здесь же проверяет,
-- не достигла ли фигура нижней границы.
updateTetris :: Float -> Gamestate -> Gamestate
updateTetris dt gs
  | isGameOver gs = Gamestate { board = genEmptyBoard, curfig = head . figures $ gs, curfig1 = shift1 . head . tail . figures $ gs, figures = tail . tail . figures $ gs
                              , speed = init_speed, score = 0, time = 0, steps = [], difficulty = difficulty gs}
  | otherwise = newLevel (newTact gs dt (speed gs))

-- ===========================================
-- * Искусственный интеллект
-- =======================================

-- Строт список длины 'n' из копий элемента 'a'
listNum :: a -> Int -> [a]
listNum _ 0 = []
listNum e1 d = e1 : listNum e1 (d - 1)

-- | В зависимости от сложности строит список количества ходов ИИ
listSteps :: Float -> [Int]
listSteps d = distribute (listNum (floor d) 10) (round (d * 10) - (floor d) * 10)

-- | Равномерно распределяем ходы
distribute :: [Int] -> Int -> [Int]
distribute [] _ = []
distribute [e1] d = [e1 + d]
distribute s d = distribute (evens s) (div d 2 + mod d 2) ++ distribute (odds s) (div d 2)

-- | Оставляем в списке только эдементы на нечетных позициях
evens :: [a] -> [a]
evens [] = []
evens [e1] = [e1]
evens (e1:_:xs) = e1 : evens xs

-- | Оставляем в списке только эдементы на четных позициях
odds :: [a] -> [a]
odds [] = []
odds [_] = []
odds (_:e2:xs) = e2 : odds xs

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

-- | в 'newTact' вызывается 'makeStep' n раз. Т.е ИИ делает n хода в такт. n зависит от сложности
makeStep :: Gamestate -> Gamestate
makeStep gs
  | newStep   = gs 
  | needturn  = turn      gs
  | needleft  = moveLeft  gs
  | needright = moveRight gs
  | otherwise = dropit    gs (screenHeight - (heightFigure . curfig $ gs))
    where
      newStep   = heightFigure (curfig  gs)  < 4 * blockSize 
      needturn  = turns  (bestStep gs) > 0
      needleft  = offset (bestStep gs) < 0
      needright = offset (bestStep gs) > 0

-- | ИИ ходит.
makingSteps :: Int -> Gamestate -> Gamestate
makingSteps 0 gs = gs
makingSteps n gs = makingSteps (n - 1) (makeStep gs)

-- ===========================================
-- * Время
-- =======================================

-- | Новый такт.
newTact :: Gamestate -> Float -> Float -> Gamestate
newTact gs dt tact
  | paused = gs
  | new && collides && collides1 = gs {board = deleteRows . sortRows . updateBoard $ gs {board = updateBoard1 gs}, curfig = shift2 . head . figures $ gs
                                      , curfig1 = shift1 . head . tail . figures $ gs, figures = tail . tail . figures $ gs, score = score gs + 1}
  | new && collides = gs {board = deleteRows . sortRows . updateBoard $ gs, curfig = shift2 . head . figures $ gs, figures = tail . figures $ gs, score = score gs + 1 }
  | new && collides1 = gs {board = deleteRows . sortRows . updateBoard1 $ gs, curfig1 = shift1 . head . figures $ gs, figures = tail . figures $ gs, score = score gs + 1}
  | new && null (steps gs) = newTact gs {steps = (listSteps (difficulty gs))} dt tact 
  | new = newTact (makingSteps (head (steps gs)) gs {curfig = moveDownFigure (curfig gs), curfig1 = moveDownFigure (curfig1 gs)
                                                    , time = 0, steps = (tail . steps $ gs)}) (dt + (time gs) - tact) tact
  | collides = gs {time = time gs + dt + tact * 0.3}
  | collides1 = gs {time = time gs + dt + tact * 0.3}
  | otherwise = gs {time = time gs + dt}
  where
    new = time gs + dt >= tact
    collides  = collidesFigureDown (figureToDraw . moveDownFigure . curfig  $ gs) (board gs)
    collides1 = collidesFigureDown (figureToDraw . moveDownFigure . curfig1 $ gs) (board gs)
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

handleTetris (EventKey (Char 'd') Down _ _) gs = moveRight1 gs
handleTetris (EventKey (Char 'd') Up _ _)   t  = t

handleTetris (EventKey (Char 'a') Down _ _) gs = moveLeft1 gs
handleTetris (EventKey (Char 'a') Up _ _)   t  = t

handleTetris(EventKey (SpecialKey KeyEnter) Down _ _ ) gs = dropit gs (screenHeight - (heightFigure . curfig $ gs))
handleTetris(EventKey (SpecialKey KeyEnter) Up _ _ )   t  = t

handleTetris (EventKey (SpecialKey KeySpace) Down _ _) gs = dropit1 gs (screenHeight - (heightFigure . curfig1 $ gs))
handleTetris (EventKey (SpecialKey KeySpace) Up _ _)   t  = t

handleTetris (EventKey (Char 'k') Down _ _ ) gs = turn gs
handleTetris (EventKey (Char 'k') Up _ _ )   t  = t

handleTetris (EventKey (Char 's') Down _ _ ) gs = turn1 gs
handleTetris (EventKey (Char 's') Up _ _ )   t  = t

handleTetris (EventKey (Char 'p') Down _ _ ) gs = gs {speed = - (speed gs)}
handleTetris (EventKey (Char 'p') Up _ _ )   t  = t

--handleTetris (EventKey (Char 'w') Down _ _ ) gs = gs {difficulty = (difficulty gs) + 0.5}
--handleTetris (EventKey (Char 'w') Up _ _ )   t  = t

--handleTetris (EventKey (Char 's') Down _ _ ) gs = gs {difficulty = (difficulty gs) - 0.5}
--handleTetris (EventKey (Char 's') Up _ _ )   t  = t

handleTetris  _ t = t
