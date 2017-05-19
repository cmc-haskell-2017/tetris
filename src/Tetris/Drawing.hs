{-# LANGUAGE RecordWildCards #-}


module Tetris.Drawing where

import Graphics.Gloss.Interface.Pure.Game

import Tetris.Types

------------

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

-- =========================================
-- Drawing
-- =========================================

drawBoard ::  Int -> Board -> Picture
drawBoard bias s = pictures (map (drawBlock bias) s)

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
drawBlock :: Int -> Coord-> Picture
drawBlock bias crd 
  =  pictures [ translate (-w) h (scale  1 1 (pictures (
                  [ color (numtocolor clr1) (polygon [ (fromIntegral b,         fromIntegral (-c))
                                                    , (fromIntegral b,         fromIntegral (-c - 30))
                                                    , (fromIntegral  (b + 30), fromIntegral (-c - 30))
                                                    , (fromIntegral  (b + 30), fromIntegral (- c))
                                                    ])
                  ] ++ (magframe b c)))) ]
  where
    w = fromIntegral screenWidth  / 2 + fromIntegral bias
    h = fromIntegral screenHeight / 2
    b = x crd
    c = y crd
    clr1 = clr crd



-- drawBlock :: Int -> Coord -> Picture

-- drawBlock bias (b,c,1) =  pictures [ translate (-w) h (scale  1 1 (pictures
--  [ color blue  (polygon [ ( fromIntegral b, fromIntegral (-c)), (fromIntegral b, fromIntegral (-c - 30)), (fromIntegral  (b + 30),fromIntegral (-c - 30)), (fromIntegral  (b + 30),fromIntegral (- c)) ])            -- белая рамка
--    ,color magenta  (polygon [ ( fromIntegral b, fromIntegral (-c)), (fromIntegral b, fromIntegral (-c - 2)), (fromIntegral  (b + 30),fromIntegral (-c - 2)), (fromIntegral  (b + 30),fromIntegral (- c)) ])
--    ,color magenta  (polygon [ ( fromIntegral b, fromIntegral (-c)), (fromIntegral b, fromIntegral (-c - 30)), (fromIntegral  (b + 2),fromIntegral (-c-30 )), (fromIntegral  (b +2),fromIntegral (- c)) ])
--    ,color magenta  (polygon [ ( fromIntegral b, fromIntegral (-c-28)), (fromIntegral b, fromIntegral (-c - 30)), (fromIntegral  (b + 30),fromIntegral (-c - 30)), (fromIntegral  (b + 30),fromIntegral (- c-28)) ])
--    ,color magenta  (polygon [ ( fromIntegral b+28, fromIntegral (-c)), (fromIntegral b+28, fromIntegral (-c - 30)), (fromIntegral  (b + 30),fromIntegral (-c - 30)), (fromIntegral  (b + 30),fromIntegral (- c)) ])

--    ]))
--     ]
--   where
--   w = fromIntegral screenWidth  / 2 + fromIntegral bias
--   h = fromIntegral screenHeight / 2
-- drawBlock bias (b,c,2) =  pictures [ translate (-w) h (scale  1 1 (pictures
--  [ color yellow  (polygon [ ( fromIntegral b, fromIntegral (-c)), (fromIntegral b, fromIntegral (-c - 30)), (fromIntegral  (b + 30),fromIntegral (-c - 30)), (fromIntegral  (b + 30),fromIntegral (- c)) ])            -- белая рамка
--    ,color magenta  (polygon [ ( fromIntegral b, fromIntegral (-c)), (fromIntegral b, fromIntegral (-c - 2)), (fromIntegral  (b + 30),fromIntegral (-c - 2)), (fromIntegral  (b + 30),fromIntegral (- c)) ])
--    ,color magenta  (polygon [ ( fromIntegral b, fromIntegral (-c)), (fromIntegral b, fromIntegral (-c - 30)), (fromIntegral  (b + 2),fromIntegral (-c-30 )), (fromIntegral  (b +2),fromIntegral (- c)) ])
--    ,color magenta  (polygon [ ( fromIntegral b, fromIntegral (-c-28)), (fromIntegral b, fromIntegral (-c - 30)), (fromIntegral  (b + 30),fromIntegral (-c - 30)), (fromIntegral  (b + 30),fromIntegral (- c-28)) ])
--    ,color magenta  (polygon [ ( fromIntegral b+28, fromIntegral (-c)), (fromIntegral b+28, fromIntegral (-c - 30)), (fromIntegral  (b + 30),fromIntegral (-c - 30)), (fromIntegral  (b + 30),fromIntegral (- c)) ])
--    ]))]
--   where
--   w = fromIntegral screenWidth  / 2 + fromIntegral bias
--   h = fromIntegral screenHeight / 2
-- drawBlock bias (b,c,3) =  pictures [ translate (-w) h (scale  1 1 (pictures
--  [ color red  (polygon [ ( fromIntegral b, fromIntegral (-c)), (fromIntegral b, fromIntegral (-c - 30)), (fromIntegral  (b + 30),fromIntegral (-c - 30)), (fromIntegral  (b + 30),fromIntegral (- c)) ])            -- белая рамка
--    ,color magenta  (polygon [ ( fromIntegral b, fromIntegral (-c)), (fromIntegral b, fromIntegral (-c - 2)), (fromIntegral  (b + 30),fromIntegral (-c - 2)), (fromIntegral  (b + 30),fromIntegral (- c)) ])
--    ,color magenta  (polygon [ ( fromIntegral b, fromIntegral (-c)), (fromIntegral b, fromIntegral (-c - 30)), (fromIntegral  (b + 2),fromIntegral (-c-30 )), (fromIntegral  (b +2),fromIntegral (- c)) ])
--    ,color magenta  (polygon [ ( fromIntegral b, fromIntegral (-c-28)), (fromIntegral b, fromIntegral (-c - 30)), (fromIntegral  (b + 30),fromIntegral (-c - 30)), (fromIntegral  (b + 30),fromIntegral (- c-28)) ])
--    ,color magenta  (polygon [ ( fromIntegral b+28, fromIntegral (-c)), (fromIntegral b+28, fromIntegral (-c - 30)), (fromIntegral  (b + 30),fromIntegral (-c - 30)), (fromIntegral  (b + 30),fromIntegral (- c)) ])
--    ]))]
--   where
--   w = fromIntegral screenWidth  / 2 + fromIntegral bias
--   h = fromIntegral screenHeight / 2
-- drawBlock bias (b,c,4) =  pictures [ translate (-w) h (scale  1 1 (pictures
--  [ color green  (polygon [ ( fromIntegral b, fromIntegral (-c)), (fromIntegral b, fromIntegral (-c - 30)), (fromIntegral  (b + 30),fromIntegral (-c - 30)), (fromIntegral  (b + 30),fromIntegral (- c)) ])            -- белая рамка
--    ,color magenta  (polygon [ ( fromIntegral b, fromIntegral (-c)), (fromIntegral b, fromIntegral (-c - 2)), (fromIntegral  (b + 30),fromIntegral (-c - 2)), (fromIntegral  (b + 30),fromIntegral (- c)) ])
--    ,color magenta  (polygon [ ( fromIntegral b, fromIntegral (-c)), (fromIntegral b, fromIntegral (-c - 30)), (fromIntegral  (b + 2),fromIntegral (-c-30 )), (fromIntegral  (b +2),fromIntegral (- c)) ])
--    ,color magenta  (polygon [ ( fromIntegral b, fromIntegral (-c-28)), (fromIntegral b, fromIntegral (-c - 30)), (fromIntegral  (b + 30),fromIntegral (-c - 30)), (fromIntegral  (b + 30),fromIntegral (- c-28)) ])
--    ,color magenta  (polygon [ ( fromIntegral b+28, fromIntegral (-c)), (fromIntegral b+28, fromIntegral (-c - 30)), (fromIntegral  (b + 30),fromIntegral (-c - 30)), (fromIntegral  (b + 30),fromIntegral (- c)) ])
--    ]))]
--   where
--   w = fromIntegral screenWidth  / 2 + fromIntegral bias
--   h = fromIntegral screenHeight / 2 
-- drawBlock bias (b,c,5) =  pictures [ translate (-w) h (scale  1 1 (pictures
--  [ color orange  (polygon [ ( fromIntegral b, fromIntegral (-c)), (fromIntegral b, fromIntegral (-c - 30)), (fromIntegral  (b + 30),fromIntegral (-c - 30)), (fromIntegral  (b + 30),fromIntegral (- c)) ])            -- белая рамка
--    ,color magenta  (polygon [ ( fromIntegral b, fromIntegral (-c)), (fromIntegral b, fromIntegral (-c - 2)), (fromIntegral  (b + 30),fromIntegral (-c - 2)), (fromIntegral  (b + 30),fromIntegral (- c)) ])
--    ,color magenta  (polygon [ ( fromIntegral b, fromIntegral (-c)), (fromIntegral b, fromIntegral (-c - 30)), (fromIntegral  (b + 2),fromIntegral (-c-30 )), (fromIntegral  (b +2),fromIntegral (- c)) ])
--    ,color magenta  (polygon [ ( fromIntegral b, fromIntegral (-c-28)), (fromIntegral b, fromIntegral (-c - 30)), (fromIntegral  (b + 30),fromIntegral (-c - 30)), (fromIntegral  (b + 30),fromIntegral (- c-28)) ])
--    ,color magenta  (polygon [ ( fromIntegral b+28, fromIntegral (-c)), (fromIntegral b+28, fromIntegral (-c - 30)), (fromIntegral  (b + 30),fromIntegral (-c - 30)), (fromIntegral  (b + 30),fromIntegral (- c)) ])
--    ]))]
--   where
--   w = fromIntegral screenWidth  / 2 + fromIntegral bias
--   h = fromIntegral screenHeight / 2


-- drawBlock bias (b,c,_) =  pictures [ translate (-w) h (scale  1 1 (pictures
--  [ color white  (polygon [ ( fromIntegral b, fromIntegral (-c)), (fromIntegral b, fromIntegral (-c - 30)), (fromIntegral  (b + 30),fromIntegral (-c - 30)), (fromIntegral  (b + 30),fromIntegral (- c)) ])            -- белая рамка
--    ,color magenta  (polygon [ ( fromIntegral b, fromIntegral (-c)), (fromIntegral b, fromIntegral (-c - 2)), (fromIntegral  (b + 30),fromIntegral (-c - 2)), (fromIntegral  (b + 30),fromIntegral (- c)) ])
--    ,color magenta  (polygon [ ( fromIntegral b, fromIntegral (-c)), (fromIntegral b, fromIntegral (-c - 30)), (fromIntegral  (b + 2),fromIntegral (-c-30 )), (fromIntegral  (b +2),fromIntegral (- c)) ])
--    ,color magenta  (polygon [ ( fromIntegral b, fromIntegral (-c-28)), (fromIntegral b, fromIntegral (-c - 30)), (fromIntegral  (b + 30),fromIntegral (-c - 30)), (fromIntegral  (b + 30),fromIntegral (- c-28)) ])
--    ,color magenta  (polygon [ ( fromIntegral b+28, fromIntegral (-c)), (fromIntegral b+28, fromIntegral (-c - 30)), (fromIntegral  (b + 30),fromIntegral (-c - 30)), (fromIntegral  (b + 30),fromIntegral (- c)) ])
--    ]))]
--   where
--   w = fromIntegral screenWidth  / 2 + fromIntegral bias
--   h = fromIntegral screenHeight / 2


drawFigure :: Int -> GameState ->  Picture
drawFigure bias GameState{..} | null figures = Blank
                                 | otherwise = drawBlockedFigure bias (figureToDraw $ head figures)



drawBlockedFigure :: Int -> BlockedFigure -> Picture
drawBlockedFigure bias ((a, b, c, d)) =   pictures  [drawBlock bias a ,
                                                     drawBlock bias b ,
                                                     drawBlock bias c ,
                                                     drawBlock bias d ]


--Рисуем тетрис
drawTetris :: Int -> GameState -> Picture
drawTetris bias gs@GameState{..} = pictures
  [   
  drawFigure  bias gs 
    , drawBoard   bias board 
    , drawScore   bias score
  ] 


drawScore :: Int -> Score -> Picture
drawScore bias score = translate (-w) h (scale 30 30 (pictures
  [ color yellow (polygon [ (0, 0), (0, -2), (6, -2), (6, 0) ])            -- белая рамка
  , color black (polygon [ (0, 0), (0, -1.9), (5.9, -1.9), (5.9, 0) ])    -- чёрные внутренности
  , translate 2 (-1.5) (scale 0.01 0.01 (color green (text (show score))))  -- красный счёт
  ]))
  where
    w = fromIntegral screenWidth  / 2 + fromIntegral bias
    h = fromIntegral screenHeight / 2