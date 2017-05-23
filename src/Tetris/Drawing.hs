{-# LANGUAGE RecordWildCards #-}


module Tetris.Drawing where

import Graphics.Gloss.Interface.Pure.Game

import Tetris.Types


-- =========================================
-- Preparing to draw
-- =========================================


-- | Converting figure into the list of blocks to draw
figureToDraw :: Figure -> BlockedFigure
figureToDraw (Figure O d c) = figureToDrawO (Figure O d c)
figureToDraw (Figure I d c) = figureToDrawI (Figure I d c)
figureToDraw (Figure T d c) = figureToDrawT (Figure T d c)
figureToDraw (Figure J d c) = figureToDrawJ (Figure J d c)
figureToDraw (Figure L d c) = figureToDrawL (Figure L d c)
figureToDraw (Figure S d c) = figureToDrawS (Figure S d c)
figureToDraw (Figure Z d c) = figureToDrawZ (Figure Z d c)


-- | Converting square into the list of blocks to draw
figureToDrawO :: Figure -> BlockedFigure
figureToDrawO (Figure _ _ c) 
  = (c, c {x = x c + bs}, c {y = y c - bs}, c {x = x c + bs, y = y c - bs})
  where
    bs = blockSize

-- | Converting stick into the list of blocks to draw
figureToDrawI :: Figure -> BlockedFigure
figureToDrawI (Figure _ d c) 
  | (d == DUp) || (d == DDown) = (c {y = y c + bs}, c, c {y = y c - bs}, c {y = y c - 2*bs})
  | otherwise                  = (c {x = x c - bs}, c, c {x = x c + bs}, c {x = x c + 2*bs})
  where
    bs = blockSize

-- | Converting "Z" into the list of blocks to draw
figureToDrawZ :: Figure -> BlockedFigure
figureToDrawZ (Figure _ d c) 
  | (d == DUp) || (d == DDown) = (c {x = x c - bs, y = y c - bs}, c {x = x c - bs}, c,      c {y = y c + bs})
  | otherwise                  = (c {x = x c - bs},      c,      c {y = y c - bs}, c {x = x c + bs, y = y c - bs})
  where
    bs = blockSize

-- | Converting "S" into the list of blocks to draw
figureToDrawS :: Figure -> BlockedFigure
figureToDrawS (Figure _ d c) 
  | (d == DUp) || (d == DDown) = (c {x = x c - bs, y = y c + bs}, c {x = x c - bs}, c,      c {y = y c - bs})
  | otherwise                  = (c {x = x c - bs},      c,      c {y = y c + bs}, c {x = x c + bs, y = y c + bs})
  where
    bs = blockSize

-- | Converting "J" into the list of blocks to draw
figureToDrawJ :: Figure -> BlockedFigure
figureToDrawJ (Figure _ d c) 
  | d == DDown  = (c {x = x c - bs, y = y c - bs}, c {y = y c - bs}, c,      c {y = y c + bs})
  | d == DUp    = (c {y = y c - bs},      c,      c {y = y c + bs}, c {x = x c + bs, y = y c + bs})
  | d == DRight = (c {x = x c - bs},      c,      c {x = x c + bs}, c {x = x c + bs, y = y c - bs})
  | otherwise   = (c {x = x c - bs, y = y c + bs}, c {x = x c - bs}, c,      c {x = x c + bs})
  where
    bs = blockSize

-- | Converting "L" into the list of blocks to draw
figureToDrawL :: Figure -> BlockedFigure
figureToDrawL (Figure _ d c) 
  | d == DDown  = (c {y = y c + bs},      c,      c {y = y c - bs}, c {x = x c + bs, y = y c - bs})
  | d == DUp    = (c {y = y c - bs},      c,      c {y = y c + bs}, c {x = x c - bs, y = y c + bs})
  | d == DRight = (c {x = x c - bs},      c,      c {x = x c + bs}, c {x = x c + bs, y = y c + bs})
  | otherwise   = (c {x = x c - bs, y = y c - bs}, c {x = x c - bs}, c,      c {x = x c + bs})
  where
    bs = blockSize

-- | Converting "T" into the list of blocks to draw
figureToDrawT :: Figure -> BlockedFigure
figureToDrawT (Figure _ d c) 
  | d == DDown  = (c {x = x c - bs}, c, c {x = x c + bs}, c {y = y c - bs})
  | d == DUp    = (c {x = x c - bs}, c, c {x = x c + bs}, c {y = y c + bs})
  | d == DRight = (c {y = y c + bs}, c, c {y = y c - bs}, c {x = x c + bs})
  | otherwise   = (c {y = y c + bs}, c, c {y = y c - bs}, c {x = x c - bs})
  where
    bs = blockSize


-- =========================================
-- Actual Drawing
-- =========================================


-- | function that draws all the blocks that belong to the board
drawBoard ::  Int -> Board -> Picture
drawBoard bias s = pictures (map (drawBlock bias) s)


-- | draws the frame of the block
magframe :: Int -> Int -> [Picture]
magframe b c = 
  [ color black  (polygon [ (fromIntegral b,        fromIntegral (-c))
                            , (fromIntegral b,        fromIntegral (-c - 2))
                            , (fromIntegral (b + 30), fromIntegral (-c - 2))
                            , (fromIntegral (b + 30), fromIntegral (-c)) 
                            ])
  , color black  (polygon [ (fromIntegral b,        fromIntegral (-c))
                            , (fromIntegral b,        fromIntegral (-c - 30))
                            , (fromIntegral (b + 2),  fromIntegral (-c - 30))
                            , (fromIntegral (b + 2),  fromIntegral (-c))
                            ])
  , color black  (polygon [ (fromIntegral b,        fromIntegral (-c - 28))
                            , (fromIntegral b,        fromIntegral (-c - 30))
                            , (fromIntegral (b + 30), fromIntegral (-c - 30))
                            , (fromIntegral (b + 30), fromIntegral (-c - 28))
                            ])
  , color black  (polygon [ (fromIntegral (b + 28), fromIntegral (-c))
                            , (fromIntegral (b + 28), fromIntegral (-c - 30))
                            , (fromIntegral (b + 30), fromIntegral (-c - 30))
                            , (fromIntegral (b + 30), fromIntegral (-c)) 
                            ])
  ]

-- | Converting digits into the color names
numtocolor :: Int -> Color
numtocolor 0 = makeColor 1 0.843137 0 1
numtocolor 1 = makeColor 0 1 1 1
numtocolor 2 = makeColor 0.627451 0.12549 0.941176 1
numtocolor 3 = makeColor 0 0 0.803922 1
numtocolor 4 = makeColor 1 0.647059 0 1
numtocolor 5 = makeColor 0.196078 0.803922 0.196078 1
numtocolor _ = makeColor 1 0 0 1


-- | Drawing the block
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


-- | drawing the figure (after converting in to the bunch of blocks)
drawFigure :: Int -> GameState ->  Picture
drawFigure bias GameState{..} | null figures = Blank
                                 | otherwise = drawBlockedFigure bias (figureToDraw $ head figures)



-- | drawing the figure represented as a bunch of blocks
drawBlockedFigure :: Int -> BlockedFigure -> Picture
drawBlockedFigure bias ((a, b, c, d)) =   pictures  [drawBlock bias a ,
                                                     drawBlock bias b ,
                                                     drawBlock bias c ,
                                                     drawBlock bias d ]


-- | drawing the whle tetris
drawTetris :: Int -> GameState -> Picture
drawTetris bias gs@GameState{..} = pictures
  [   
  drawFigure  bias gs 
    , drawBoard   bias board 
    , drawScore   bias score
  ] 


-- | drawing the score and the frame for it
drawScore :: Int -> Score -> Picture
drawScore bias score = translate (-w) h (scale 30 30 (pictures
  [ color yellow (polygon [ (0, 0), (0, -2), (6, -2), (6, 0) ])            -- белая рамка
  , color black (polygon [ (0, 0), (0, -1.9), (5.9, -1.9), (5.9, 0) ])    -- чёрные внутренности
  , translate 2 (-1.5) (scale 0.01 0.01 (color green (text (show score))))  -- красный счёт
  ]))
  where
    w = fromIntegral screenWidth  / 2 + fromIntegral bias
    h = fromIntegral screenHeight / 2