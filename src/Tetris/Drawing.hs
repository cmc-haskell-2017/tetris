module Tetris.Drawing where

import System.Random
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Geometry.Line
import Graphics.Gloss.Interface.Pure.Game
import GHC.Float

import Tetris.Types

------------


figureToDraw::Figure->BlockedFigure
figureToDraw (Figure O d c) = figureToDrawO (Figure O d c)
figureToDraw (Figure I d c) = figureToDrawI (Figure I d c)
figureToDraw (Figure T d c) = figureToDrawT (Figure T d c)
figureToDraw (Figure J d c) = figureToDrawJ (Figure J d c)
figureToDraw (Figure L d c) = figureToDrawL (Figure L d c)
figureToDraw (Figure S d c) = figureToDrawS (Figure S d c)
figureToDraw (Figure Z d c) = figureToDrawZ (Figure Z d c)


-- =========================================
-- Getting ready to draw
-- =========================================


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


-- =========================================
-- Drawing
-- =========================================

drawBoard ::  Int -> Board -> Picture
drawBoard bias s = pictures (map (drawBlock bias) s)

drawBlock :: Int -> Coord -> Picture

drawBlock bias (b,c,1) =  pictures [ translate (-w) h (scale  1 1 (pictures
 [ color blue  (polygon [ ( fromIntegral b, fromIntegral (-c)), (fromIntegral b, fromIntegral (-c - 30)), (fromIntegral  (b + 30),fromIntegral (-c - 30)), (fromIntegral  (b + 30),fromIntegral (- c)) ])            -- белая рамка
   ,color magenta  (polygon [ ( fromIntegral b, fromIntegral (-c)), (fromIntegral b, fromIntegral (-c - 2)), (fromIntegral  (b + 30),fromIntegral (-c - 2)), (fromIntegral  (b + 30),fromIntegral (- c)) ])
   ,color magenta  (polygon [ ( fromIntegral b, fromIntegral (-c)), (fromIntegral b, fromIntegral (-c - 30)), (fromIntegral  (b + 2),fromIntegral (-c-30 )), (fromIntegral  (b +2),fromIntegral (- c)) ])
   ,color magenta  (polygon [ ( fromIntegral b, fromIntegral (-c-28)), (fromIntegral b, fromIntegral (-c - 30)), (fromIntegral  (b + 30),fromIntegral (-c - 30)), (fromIntegral  (b + 30),fromIntegral (- c-28)) ])
   ,color magenta  (polygon [ ( fromIntegral b+28, fromIntegral (-c)), (fromIntegral b+28, fromIntegral (-c - 30)), (fromIntegral  (b + 30),fromIntegral (-c - 30)), (fromIntegral  (b + 30),fromIntegral (- c)) ])

   ]))
    ]
  where
  w = fromIntegral screenWidth  / 2 + fromIntegral bias
  h = fromIntegral screenHeight / 2
drawBlock bias (b,c,2) =  pictures [ translate (-w) h (scale  1 1 (pictures
 [ color yellow  (polygon [ ( fromIntegral b, fromIntegral (-c)), (fromIntegral b, fromIntegral (-c - 30)), (fromIntegral  (b + 30),fromIntegral (-c - 30)), (fromIntegral  (b + 30),fromIntegral (- c)) ])            -- белая рамка
   ,color magenta  (polygon [ ( fromIntegral b, fromIntegral (-c)), (fromIntegral b, fromIntegral (-c - 2)), (fromIntegral  (b + 30),fromIntegral (-c - 2)), (fromIntegral  (b + 30),fromIntegral (- c)) ])
   ,color magenta  (polygon [ ( fromIntegral b, fromIntegral (-c)), (fromIntegral b, fromIntegral (-c - 30)), (fromIntegral  (b + 2),fromIntegral (-c-30 )), (fromIntegral  (b +2),fromIntegral (- c)) ])
   ,color magenta  (polygon [ ( fromIntegral b, fromIntegral (-c-28)), (fromIntegral b, fromIntegral (-c - 30)), (fromIntegral  (b + 30),fromIntegral (-c - 30)), (fromIntegral  (b + 30),fromIntegral (- c-28)) ])
   ,color magenta  (polygon [ ( fromIntegral b+28, fromIntegral (-c)), (fromIntegral b+28, fromIntegral (-c - 30)), (fromIntegral  (b + 30),fromIntegral (-c - 30)), (fromIntegral  (b + 30),fromIntegral (- c)) ])
   ]))]
  where
  w = fromIntegral screenWidth  / 2 + fromIntegral bias
  h = fromIntegral screenHeight / 2
drawBlock bias (b,c,3) =  pictures [ translate (-w) h (scale  1 1 (pictures
 [ color red  (polygon [ ( fromIntegral b, fromIntegral (-c)), (fromIntegral b, fromIntegral (-c - 30)), (fromIntegral  (b + 30),fromIntegral (-c - 30)), (fromIntegral  (b + 30),fromIntegral (- c)) ])            -- белая рамка
   ,color magenta  (polygon [ ( fromIntegral b, fromIntegral (-c)), (fromIntegral b, fromIntegral (-c - 2)), (fromIntegral  (b + 30),fromIntegral (-c - 2)), (fromIntegral  (b + 30),fromIntegral (- c)) ])
   ,color magenta  (polygon [ ( fromIntegral b, fromIntegral (-c)), (fromIntegral b, fromIntegral (-c - 30)), (fromIntegral  (b + 2),fromIntegral (-c-30 )), (fromIntegral  (b +2),fromIntegral (- c)) ])
   ,color magenta  (polygon [ ( fromIntegral b, fromIntegral (-c-28)), (fromIntegral b, fromIntegral (-c - 30)), (fromIntegral  (b + 30),fromIntegral (-c - 30)), (fromIntegral  (b + 30),fromIntegral (- c-28)) ])
   ,color magenta  (polygon [ ( fromIntegral b+28, fromIntegral (-c)), (fromIntegral b+28, fromIntegral (-c - 30)), (fromIntegral  (b + 30),fromIntegral (-c - 30)), (fromIntegral  (b + 30),fromIntegral (- c)) ])
   ]))]
  where
  w = fromIntegral screenWidth  / 2 + fromIntegral bias
  h = fromIntegral screenHeight / 2
drawBlock bias (b,c,4) =  pictures [ translate (-w) h (scale  1 1 (pictures
 [ color green  (polygon [ ( fromIntegral b, fromIntegral (-c)), (fromIntegral b, fromIntegral (-c - 30)), (fromIntegral  (b + 30),fromIntegral (-c - 30)), (fromIntegral  (b + 30),fromIntegral (- c)) ])            -- белая рамка
   ,color magenta  (polygon [ ( fromIntegral b, fromIntegral (-c)), (fromIntegral b, fromIntegral (-c - 2)), (fromIntegral  (b + 30),fromIntegral (-c - 2)), (fromIntegral  (b + 30),fromIntegral (- c)) ])
   ,color magenta  (polygon [ ( fromIntegral b, fromIntegral (-c)), (fromIntegral b, fromIntegral (-c - 30)), (fromIntegral  (b + 2),fromIntegral (-c-30 )), (fromIntegral  (b +2),fromIntegral (- c)) ])
   ,color magenta  (polygon [ ( fromIntegral b, fromIntegral (-c-28)), (fromIntegral b, fromIntegral (-c - 30)), (fromIntegral  (b + 30),fromIntegral (-c - 30)), (fromIntegral  (b + 30),fromIntegral (- c-28)) ])
   ,color magenta  (polygon [ ( fromIntegral b+28, fromIntegral (-c)), (fromIntegral b+28, fromIntegral (-c - 30)), (fromIntegral  (b + 30),fromIntegral (-c - 30)), (fromIntegral  (b + 30),fromIntegral (- c)) ])
   ]))]
  where
  w = fromIntegral screenWidth  / 2 + fromIntegral bias
  h = fromIntegral screenHeight / 2 
drawBlock bias (b,c,5) =  pictures [ translate (-w) h (scale  1 1 (pictures
 [ color orange  (polygon [ ( fromIntegral b, fromIntegral (-c)), (fromIntegral b, fromIntegral (-c - 30)), (fromIntegral  (b + 30),fromIntegral (-c - 30)), (fromIntegral  (b + 30),fromIntegral (- c)) ])            -- белая рамка
   ,color magenta  (polygon [ ( fromIntegral b, fromIntegral (-c)), (fromIntegral b, fromIntegral (-c - 2)), (fromIntegral  (b + 30),fromIntegral (-c - 2)), (fromIntegral  (b + 30),fromIntegral (- c)) ])
   ,color magenta  (polygon [ ( fromIntegral b, fromIntegral (-c)), (fromIntegral b, fromIntegral (-c - 30)), (fromIntegral  (b + 2),fromIntegral (-c-30 )), (fromIntegral  (b +2),fromIntegral (- c)) ])
   ,color magenta  (polygon [ ( fromIntegral b, fromIntegral (-c-28)), (fromIntegral b, fromIntegral (-c - 30)), (fromIntegral  (b + 30),fromIntegral (-c - 30)), (fromIntegral  (b + 30),fromIntegral (- c-28)) ])
   ,color magenta  (polygon [ ( fromIntegral b+28, fromIntegral (-c)), (fromIntegral b+28, fromIntegral (-c - 30)), (fromIntegral  (b + 30),fromIntegral (-c - 30)), (fromIntegral  (b + 30),fromIntegral (- c)) ])
   ]))]
  where
  w = fromIntegral screenWidth  / 2 + fromIntegral bias
  h = fromIntegral screenHeight / 2


drawBlock bias (b,c,_) =  pictures [ translate (-w) h (scale  1 1 (pictures
 [ color white  (polygon [ ( fromIntegral b, fromIntegral (-c)), (fromIntegral b, fromIntegral (-c - 30)), (fromIntegral  (b + 30),fromIntegral (-c - 30)), (fromIntegral  (b + 30),fromIntegral (- c)) ])            -- белая рамка
   ,color magenta  (polygon [ ( fromIntegral b, fromIntegral (-c)), (fromIntegral b, fromIntegral (-c - 2)), (fromIntegral  (b + 30),fromIntegral (-c - 2)), (fromIntegral  (b + 30),fromIntegral (- c)) ])
   ,color magenta  (polygon [ ( fromIntegral b, fromIntegral (-c)), (fromIntegral b, fromIntegral (-c - 30)), (fromIntegral  (b + 2),fromIntegral (-c-30 )), (fromIntegral  (b +2),fromIntegral (- c)) ])
   ,color magenta  (polygon [ ( fromIntegral b, fromIntegral (-c-28)), (fromIntegral b, fromIntegral (-c - 30)), (fromIntegral  (b + 30),fromIntegral (-c - 30)), (fromIntegral  (b + 30),fromIntegral (- c-28)) ])
   ,color magenta  (polygon [ ( fromIntegral b+28, fromIntegral (-c)), (fromIntegral b+28, fromIntegral (-c - 30)), (fromIntegral  (b + 30),fromIntegral (-c - 30)), (fromIntegral  (b + 30),fromIntegral (- c)) ])
   ]))]
  where
  w = fromIntegral screenWidth  / 2 + fromIntegral bias
  h = fromIntegral screenHeight / 2


drawFigure :: Int -> Gamestate ->  Picture
drawFigure bias (b,(f:fs),s,t) = drawBlockedFigure bias (figureToDraw f)
drawFigure _ (b,[],s,t) = Blank



drawBlockedFigure :: Int -> BlockedFigure -> Picture
drawBlockedFigure bias ((a, b, c, d)) =   pictures  [drawBlock bias a ,
                                                     drawBlock bias b ,
                                                     drawBlock bias c ,
                                                     drawBlock bias d ]


--Рисуем тетрис
drawTetris :: Int -> Gamestate -> Picture
drawTetris bias (b,fs,s,t) = pictures
  [ drawFigure  bias (b,fs,s,t) ,
    drawBoard   bias b ,
    drawScore   bias t
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