{-# LANGUAGE RecordWildCards #-}


module Tetris.Colliding where

import Tetris.Types

-- =========================================
-- Colliding
-- =========================================

-- |checks block for colliding another one which is below
collidesBlockSides::Coord -> Board -> Bool
collidesBlockSides block []   =   (x block < 0) || (x block  + blockSize > screenWidth)
collidesBlockSides block (c:[]) = (x block < 0) || (x block + blockSize > screenWidth) || (x block == x c) && (y block == y c)
collidesBlockSides block (c:brds) | (x block < 0) || (x block + blockSize > screenWidth) || (x block == x c) && (y block == y c)  = True
                                  | otherwise = collidesBlockSides block brds


-- |checks block for colliding another one which is on sides
collidesBlockDown::Coord -> Board-> Bool
collidesBlockDown block []      =   (y block + blockSize > screenHeight)
collidesBlockDown block (c:[])  =   ((y block + blockSize > screenHeight) || (x block == x c) && (y block == y c))
collidesBlockDown block (c:brds)  | (y block + blockSize > screenHeight) || (x block == x c) && (y block == y c)  = True
                                  |  otherwise = collidesBlockDown block brds


-- |checks block for colliding another one which is above (just in case)
collidesBlockUp::Coord -> Board-> Bool
collidesBlockUp block []  =  y block < 0
collidesBlockUp block (c:[])  =   y block < 0 || (y block == y c)
collidesBlockUp block (c:brds)  | y block < 0 || (y block == y c) = True
                                | otherwise = collidesBlockUp block brds


-- |checks if any block of the figure collides the board
collidesFigure::BlockedFigure -> Board -> Bool
collidesFigure (a,b,c,d) board = (collidesFigureSides (a,b,c,d) board) || (collidesFigureDown (a,b,c,d) board)


-- |checks if any block of the figure collides the board from the sides
collidesFigureSides::BlockedFigure -> Board -> Bool
collidesFigureSides (a,b,c,d) board | (collidesBlockSides a board) || (collidesBlockSides b board) || (collidesBlockSides c board) || (collidesBlockSides d board) = True
        |otherwise = False


-- |checks if any block of the figure collides the board just from above
collidesFigureDown::BlockedFigure -> Board -> Bool
collidesFigureDown (a,b,c,d) board | (collidesBlockDown a board) || (collidesBlockDown b board) || (collidesBlockDown c board) || (collidesBlockDown d board) = True
        |otherwise = False
