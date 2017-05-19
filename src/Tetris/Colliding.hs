{-# LANGUAGE RecordWildCards #-}


module Tetris.Colliding where

import Tetris.Types


collidesBlock::Coord -> Bool
collidesBlock Coord{..} | (x < 0) || (x  + blockSize > screenWidth) || (y < 0) || (y + blockSize > screenHeight) = True
       |otherwise = False


collidesBlockSides::Coord -> Board -> Bool
collidesBlockSides block [] = (x block < 0) || (x block  + blockSize > screenWidth)
collidesBlockSides block (c:[]) = (x block < 0) || (x block + blockSize > screenWidth) || (x block == x c) && (y block == y c)
collidesBlockSides block (c:brds) | (x block < 0) || (x block + blockSize > screenWidth) || (x block == x c) && (y block == y c)  = True
                                      | otherwise = collidesBlockSides block brds


collidesBlockDown::Coord -> Board-> Bool
collidesBlockDown block []  =   (y block + blockSize > screenHeight)
collidesBlockDown block (c:[])  =   ((y block + blockSize > screenHeight) || (x block == x c) && (y block == y c))
collidesBlockDown block (c:brds)  | (y block + blockSize > screenHeight) || (x block == x c) && (y block == y c)  = True
                                      |  otherwise = collidesBlockDown block brds



collidesBlockUp::Coord -> Board-> Bool
collidesBlockUp block []  =  y block < 0
collidesBlockUp block (c:[])  =   y block < 0 && (y block == y c)
collidesBlockUp block (c:brds)  | y block < 0 && (y block == y c)  = True
                                          |  otherwise = collidesBlockUp block brds


collidesFigure::BlockedFigure -> Board -> Bool
collidesFigure (a,b,c,d) board = (collidesFigureSides (a,b,c,d) board) || (collidesFigureDown (a,b,c,d) board)

collidesFigureSides::BlockedFigure -> Board -> Bool
collidesFigureSides (a,b,c,d) board | (collidesBlockSides a board) || (collidesBlockSides b board) || (collidesBlockSides c board) || (collidesBlockSides d board) = True
        |otherwise = False


collidesFigureDown::BlockedFigure -> Board -> Bool
collidesFigureDown (a,b,c,d) board | (collidesBlockDown a board) || (collidesBlockDown b board) || (collidesBlockDown c board) || (collidesBlockDown d board) = True
        |otherwise = False
