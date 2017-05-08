module Tetris.Colliding where

import System.Random
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Geometry.Line
import Graphics.Gloss.Interface.Pure.Game
import GHC.Float


import Tetris.Types
import Tetris.Generating
import Tetris.Drawing


collidesBlock::Coord -> Bool
collidesBlock (a,b,z) | (a < 0) || (a  + blockSize > screenWidth) || (b < 0) || (b + blockSize > screenHeight) = True
       |otherwise = False


collidesBlockSides::Coord -> Board -> Bool
collidesBlockSides (a,b,z) [] = (a < 0) || (a  + blockSize > screenWidth)
collidesBlockSides (a,b,z) ((brda, brdb,z1):[]) = (a < 0) || (a  + blockSize > screenWidth) || (a==brda) && (b==brdb)
collidesBlockSides (a,b,z) ((brda, brdb,z1):brds) | (a < 0) || (a  + blockSize > screenWidth) || (a==brda) && (b==brdb)  = True
                                             | otherwise = collidesBlockSides (a,b,z) brds


collidesBlockDown::Coord -> Board-> Bool
collidesBlockDown (a,b,z) []  =   (b + blockSize > screenHeight)
collidesBlockDown (a,b,z) ((brda,brdb,z1):[])  =   ((b + blockSize > screenHeight) || (a==brda) && (b==brdb))
collidesBlockDown (a,b,z) ((brda,brdb,z1):brds)  | (b + blockSize > screenHeight) || (a==brda) && (b==brdb)  = True
                                            |  otherwise = collidesBlockDown (a,b,z) brds



collidesBlockUp::Coord -> Board-> Bool
collidesBlockUp (a,b,z) []  =  b < 0
collidesBlockUp (a,b,z) ((brda,brdb,z1):[])  =   (b < 0 && (b==brdb))
collidesBlockUp (a,b,z) ((brda,brdb,z1):brds)  | b < 0 && (b==brdb)  = True
                                          |  otherwise = collidesBlockUp (a,b,z) brds


collidesFigure::BlockedFigure -> Board -> Bool
collidesFigure (a,b,c,d) board = (collidesFigureSides (a,b,c,d) board) || (collidesFigureDown (a,b,c,d) board)

collidesFigureSides::BlockedFigure -> Board -> Bool
collidesFigureSides (a,b,c,d) board | (collidesBlockSides a board) || (collidesBlockSides b board) || (collidesBlockSides c board) || (collidesBlockSides d board) = True
        |otherwise = False


collidesFigureDown::BlockedFigure -> Board -> Bool
collidesFigureDown (a,b,c,d) board | (collidesBlockDown a board) || (collidesBlockDown b board) || (collidesBlockDown c board) || (collidesBlockDown d board) = True
        |otherwise = False
