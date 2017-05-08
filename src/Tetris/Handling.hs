module Tetris.Handling where

import System.Random
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Geometry.Line
import Graphics.Gloss.Interface.Pure.Game
import GHC.Float

import Tetris.Types
import Tetris.Drawing
import Tetris.Colliding

handleTetris :: Event -> Gamestate -> Gamestate

handleTetris (EventKey (Char 'l') Down _ _) (a,(Figure sha dir (b,c,z):rest),d,e) = moveRight (a,(Figure sha dir (b,c,z):rest),d,e)
handleTetris (EventKey (Char 'l') Up _ _) t = t

handleTetris (EventKey (Char 'j') Down _ _)  (a,(Figure sha dir (b,c,z):rest),d,e)  = moveLeft (a,(Figure sha dir (b,c,z):rest),d,e)
handleTetris (EventKey (Char 'j') Up _ _)  t  = t

handleTetris(EventKey (SpecialKey KeySpace) Down _ _ ) (a,(Figure sha dir (b,c,z):rest),d,e)  = dropit (a,(Figure sha dir (b,c,z):rest),d,e) (screenHeight-c)
handleTetris(EventKey (SpecialKey KeySpace) Up _ _ ) t = t

handleTetris (EventKey (Char 'k') Down _ _ ) (a,(Figure sha dir (b,c,z):rest),d,e) = turn (a, (Figure sha dir (b ,c,z):rest),d,e)
handleTetris (EventKey (Char 'k') Up _ _ ) t = t

handleTetris (EventKey (Char 'p') Down _ _ ) (a,(Figure sha dir (b,c,z):rest),(sp, ti),e) = (a,(Figure sha dir (b,c,z):rest),(- sp, ti),e)
handleTetris (EventKey (Char 'p') Up _ _ ) t = t

handleTetris  _ t = t  



turn::Gamestate -> Gamestate
turn (a,(Figure t DUp c):rest,d,e) | collide1 = (a,(Figure t DUp c):rest,d,e)
                                   | otherwise = (a,(Figure t DRight c):rest,d,e)
                            where 
                                collide1 = collidesFigure (figureToDraw (Figure t DRight c)) a
turn (a,(Figure t DRight c):rest,d,e) | collide2 = (a,(Figure t DRight c):rest,d,e)
                                      | otherwise = (a,(Figure t DDown c):rest,d,e)
                            where 
                                collide2 = collidesFigure (figureToDraw (Figure t DDown c)) a
turn (a,(Figure t DDown c):rest,d,e) | collide3 = (a,(Figure t DDown c):rest,d,e)
                                     | otherwise = (a,(Figure t DLeft c):rest,d,e)
                            where 
                                collide3 = collidesFigure (figureToDraw (Figure t DLeft c)) a
turn (a,(Figure t DLeft c):rest,d,e) | collide4 = (a,(Figure t DLeft c):rest,d,e)
                                     | otherwise = (a,(Figure t DUp c):rest,d,e)
                            where 
                                collide4 = collidesFigure (figureToDraw (Figure t DUp c)) a




dropit::Gamestate -> Int -> Gamestate
dropit (a,((Figure sha dir (b,c,z)):rest),d,e) pts  | collide = (a,((Figure sha dir (b,c,z)):rest),d,e+(div pts blockSize))                   
                                                  | otherwise = dropit (a,((Figure sha dir (b,c + blockSize,z)):rest),d,e) pts                                        
                                          where                                           
                                              collide = collidesFigureDown (figureToDraw (Figure sha dir (b,c + blockSize,z))) a



moveLeft::Gamestate -> Gamestate
moveLeft (a,((Figure s t (b,c,z)):rest),d,e) | collide = (a, ((Figure s t (b,c,z)):rest),d,e)
        |otherwise = (a, ((Figure s t (b - blockSize,c,z)):rest),d,e)
  where 
    collide = collidesFigureSides (figureToDraw (Figure s t (b - blockSize,c,z))) a

moveRight::Gamestate -> Gamestate
moveRight (a,(Figure s t (b,c,z)):rest,d,e) | collide = (a, ((Figure s t (b,c,z)):rest),d,e)
        |otherwise = (a, ((Figure s t (b + blockSize,c,z)):rest),d,e)
  where 
    collide = collidesFigureSides (figureToDraw (Figure s t (b + blockSize,c,z))) a


