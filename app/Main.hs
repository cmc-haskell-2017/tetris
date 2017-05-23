module Main where

import System.Random
import Graphics.Gloss.Interface.Pure.Game

import Tetris


-- | main function for a single player game mode
main :: IO ()
main = run


-- | run function for a single player game mode
run :: IO ()
run = do
 g <- newStdGen
 play display bgColor fps (genUniverse g ) (drawTetris 0) handleTetris updateTetris
   where
    display = InWindow "Tetris" (screenWidth, screenHeight) (200, 200)
    bgColor = black   
    fps     = glob_fps  