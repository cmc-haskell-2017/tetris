module Main where

import System.Random
-- import Graphics.Gloss.Data.Vector
-- import Graphics.Gloss.Geometry.Line
import Graphics.Gloss.Interface.Pure.Game
-- import GHC.Float

import Tetris

main :: IO ()
main = run

run :: IO ()
run = do
 g <- newStdGen
 play display bgColor fps (genUniverse g ) drawTetris handleTetris updateTetris
   where
    display = InWindow "Tetris" (screenWidth, screenHeight) (200, 200)
    bgColor = black   -- цвет фона
    fps     = glob_fps   -- кол-во кадров в секунду