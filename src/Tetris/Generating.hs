{-# LANGUAGE RecordWildCards #-}
module Tetris.Generating where

import System.Random
import Tetris.Types

------------------------------------------------------------------------------------------------------------------------------------------

-- =========================================
-- Generating
-- =========================================


-- | generating new figure from random integer number
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


-- | building the infinite list of figures by applying the "fegFigure" function
-- to infinite list of random numbers
initFigures :: StdGen -> [Figure]
initFigures g = map genFigure (randomRs getrange g)


-- | range for random number generation
getrange :: (Int, Int)
getrange = (0, 6)
  


-- | setting up an empty board as an empty list
genEmptyBoard::Board
genEmptyBoard = []


-- | generating the start position of universe - empty board and infinite list of figures
genUniverse::StdGen -> GameState
genUniverse g = GameState genEmptyBoard (initFigures g) init_tact 0 0


-- | generating empty universe - without any figures (just for client-serer thing)
genEmptyUniverse::StdGen -> GameState
genEmptyUniverse _ = GameState genEmptyBoard [] init_tact 0 0


-------------------------------------------------------------------------------------------------------------------------------------




