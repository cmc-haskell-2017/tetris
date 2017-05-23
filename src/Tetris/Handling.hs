{-# LANGUAGE RecordWildCards #-}


module Tetris.Handling where


import Graphics.Gloss.Interface.Pure.Game


import Tetris.Types
import Tetris.Drawing
import Tetris.Colliding


-- =========================================
-- Handling
-- =========================================


-- | handling all events coming from keyboard (only for local game mode)
handleTetris :: Event -> GameState -> GameState
handleTetris (EventKey (Char 'l') Down _ _) gs | paused = gs
                                               | otherwise = moveRight gs
            where paused = speed gs < 0
handleTetris (EventKey (Char 'l') Up _ _) t = t

handleTetris (EventKey (Char 'j') Down _ _)  gs  | paused = gs
                                                 | otherwise = moveLeft gs
            where paused = speed gs < 0
handleTetris (EventKey (Char 'j') Up _ _)  t  = t

handleTetris(EventKey (SpecialKey KeySpace) Down _ _ ) gs | paused = gs
                                                 | otherwise = dropit (screenHeight - (y $ coord $ head (figures gs))) gs
            where paused = speed gs < 0
handleTetris(EventKey (SpecialKey KeySpace) Up _ _ ) t = t

handleTetris (EventKey (Char 'k') Down _ _ ) gs  | paused = gs
                                                 | otherwise = turn gs
            where paused = speed gs < 0
handleTetris (EventKey (Char 'k') Up _ _ ) t = t

handleTetris (EventKey (Char 'p') Down _ _ ) gs = pause gs
handleTetris (EventKey (Char 'p') Up _ _ ) t = t

handleTetris  _ t = t  


-- | pauses the game (in fact it just reverses the value of speed so
-- the updating function knows that the game is paused
pause :: GameState -> GameState
pause GameState{..} = GameState board figures (- speed) time score


-- | update gamestate for turning current figure to the right
turn::GameState -> GameState
turn GameState{..} = GameState board ((turnFigure board (head figures)) : (tail figures)) speed time score


-- | actually turns given figure
turnFigure :: Board -> Figure -> Figure
turnFigure board (Figure t DUp c)    | collide1 = Figure t DUp c
                                     | otherwise = Figure t DRight c
                            where 
                                collide1 = collidesFigure (figureToDraw (Figure t DRight c)) board
turnFigure board (Figure t DRight c) | collide2 = Figure t DRight c
                                     | otherwise = Figure t DDown c
                            where 
                                collide2 = collidesFigure (figureToDraw (Figure t DDown c)) board
turnFigure board (Figure t DDown c)  | collide3 = Figure t DDown c
                                     | otherwise = Figure t DLeft c
                            where 
                                collide3 = collidesFigure (figureToDraw (Figure t DLeft c)) board
turnFigure board (Figure t DLeft c)  | collide4 = Figure t DLeft c
                                     | otherwise = Figure t DUp c
                            where 
                                collide4 = collidesFigure (figureToDraw (Figure t DUp c)) board



-- | drops figure down to the board 
dropit :: Int -> GameState -> GameState
dropit pts GameState{..} | collide = GameState board figures speed time (score + div pts blockSize)                
                         | otherwise = dropit pts (GameState board ((moveFigureDown $ head figures) : tail figures) speed time score)
                            where                                           
                                collide = collidesFigureDown (figureToDraw (moveFigureDown $ head figures)) board


-- | updates gamestate to move figure left
moveLeft::GameState -> GameState
moveLeft gs@GameState{..} | collide = gs
                          | otherwise = GameState board ((moveFigureLeft $ head figures) : (tail figures)) speed time score
    where 
      collide = collidesFigureSides (figureToDraw $ moveFigureLeft $ head figures) board


-- | updates gamestate to move figure right
moveRight::GameState -> GameState
moveRight gs@GameState{..} | collide = gs
                           | otherwise = GameState board ((moveFigureRight $ head figures) : (tail figures)) speed time score
    where 
      collide = collidesFigureSides (figureToDraw $ moveFigureRight $ head figures) board


-- | just moves given figure down for one blocksize
moveFigureDown :: Figure -> Figure
moveFigureDown (Figure t d c) = Figure t d (Coord (x c) (y c + blockSize) (clr c))


-- | just moves given figure right for one blocksize
moveFigureRight :: Figure -> Figure
moveFigureRight (Figure t d c) = Figure t d (Coord (x c + blockSize) (y c) (clr c))


-- | just moves given figure right for one blocksize
moveFigureLeft :: Figure -> Figure
moveFigureLeft (Figure t d c) = Figure t d (Coord (x c - blockSize) (y c) (clr c))