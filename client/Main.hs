{-# LANGUAGE RecordWildCards #-}

module Main where

import qualified Data.Text.Lazy as Text

import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import Control.Monad (forever)

import System.Random

import Graphics.Gloss.Interface.IO.Game
import Network.WebSockets
import System.Exit (exitSuccess)

import Tetris

data MP_Gamestate = MP_Gamestate 
  { state      :: TVar Gamestate
  , connection :: Connection
  }


handleUpdatesMP :: MP_Gamestate -> IO ()
handleUpdatesMP MP_Gamestate{..} = forever $ do
  putStrLn "here!"
  gs <- receiveData connection
  putStrLn "there!"
  atomically $ writeTVar state (fromWebGS gs)


handleTetrisMP :: Event -> MP_Gamestate -> IO MP_Gamestate

handleTetrisMP (EventKey (Char p) Down _ _) gs = sendIvent (Text.singleton p) gs
handleTetrisMP (EventKey (Char p) Up _ _) gs = return gs

handleTetrisMP(EventKey (SpecialKey KeySpace) Down _ _ ) gs = sendIvent (Text.singleton ' ') gs
handleTetrisMP(EventKey (SpecialKey KeySpace) Up _ _ ) gs = return gs

handleTetrisMP  _ gs = return gs  


sendIvent::Text.Text -> MP_Gamestate -> IO MP_Gamestate
sendIvent txt gs@MP_Gamestate{..}  = do
	forkIO $ sendBinaryData connection txt
	return gs


renderTetris :: MP_Gamestate -> IO Picture
renderTetris MP_Gamestate{..} = do
 gs <- readTVarIO state
 io <- return (drawTetris gs)
 return io


-- does nothing
updateTetrisMP :: Float -> MP_Gamestate -> IO MP_Gamestate
updateTetrisMP dt gs = do
  -- sendIvent (Text.singleton 'd') gs
  return gs


main :: IO ()
main = do
 g <- newStdGen
 state <- atomically $ newTVar (genUniverse g)
 runClient "localhost" 8000 "/connect" $ \conn -> do
    let gs = MP_Gamestate state conn
    _ <- forkIO (handleUpdatesMP gs)
    playIO display bgColor fps gs renderTetris handleTetrisMP updateTetrisMP
  where
    display  = InWindow "Tetris" (screenWidth, screenHeight) (200, 200)
    bgColor  = black      -- цвет фона
    fps      = glob_fps   -- кол-во кадров в секунду
