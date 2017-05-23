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
import System.Environment (getArgs)

import Tetris


-- | multiplayer gamestate data type to keep two normal gamestates 
-- and a connecton to the server
data MP_Gamestate = MP_Gamestate 
  {
    opponentState :: TVar GameState
  , myState       :: TVar GameState
  , connection    :: Connection
  }



-- | function that handles updates coming from the server
-- and writes them to the current gamestate
handleUpdatesMP :: MP_Gamestate -> IO ()
handleUpdatesMP MP_Gamestate{..} = forever $ do
  pair <- receiveData connection

  myGS <- return (getFst pair)
  opGS <- return (getSnd pair)

  atomically $ do
     writeTVar opponentState (fromWeb opGS)
     writeTVar myState       (fromWeb myGS)


-- | get first element of GSPair 
getFst :: GSPair -> WebGS
getFst (GSPair f s) = f


-- | get second element of GSPair 
getSnd :: GSPair -> WebGS
getSnd (GSPair f s) = s


-- | function that handles updates (from keyboard)
-- and sends them to the server
handleTetrisMP :: Event -> MP_Gamestate -> IO MP_Gamestate

handleTetrisMP (EventKey (Char p) Down _ _) gs = sendIvent (Text.singleton p) gs
handleTetrisMP (EventKey (Char p) Up _ _) gs = return gs

handleTetrisMP(EventKey (SpecialKey KeySpace) Down _ _ ) gs = sendIvent (Text.singleton ' ') gs
handleTetrisMP(EventKey (SpecialKey KeySpace) Up _ _ ) gs = return gs

handleTetrisMP  _ gs = return gs  


-- | sends given event (acton) to the server
sendIvent::Text.Text -> MP_Gamestate -> IO MP_Gamestate
sendIvent txt gs@MP_Gamestate{..}  = do
  forkIO $ sendBinaryData connection txt
  return gs


-- | converts MP_Gamestate to the picture by calling "drawing" functions
renderTetris :: MP_Gamestate -> IO Picture
renderTetris MP_Gamestate{..} = do

 gs1 <- readTVarIO myState
 gs2 <- readTVarIO opponentState

 io1  <- return (drawTetris (div screenWidth 2) gs1)
 io2  <- return (drawTetris ( - div screenWidth 2) gs2)
 return (pictures [io1, io2])


-- | does nothing, only needed as a argument for a "play" function
updateTetrisMP :: Float -> MP_Gamestate -> IO MP_Gamestate
updateTetrisMP dt gs = do
  return gs


-- | Main tetris-client function
main :: IO ()
main = do
 g <- newStdGen
 state1 <- atomically $ newTVar (genEmptyUniverse g)
 state2 <- atomically $ newTVar (genEmptyUniverse g)
 args <- getArgs
 runClient (head args) (read (head $ tail args) :: Int) "/connect" $ \conn -> do
    let gs = MP_Gamestate state1 state2 conn
    _ <- forkIO (handleUpdatesMP gs)
    playIO display bgColor fps gs renderTetris handleTetrisMP updateTetrisMP
    return ()
  where
    display  = InWindow "Tetris" (screenWidth * 2, screenHeight) (200, 200)
    bgColor  = black      -- цвет фона
    fps      = glob_fps   -- кол-во кадров в секунду
