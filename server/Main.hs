{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Control.Concurrent (forkIO, threadDelay, killThread ,ThreadId)
import Control.Concurrent.STM
import Control.Exception (throw, catch, toException)
import Control.Monad (forever)
import Control.Monad.Random (evalRand, newStdGen)
import System.Exit
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Text.Lazy as Text

import Network.HTTP.Types (status400)
import Network.Wai (responseLBS)
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.WebSockets
import Servant

import Control.Concurrent (forkIO)
import Network.Wai.Handler.Warp (run)

-- import Debug.Hood.Observe

import Tetris


data State = Active | Closed deriving(Eq, Show)

-- | Server config.
data Config = Config
  { configUniverse  :: TVar GameState               -- ^ The current state of the universe.
  , configClients   :: TVar (Map PlayerName Client) -- ^ All connected clients by a unique name.
  , configNames     :: TVar [PlayerName]            -- ^ Source of new names.
  , controlThreads  :: TVar (Map PlayerName ThreadId)
  , configState     :: TVar State
  }

-- | A client is represented by its websocket 'Connection'.
type Client = Connection

-- | Default server config with empty universe and no clients.
mkDefaultConfig :: IO Config
mkDefaultConfig = do
  g <- newStdGen
  cfg <- atomically $ Config
          <$> newTVar (toGS $ genUniverse g)
          <*> newTVar Map.empty
          <*> newTVar (map show [1..])
          <*> newTVar Map.empty
          <*> newTVar Active
  return cfg

-- | An API for the Game of Snakes server.
type API = "connect" :> Raw

type Action = Text.Text



main :: IO ()
main = do
  config <- mkDefaultConfig
  -- forkIO $ periodicUpdates 10000 config
  run 8000 $ server config
  


-- | The Game of Snakes server 'Application'.
server :: Config -> Server API
server config@Config{..} = websocketsOr defaultConnectionOptions wsApp backupApp
  where
    wsApp :: ServerApp
    wsApp pending_conn = do
      cl <- readTVarIO configClients
      t <- return (length $ Map.toList cl)
      -- _ <- updadeState config

      conn <- acceptRequest pending_conn
      name <- addClient conn config
      putStrLn $ show name

      if t < 1 then
        handleActions name conn config
      else
        putStrLn "too many players!"

    -- this application will be used for non-websocket requests
    backupApp _ respond = respond $ responseLBS status400 [] "Not a WebSocket request"


-- updadeState :: Config -> IO ()
-- updadeState config@Config{..} = do
--   atomically $ do
--     st <- readTVar configState
--     state <- return (nextState st)
--     writeTVar configState state

-- nextState :: State -> State
-- nextState st 
--   | st == Closed = One
--   | st == One    = Active
--   | st == Active = throw $ toException "string"::String


addClient :: Client -> Config -> IO PlayerName
addClient client config@Config{..} = do
  g <- newStdGen
  length <- getLength config
  forkId <- if (length == 0) then
              forkIO $ periodicUpdates 10000 config
            else 
              forkIO $ periodicUpdates 10000 config
  atomically $ do
    name:names <- readTVar configNames
    writeTVar configNames names
    modifyTVar configClients (Map.insert name client)
    modifyTVar controlThreads (Map.insert name forkId)
    return name

getLength :: Config -> IO Int
getLength Config{..} = do
  cl <- readTVarIO configClients
  return (length $ Map.toList cl)

handleActions :: PlayerName -> Connection -> Config -> IO ()
handleActions name conn cfg@Config{..} = forever $ do
  action <- receiveData conn
  -- putStrLn ("recieved!!" ++ show action)
  atomically $ do
    modifyTVar configUniverse (handlePlayerAction action name)


handlePlayerAction :: Action -> PlayerName -> GameState -> GameState
handlePlayerAction act name gs@GameState{..}
  | Text.head act == 'l' = toGS $ moveRight $ fromGS gs 
  | Text.head act == 'j' = toGS $ moveLeft $ fromGS gs 
  | Text.head act == 'k' = toGS $ turn $ fromGS gs 
  | Text.head act == ' ' = toGS $ dropit pts $ fromGS gs 
  | Text.head act == 'p' = toGS $ pause $ fromGS gs 
  | otherwise = gs
    where
      pts = getSndCoord $ fromGS gs
  -- return gs


getSndCoord :: Gamestate -> Int
getSndCoord (a,(Figure sha dir (b,c,z):rest),d,e) = screenHeight - c



periodicUpdates :: Int -> Config -> IO ()
periodicUpdates ms cfg@Config{..} = forever $ do
  threadDelay ms -- wait ms milliseconds
  stateIO <- readTVarIO configState
  disconnectState stateIO
  universe <- atomically $ do
    state <- readTVar configState
    res <- (updateTetris secs . fromGS) <$> readTVar configUniverse
    writeTVar configUniverse $ toGS res
    return (toGS res)
  return ()
  -- putStrLn "here!"
  broadcastUpdate universe cfg
  where
    secs = fromIntegral ms / 1000000



disconnectState :: State -> IO ()
disconnectState Closed = do 
  -- putStrLn "good"
  exitSuccess
disconnectState _ = do return ()



broadcastUpdate :: GameState -> Config -> IO ()
broadcastUpdate gs cfg@Config{..} = do
  clients <- readTVarIO configClients
  txt <- do return (Text.singleton 'f')
  mapM_ (forkIO . sendUpdate) (Map.toList clients)
  where
    sendUpdate (name, conn) = sendBinaryData conn (toWeb gs) `catch` handleClosedConnection name

    handleClosedConnection :: PlayerName -> ConnectionException -> IO ()
    handleClosedConnection name _ = do
      putStrLn (name ++ " disconnected.")
      atomically $ do
        modifyTVar configClients  (Map.delete name)
        cthreads <- readTVar controlThreads
        return (killCtrlThread name cthreads)
        -- prt <- show cthreads
        -- return (putStrLn $ show cthreads)
        modifyTVar controlThreads (Map.delete name) 
        writeTVar configState Closed     
      state <- readTVarIO configState
      putStrLn (show state)  



killCtrlThread :: PlayerName -> (Map.Map PlayerName ThreadId) -> IO ()
killCtrlThread name map = do
  -- putStrLn "here"
  return (fmap killThread (Map.lookup name map))
  (putStrLn . show) (Map.lookup name map)