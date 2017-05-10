{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM
import Control.Exception (catch)
import Control.Monad (forever)
import Control.Monad.Random (evalRand, newStdGen)
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

-- | Server config.
data Config = Config
  { configUniverse  :: TVar GameState               -- ^ The current state of the universe.
  , configClients   :: TVar (Map PlayerName Client) -- ^ All connected clients by a unique name.
  , configNames     :: TVar [PlayerName]            -- ^ Source of new names.
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
      conn <- acceptRequest pending_conn
      name <- addClient conn config
      putStrLn $ show name
      handleActions name conn config

    -- this application will be used for non-websocket requests
    backupApp _ respond = respond $ responseLBS status400 [] "Not a WebSocket request"



addClient :: Client -> Config -> IO PlayerName
addClient client config@Config{..} = do
  g <- newStdGen
  forkIO $ periodicUpdates 10000 config
  atomically $ do
    name:names <- readTVar configNames
    writeTVar configNames names
    modifyTVar configClients (Map.insert name client)
    return name


handleActions :: PlayerName -> Connection -> Config -> IO ()
handleActions name conn cfg@Config{..} = forever $ do
  action <- receiveData conn
  putStrLn ("recieved!!" ++ show action)
  atomically $ do
    modifyTVar configUniverse (handlePlayerAction action name)


handlePlayerAction :: Action -> PlayerName -> GameState -> GameState
handlePlayerAction act name gs@GameState{..}
  | Text.head act == 'l' = toGS $ moveRight $ fromGS gs 
  | Text.head act == 'j' = toGS $ moveLeft $ fromGS gs 
  | Text.head act == 'k' = toGS $ turn $ fromGS gs 
  | Text.head act == ' ' = toGS $ dropit pts $ fromGS gs 
  | Text.head act == 'p' = toGS $ pause $ fromGS gs 
    where
      pts = getSndCoord $ fromGS gs
  -- return gs


getSndCoord :: Gamestate -> Int
getSndCoord (a,(Figure sha dir (b,c,z):rest),d,e) = screenHeight - c



periodicUpdates :: Int -> Config -> IO ()
periodicUpdates ms cfg@Config{..} = forever $ do
  threadDelay ms -- wait ms milliseconds
  universe <- atomically $ do
    res <- (updateTetris secs . fromGS) <$> readTVar configUniverse
    writeTVar configUniverse $ toGS res
    return (toGS res)
  return ()
  -- putStrLn "here!"
  broadcastUpdate universe cfg
  where
    secs = fromIntegral ms / 1000000



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
