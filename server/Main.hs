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
  { configUniverse  :: TVar WebGS               -- ^ The current state of the universe.
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
          <$> newTVar (toWebGS $ genUniverse g)
          <*> newTVar Map.empty
          <*> newTVar (map show [1..])
  return cfg

-- | An API for the Game of Snakes server.
type API = "connect" :> Raw

type Action = Text.Text



main :: IO ()
main = do
  config <- mkDefaultConfig
  -- forkIO $ periodicUpdates 10000 config   -- update Universe every 10 milliseconds
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
      if length cl > 0 
      then 
        do return ()
      else 
        -- putStrLn $ name ++ " joined!"
        handleActions name conn config

        -- conn <- acceptRequest pending_conn
        -- name <- addClient conn config
        -- putStrLn $ name ++ " joined!"
        -- handleActions name conn config

    -- this application will be used for non-websocket requests
    backupApp _ respond = respond $ responseLBS status400 [] "Not a WebSocket request"

-- | Add a new client to the server state.
-- This will update 'configClients' and add
-- a new player to the 'configUniverse'.
addClient :: Client -> Config -> IO PlayerName
addClient client Config{..} = do
  g <- newStdGen
  atomically $ do
    name:names <- readTVar configNames
    writeTVar configNames names
    modifyTVar configClients (Map.insert name client)
    -- modifyTVar configUniverse (flip evalRand g . spawnPlayer name)
    return name

-- | An infinite loop, receiving data from the 'Client'
-- and handling its actions via 'handlePlayerAction'.
handleActions :: PlayerName -> Connection -> Config -> IO ()
handleActions name conn cfg@Config{..} = forever $ do
  action <- receiveData conn
  putStrLn ("recieved!!" ++ show action)
  atomically $ do
    modifyTVar configUniverse (handlePlayerAction action name)


handlePlayerAction :: Action -> PlayerName -> WebGS -> WebGS
handlePlayerAction act name gs@WebGS{..}
  | Text.head act == 'l' = toWebGS $ moveRight $ fromWebGS gs 
  | Text.head act == 'j' = toWebGS $ moveLeft $ fromWebGS gs 
  | Text.head act == 'k' = toWebGS $ turn $ fromWebGS gs 
  | Text.head act == ' ' = toWebGS $ dropit pts $ fromWebGS gs 
  | Text.head act == 'p' = toWebGS $ pause $ fromWebGS gs 
    where
      pts = getSndCoord $ fromWebGS gs
  -- return gs


getSndCoord :: Gamestate -> Int
getSndCoord (a,(Figure sha dir (b,c,z):rest),d,e) = screenHeight - c



periodicUpdates :: Int -> Config -> IO ()
periodicUpdates ms cfg@Config{..} = forever $ do
  threadDelay ms -- wait ms milliseconds
  -- g <- newStdGen
  universe <- atomically $ do
    res <- (updateTetris secs . fromWebGS) <$> readTVar configUniverse
    writeTVar configUniverse $ toWebGS res
    return (toWebGS res)
  broadcastUpdate universe cfg
  where
    -- FIXME: (ms / 10^6) is not the actual time that has passed since the previous update
    -- we should use getCurrentTime to more accurately keep track of time deltas
    secs = fromIntegral ms / 1000000

-- | Send every 'Client' updated 'Universe' concurrently.


broadcastUpdate :: WebGS -> Config -> IO ()
broadcastUpdate gs cfg@Config{..} = do
  clients <- readTVarIO configClients
  mapM_ (forkIO . sendUpdate) (Map.toList clients)
  where
    sendUpdate (name, conn) = sendBinaryData conn gs `catch` handleClosedConnection name

    handleClosedConnection :: PlayerName -> ConnectionException -> IO ()
    handleClosedConnection name _ = do
      putStrLn (name ++ " disconnected.")
      atomically $ do
        modifyTVar configClients  (Map.delete name)
        -- modifyTVar configUniverse (kickPlayer name)