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
import System.Environment (getArgs)


import Tetris


-- | data type for current server state 
data State = Active | Waiting | Closed deriving(Eq, Show)


-- | Server configuration
data Config = Config
  { configUniverse  :: TVar (Map PlayerName GameState)               -- ^ The current state of the universe.
  , configClients   :: TVar (Map PlayerName Client) -- ^ All connected clients by a unique name.
  , configNames     :: TVar [PlayerName]            -- ^ Source of new names.
  , controlThreads  :: TVar (Map PlayerName ThreadId)
  , configState     :: TVar State
  }


-- | A client is represented by its websocket 'Connection'.
type Client = Connection


-- | Creates default server config with empty universe and no clients.
mkDefaultConfig :: IO Config
mkDefaultConfig = do
  g <- newStdGen
  cfg <- atomically $ Config
          <$> newTVar Map.empty
          <*> newTVar Map.empty
          <*> newTVar (map show [1..])
          <*> newTVar Map.empty
          <*> newTVar Active
  return cfg


-- | An API for the Game of Snakes server.
type API = "connect" :> Raw


-- | Action that would be recieved from client through socket
type Action = Text.Text


-- | Server main function
main :: IO ()
main = do
  config <- mkDefaultConfig
  args <- getArgs
  run (read $ head $ args :: Int) $ server config
  


-- | The Game of Snakes server 'Application'.
server :: Config -> Server API
server config@Config{..} = websocketsOr defaultConnectionOptions wsApp backupApp
  where
    wsApp :: ServerApp
    wsApp pending_conn = do
      cl <- readTVarIO configClients
      t <- return (length $ Map.toList cl)

      conn <- acceptRequest pending_conn
      name <- addClient conn config
      putStrLn $ show name

      if t < 2 then 
        do
          if t < 1 then 
            atomically $ do writeTVar configState Waiting
          else 
            atomically $ do writeTVar configState Active
          handleActions name conn config
      else
        putStrLn "too many players!"

    -- this application will be used for non-websocket requests
    backupApp _ respond = respond $ responseLBS status400 [] "Not a WebSocket request"


-- | function to accept a new client and to add him 
-- to the list of players
addClient :: Client -> Config -> IO PlayerName
addClient client config@Config{..} = do
  g <- newStdGen
  length <- getLength config
  forkId <- if (length == 0) then
              forkIO $ periodicUpdates 10000 config
            else 
              forkIO $ pass
  atomically $ do
    name:names <- readTVar configNames
    writeTVar configNames names
    modifyTVar configClients (Map.insert name client)
    modifyTVar controlThreads (Map.insert name forkId)
    modifyTVar configUniverse (Map.insert name $ genUniverse g)
    return name


-- | just a fucntion that does nothing :)
pass :: IO ()
pass = return ()


-- | return the quantity of clients connected to server
getLength :: Config -> IO Int
getLength Config{..} = do
  cl <- readTVarIO configClients
  return (length $ Map.toList cl)


-- | recieve the players action though the web
-- and updates gamestate
handleActions :: PlayerName -> Connection -> Config -> IO ()
handleActions name conn cfg@Config{..} = forever $ do
  action <- receiveData conn
  atomically $ do
    universe <- readTVar configUniverse
    gamestate <- return (universe Map.! name)
    modifyTVar configUniverse (Map.adjust (handlePlayerAction action name) name)


-- | updates gamestate according to players action
handlePlayerAction :: Action -> PlayerName -> GameState -> GameState
handlePlayerAction act name gs@GameState{..}
  | Text.head act == 'l' = moveRight gs 
  | Text.head act == 'j' = moveLeft gs 
  | Text.head act == 'k' = turn gs 
  | Text.head act == ' ' = dropit pts gs 
  | Text.head act == 'p' = pause gs 
  | otherwise = gs
    where
      pts = (screenHeight - (y $ coord $ head figures))


-- | periodically updates gamestate to keep the game moving
periodicUpdates :: Int -> Config -> IO ()
periodicUpdates ms cfg@Config{..} = forever $ do
  threadDelay ms -- wait ms milliseconds
  stateIO <- readTVarIO configState
  disconnectState stateIO
  if stateIO == Waiting then return ()
    else do 
      universe <- atomically $ do
        res <- ((map (updateT secs)) . Map.toList) <$> readTVar configUniverse
        writeTVar configUniverse $ Map.fromList res
        return (Map.fromList res)
      broadcastUpdate universe cfg
  where
    secs = fromIntegral ms / 1000000


-- | mask for updateTetris function 
updateT :: Float -> (PlayerName, GameState) -> (PlayerName, GameState)
updateT secs (n, gs) = (n, updateTetris secs gs)


-- | stops the program in case the commection to the client is closed
disconnectState :: State -> IO ()
disconnectState Closed = do 
  exitSuccess
disconnectState _ = do return ()


-- | sends the updates gamestate to all the clients
broadcastUpdate :: (Map PlayerName GameState) -> Config -> IO ()
broadcastUpdate un cfg@Config{..} = do
  clients <- readTVarIO configClients
  txt <- do return (Text.singleton 'f')
  mapM_ (forkIO . sendUpdate) (Map.toList clients)
    where
      sendUpdate (name, conn) = do
            
            fst <- return (un Map.! name) 
            snd <- return (head $ Map.elems (Map.delete name un)) 

            -- if fst == snd then putStrLn "BAD!!!"
            -- else putStrLn "OK"

            (sendData cfg conn name) $ GSPair (toWeb fst) (toWeb snd)
            return ()

-- | function that sends provided data to the client
sendData :: Config -> Client -> PlayerName -> GSPair -> IO ()
sendData cfg@Config{..} conn name gs = sendBinaryData conn gs `catch` handleClosedConnection name
  where
    -- ^ handler for a closed connection
    handleClosedConnection :: PlayerName -> ConnectionException -> IO ()
    handleClosedConnection name _ = do
      putStrLn (name ++ " disconnected.")
      atomically $ do
        modifyTVar configClients  (Map.delete name)
        cthreads <- readTVar controlThreads
        return (killCtrlThread name cthreads)
        modifyTVar controlThreads (Map.delete name) 
        writeTVar configState Closed     
      state <- readTVarIO configState
      putStrLn (show state)  


-- | function to kill update-handler for a client when he disconnects
killCtrlThread :: PlayerName -> (Map.Map PlayerName ThreadId) -> IO ()
killCtrlThread name map = do
  return (fmap killThread (Map.lookup name map))
  (putStrLn . show) (Map.lookup name map)
