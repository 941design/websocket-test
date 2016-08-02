module Main where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM.TVar (TVar, newTVarIO, readTVar, swapTVar)
import Control.Monad.STM (atomically)
import qualified Data.ByteString.Lazy.Char8 as C8
import Network.WebSockets
import System.IO (stdin, hGetLine)


app :: TVar [Connection] -> PendingConnection -> IO ()
app state pending = do

  -- Starting ws conversation:
  conn <- acceptRequest pending

  -- Adding connection to global state:
  atomically $ do
    connections <- readTVar $ state
    swapTVar state (conn:connections)

  -- Doing stuff until client closes connection:
  withDataMessages conn $ \msg -> do
    putStrLn . show $ msg
    send conn . DataMessage . Text $ C8.pack $ "received: " ++ show msg

  -- TODO - Connections are not of typeclass Eq, therefore either
  -- store connections by a unique key for later removal, or 
  -- handle already closed connections gracefully.
  
  return ()


withDataMessages :: Connection -> (DataMessage -> IO ()) -> IO ()
withDataMessages conn f = do
  msg <- receive conn
  case msg of
    DataMessage m -> do
      f m
      withDataMessages conn f
    ControlMessage m -> return ()


-- Blocking!
withStdin :: (String -> IO ()) -> IO ()
withStdin f = hGetLine stdin >>= f >> withStdin f


broadcast :: Show a => TVar [Connection] -> a -> IO ()
broadcast state s = do
  -- TODO - This currently includes closed connections:
  connections <- atomically . readTVar $ state
  mapM bc connections
  -- DEBUG - Display the amount of connected clients:
  -- putStrLn . show . length $ connections
  return ()
  where
    bc conn = send conn (DataMessage (Text $ C8.pack . show $ s))


-- Blocking!
everySecond :: (() -> IO ()) -> IO ()
everySecond f = do
  threadDelay 1000000
  f ()
  everySecond f


-- TODO - The haskell way would rather be to capture state in a monad
-- and elevate the entire application into that stateful monad!
main :: IO ()
main = do

  -- Global state to manage connections:
  state <- newTVarIO []
  
  -- Manipulating global state while listening to stdin:
  forkIO . withStdin $ (broadcast state)

  -- Manipulating global state by timed event:
  forkIO . everySecond $ (\() -> (broadcast state "foobar"))

  -- Referencing global state while managing connections:
  runServer "0.0.0.0" 8080 (app state)
