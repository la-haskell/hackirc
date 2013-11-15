-----------------------------------------------------------------------------
--
-- Module      :  Main
-- Copyright   :
-- License     :  MIT
--
-- Maintainer  :  lahaskell@elem.com
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main (
    main
) where

import Data.List
import Control.Applicative
import Data.Maybe
import Control.Monad.State
import Control.Monad.Error
import Control.Monad.Trans
import Network.Socket
import Control.Concurrent
import Control.Monad
import Data.IORef

import qualified Data.Map as M

type Room = String

type User = String

type Msg = String

data ServerState = ServerState
  { users :: M.Map User Socket
  , rooms :: M.Map Room [User]
  } deriving (Show)

data UserLocalState = UserLocalState
  { myUserName :: User
  , myRooms :: [Room]
  } deriving (Show)

newtype IRC a = IRC { irc :: StateT UserLocalState (ErrorT String IO) a }
  deriving (Functor, Monad, MonadIO, MonadState UserLocalState, MonadError String)

runIRC :: UserLocalState -> IRC a -> IO (Either String (a, UserLocalState))
runIRC st = runErrorT . flip runStateT st . irc

defaultState :: ServerState
defaultState = ServerState M.empty M.empty

emptyUserState :: User -> UserLocalState
emptyUserState = flip UserLocalState []

handle :: IORef ServerState -> [String] -> IRC String
handle state ["LOGOUT"] = logout state
handle state ["JOIN", room] = joinRoom state room
handle state ["LEAVE", room] = leave state room
handle state ("SAY" : room : msg) = say state room (unwords msg)
handle state ("WHISPER" : user : msg) = whisper state user (unwords msg)
handle _ _ = return ("ERROR Unknown command")

main :: IO ()
main = do
  state <- newIORef defaultState
  sock <- socket AF_INET Stream 0
  setSocketOption sock ReuseAddr 1
  bindSocket sock (SockAddrInet 1801 iNADDR_ANY)
  listen sock 1000
  forever $ do
    (sock, _) <- accept sock
    forkIO $ userLoop state sock

userLoop :: IORef ServerState -> Socket -> IO ()
userLoop serverState sock = do
  loginCmd <- recv sock 1024
  putStrLn $ "Message received " ++ loginCmd
  case words loginCmd of
    ["LOGIN", username] -> do
      resp <- login serverState username sock
      case resp of
        Nothing -> do
          send sock "ERROR Username is taken"
          sClose sock
        Just msg -> do
          putStrLn $ "User " ++ username ++ " logged in"
          send sock msg
          go (emptyUserState username)
    _ -> do
      send sock "ERROR Inappropriate command"
      sClose sock
  where
  go :: UserLocalState -> IO ()
  go userState = do
    cmd <- recv sock 1024
    response <- runIRC userState $ handle serverState $ words cmd
    case response of
      Left err -> do
        send sock $ unwords ["ERROR", err]
        return ()
      Right (resp, newUserState) -> do
        send sock resp
        go newUserState

login :: IORef ServerState -> User -> Socket -> IO (Maybe String)
login serverState user sock = do
  atomicModifyIORef' serverState $ \st ->
    case M.lookup user (users st) of
      Nothing -> let newState = st { users = M.insert user sock (users st) }
                 in (newState, Just "OK")
      _ -> (st, Nothing)

logout :: IORef ServerState -> IRC String
logout serverState = do
    userState <- get
    let username = myUserName userState
    liftIO $ atomicModifyIORef' serverState $ \st -> (st { users = M.delete username (users st) }, "OK")

joinRoom :: IORef ServerState -> Room -> IRC String
joinRoom serverState room = do
  userState <- get
  let username = myUserName userState
  liftIO $ atomicModifyIORef' serverState $ \st -> (st { rooms = M.alter (addUser username) room (rooms st) }, ())
  modify $ \st -> st { myRooms = room : myRooms st }
  broadcast serverState room $ unwords ["JOIN", room, username]
  return "OK"
    where
    addUser username Nothing = Just [username]
    addUser username (Just users) = Just $ username:users

leave :: IORef ServerState -> Room -> IRC String
leave serverState room = do
  userState <- get
  when (not (room `elem` myRooms userState)) $ throwError $ "Not in room " ++ room
  let username = myUserName userState
  liftIO $ atomicModifyIORef' serverState $ \st -> (st { rooms = M.alter (removeUser username) room (rooms st) }, ())
  modify $ \st -> st { myRooms = delete room $ myRooms st }
  broadcast serverState room $ unwords ["LEAVE", room, username]
  return "OK"
    where
    removeUser username Nothing = Just []
    removeUser username (Just users) = Just $ delete username users

say :: IORef ServerState -> Room -> Msg -> IRC String
say serverState room message = do
  userState <- get
  when (not (room `elem` myRooms userState)) $ throwError $ "Not in room " ++ room
  let username = myUserName userState
  broadcast serverState room $ unwords ["MESSAGE", username, room, message]
  return "OK"

whisper :: IORef ServerState -> User -> Msg -> IRC String
whisper serverState user message = do
  userState <- get
  let username = myUserName userState
  st <- liftIO $ readIORef serverState
  case M.lookup user $ users st of
    Nothing -> throwError $ "User " ++ user ++ " not logged in."
    Just sock -> do
      liftIO $ send sock $ unwords ["WHISPER", username, message]
      return "OK"

broadcast :: IORef ServerState -> Room -> Msg -> IRC ()
broadcast serverState room message = do
  st <- liftIO $ readIORef serverState
  let usersInRoom = fromMaybe [] $ M.lookup room (rooms st)
  liftIO $ forM_ usersInRoom $ \user -> do
    case M.lookup user $ users st of
      Nothing -> putStrLn $ "User " ++ user ++ " not found."
      Just sock -> do
        send sock message
        return ()





