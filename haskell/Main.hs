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

module Main (
    main
) where

import Data.List
import Control.Monad.State
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

defaultState :: ServerState
defaultState = ServerState M.empty M.empty

emptyUserState :: User -> UserLocalState
emptyUserState = flip UserLocalState []

handle :: IORef ServerState -> [String] -> StateT UserLocalState IO String
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
  bindSocket sock (SockAddrInet 4242 iNADDR_ANY)
  listen sock 2
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
    (resp, newUserState) <- flip runStateT userState $ handle serverState $ words cmd
    send sock resp
    go newUserState

login :: IORef ServerState -> User -> Socket -> IO (Maybe String)
login serverState user sock = do
  atomicModifyIORef' serverState $ \st ->
    case M.lookup user (users st) of
      Nothing -> let newState = st { users = M.insert user sock (users st) }
                 in (newState, Just "OK")
      _ -> (st, Nothing)

logout :: IORef ServerState -> StateT UserLocalState IO String
logout serverState = do
    userState <- get
    let username = myUserName userState
    lift $ atomicModifyIORef' serverState $ \st -> (st { users = M.delete username (users st) }, "OK")


joinRoom :: IORef ServerState -> Room -> StateT UserLocalState IO String
joinRoom serverState room = do
  userState <- get
  let username = myUserName userState
  lift $ atomicModifyIORef' serverState $ \st -> (st { rooms = M.alter (addUser username) room (rooms st) }, ())
  modify $ \st -> st { myRooms = room : myRooms st }
  return "OK"
    where
    addUser username Nothing = Just [username]
    addUser username (Just users) = Just $ username:users


leave :: IORef ServerState -> Room -> StateT UserLocalState IO String
leave serverState room = do
  userState <- get
  let username = myUserName userState
  lift $ atomicModifyIORef' serverState $ \st -> (st { rooms = M.alter (removeUser username) room (rooms st) }, ())
  modify $ \st -> st { myRooms = delete room $ myRooms st }
  return "OK"
    where
    removeUser username Nothing = Just []
    removeUser username (Just users) = Just $ delete username users

say :: IORef ServerState -> Room -> Msg -> StateT UserLocalState IO String
say = undefined

whisper :: IORef ServerState -> Room -> Msg -> StateT UserLocalState IO String
whisper = undefined
