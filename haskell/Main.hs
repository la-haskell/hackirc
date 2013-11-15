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

import Network.Socket
import Control.Concurrent
import Control.Monad
import Data.IORef

import qualified Data.Map as M

data ServerState = ServerState
  { users :: M.Map String Socket
  , rooms :: M.Map String [String]
  }

defaultState :: ServerState
defaultState = ServerState M.empty M.empty

main :: IO ()
main = do
  state <- newIORef defaultState
  sock <- socket AF_INET Stream 0
  setSocketOption sock ReuseAddr 1
  bindSocket sock (SockAddrInet 1801 iNADDR_ANY)
  listen sock 1000
  forever $ do
    conn <- accept sock
    forkIO $ forever $ do
      cmd <- recv sock 1024
      resp <- handle state cmd
      send sock resp

handle :: IORef ServerState -> String -> IO String
handle _ = return

