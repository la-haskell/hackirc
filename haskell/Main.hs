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

main :: IO ()
main = do
  sock <- socket AF_INET Stream 0
  setSocketOption sock ReuseAddr 1
  bindSocket sock (SockAddrInet 1801 iNADDR_ANY)
  listen sock 1000
  mainLoop sock

mainLoop :: Socket -> IO ()
mainLoop sock = do
  conn <- accept sock
  forkIO (runConn conn)
  mainLoop sock

handle :: [String] -> [String]
handle = id

runConn :: (Socket, SockAddr) -> IO ()
runConn (sock, _) = forever $ do
  cmd <- recv sock 1024
  send sock $ unwords . handle . words $ cmd
