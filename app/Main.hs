{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import Control.Concurrent (forkFinally)
import Control.Monad (forever, unless)
import Data.ByteString qualified as BS
import Data.Text.Encoding as T
import Data.Text as T
--import Fmt

data Client = Client
  { clientSock :: Socket
  , clientAddr :: SockAddr
  }

main :: IO ()
main =
    acceptConnections 4000

acceptConnections :: PortNumber -> IO ()
acceptConnections port = do
    let hostAddr = tupleToHostAddress(0,0,0,0)
    sock <- socket AF_INET Stream 0
    setSocketOption sock ReuseAddr 1
    bind sock (SockAddrInet port hostAddr)
    listen sock 10
    putStrLn $ "WalkTower listening on port " ++ show port
    forever $ do
        (conn, peer) <- accept sock
        putStrLn $ "Connection from " ++ show peer
        let c = Client conn peer
        forkFinally (handleClient c) (\_ -> close conn)

handleClient :: Client -> IO ()
handleClient client@Client{..} = do
    bytes <- recv clientSock 1024
    unless (BS.null bytes) $ do
        --sendAll sock msg
        parseCommand client ((T.unpack . T.strip . T.decodeUtf8)  bytes)
        handleClient client


parseCommand :: Client -> String -> IO ()
parseCommand _client msg = do
    --putStrLn $ fmt $ "Received: "+|msg|+""
    putStrLn $ "Testing: {}" ++ msg










