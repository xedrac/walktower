{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import Control.Concurrent (forkFinally)
import Control.Monad (forever, unless)
import Data.ByteString qualified as BS
import Data.ByteString.UTF8 qualified as UTF8
import Data.Text qualified as T
import WalkTower.Commands



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
        forkFinally (recvMsg c) (\_ -> do
            putStrLn $ "Client disconnected: " ++ show peer
            close conn)

recvMsg :: Client -> IO ()
recvMsg client@Client{..} = do
    bytes <- recv clientSock 1024
    unless (BS.null bytes) $ do
        let str = UTF8.toString bytes :: String
        let msg = strip str
        putStrLn $ "recv " ++ show clientAddr ++ ": '" ++ msg ++ "'"
        parseMsg client msg
    recvMsg client
    where
        strip  = T.unpack . T.strip . T.pack

disconnect :: Client -> IO ()
disconnect client@Client{..} = do
    sendMsg client "Disconnecting from server... Goodbye!"
    close clientSock

-- | Send message with newline
sendMsg :: Client -> String -> IO ()
sendMsg client msg = sendMsg' client (msg ++ "\n")

-- | Send message exactly as providied
sendMsg' :: Client -> String -> IO ()
sendMsg' Client{..} msg = do
    putStrLn $ "send " ++ show clientAddr ++ ": " ++ msg
    sendAll clientSock (UTF8.fromString msg)

sendHuh :: Client -> IO ()
sendHuh client = sendMsg client "Huh?"

sendHelp :: Client -> String -> IO ()
sendHelp client topic = sendMsg client ("Sending help about: " ++ show topic)

parseMsg :: Client -> String -> IO ()
parseMsg client msg = do
    case parseCommand msg of
        Just cmd -> runCommand client cmd
        Nothing -> sendHuh client


runCommand :: Client -> Command -> IO ()
runCommand client = \case
    CommandAttack xs  -> sendMsg client ("Attacking: " ++ show xs)
    CommandFlee       -> sendMsg client ("Attempting to flee!")
    CommandDefend     -> sendMsg client ("Defending")
    CommandUseItem xs -> sendMsg client ("Using items: " ++ show xs)
    CommandSteal xs   -> sendMsg client ("Attempting to steal: " ++ show xs)
    CommandQuit       -> disconnect client
    CommandHelp xs    -> if null xs then
                            sendMsg client "Help about what?"
                         else
                             mapM_ (sendHelp client) xs

