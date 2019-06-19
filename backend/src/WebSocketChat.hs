{-# LANGUAGE OverloadedStrings #-}
module WebSocketChat where

import Data.Map.Strict (Map)
import           Data.Aeson         (encode, decode)
import           Control.Exception  (finally)
import qualified Data.Map.Strict as Map
import qualified Data.ByteString    as B
import           Data.ByteString.Lazy (toStrict)
import           Data.Text          (Text)
import qualified Data.Text          as T
import qualified Data.Text.IO       as T
import Control.Concurrent
import Control.Monad
import TextShow


import qualified Network.WebSockets as WS


import Common.WsProtocol

type ClientState = WS.Connection
type ServerState = Map Nickname ClientState

newServerState :: ServerState
newServerState = mempty

numClients :: ServerState -> Int
numClients = length

initBackend :: IO (MVar ServerState)
initBackend = newMVar newServerState

broadcast :: Text -> ServerState -> IO ()
broadcast message clients = do
    T.putStrLn message
    -- forM_ clients $ \(_, conn) -> WS.sendTextData conn message
    forM_ clients $ \conn -> WS.sendTextData conn $
        (toStrict . encode . S2C_ServerMessage) message


addClient :: Nickname -> WS.Connection -> ServerState -> ServerState
addClient = Map.insert

removeClient :: Nickname -> ServerState -> ServerState
removeClient = Map.delete

application :: MVar ServerState -> WS.ServerApp
application state pending = do
    conn <- WS.acceptRequest pending
    WS.forkPingThread conn 30
    msgbs <- WS.receiveData conn :: IO B.ByteString
    let msgC = decode $ WS.toLazyByteString msgbs :: Maybe C2S
    clients <- readMVar state
    case msgC of
      Nothing ->
        T.putStrLn "Decoded msgC is nothing..."
      Just (C2S_Msg _) ->
        T.putStrLn "Msg should not happen here..."
      Just C2S_Close ->
        T.putStrLn "Close should not happen here..."
      Just (C2S_Join nick) -> flip finally (disconnect nick) $ do
        modifyMVar_ state $ \s -> do
            let s' = addClient nick conn s
            WS.sendTextData conn (toStrict $ encode S2C_Welcome)
            broadcast (showt nick <> " joined") s'
            return s'
        talk conn state nick

  where
    disconnect nick = do
      -- Remove client and return new state
      s <- modifyMVar state $ \s ->
          let s' = removeClient nick s in return (s', s')
      broadcast (showt nick <> " disconnected") s


talk :: WS.Connection -> MVar ServerState -> Nickname -> IO ()
talk conn state nick = forever $ do
    msgbs <- WS.receiveData conn :: IO B.ByteString
    case decode $ WS.toLazyByteString msgbs of
        Nothing           ->
            T.putStrLn "Decoded msgC is nothing..."
        Just C2S_Close     ->
            undefined -- TBD
        Just (C2S_Join nm) ->
            T.putStrLn $ "C2S_Join should not happen here, nm =" <> showt nick
        Just (C2S_Msg (nick, text)) ->
            T.putStrLn $ "C2S_msg: " <> showt nick <> ": "
            -- readMVar state >>= broadcast (user <> ": " <> txt)

