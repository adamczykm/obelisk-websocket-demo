{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Server where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Time.Clock.System
import Debug.Trace
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
import Control.Lens
import Control.Lens.TH
import TextShow
import Data.Maybe (fromMaybe, fromJust)


import qualified Network.WebSockets as WS


import Common.WsProtocol
import Common.Base58
import Common.Lang
import Common.Crypto
import Common.Skl

data Conn = Conn
  { _connTime :: SystemTime
  , _conn :: WS.Connection
  }

instance Eq Conn where
  c1 == c2 = _connTime c1 == _connTime c2

instance Ord Conn where
  c1 <= c2 = _connTime c1 <= _connTime c2

data ClientState = ClientState
  { _conns :: Set Conn
  , _skl :: Skl
  , _txx :: [Text] }

makeLenses ''ClientState

type ServerState = Map (Base58Rep Address) ClientState

newServerState :: ServerState
newServerState = mempty

numClients :: ServerState -> Int
numClients = length

initBackend :: IO (MVar ServerState)
initBackend = newMVar newServerState


addClient :: Conn -> Base58Rep Address -> ServerState -> IO (ServerState, ServerState)
addClient conn addr ss =
  let ret = case Map.lookup addr ss of
              Nothing ->
                Map.insert addr (ClientState (Set.singleton conn) (Skl 0) []) ss
              Just (ClientState conns skl txx) ->
                Map.insert addr (ClientState (Set.insert conn conns) skl txx) ss
  in pure (ret,ret)


sendResponse cns resp = forM_ cns $ \c -> WS.sendTextData (_conn c) resp

application :: MVar ServerState -> WS.ServerApp
application state pending = do
    c <- WS.acceptRequest pending
    systime <- getSystemTime
    let conn = Conn systime c
    WS.forkPingThread (_conn conn) 30
    let loop = do
          msgbs <- WS.receiveData (_conn conn) :: IO B.ByteString
          let msgC = decode $ WS.toLazyByteString msgbs :: Maybe C2S
          case msgC of
            Nothing ->
              T.putStrLn "Decoded msgC is nothing..."
            Just (C2S_PingForSKL (i, pkey, sign)) ->
              case verifySignature (fromBase58 pkey) (toStrict (encode i)) (fromBase58 sign) of
                False -> do
                  T.putStrLn ("Signature invalid." <> showt pkey)
                  WS.sendTextData (_conn conn)
                    (toStrict $ encode (S2C_ServerMessage "Invalid signature for skl request"))
                True -> do
                  -- add new connection
                  clnst <- modifyMVar state (addClient conn (adr pkey))
                  let
                    currSt = fromJust $ Map.lookup (adr pkey) clnst
                    currConns = (_conns currSt)
                  WS.sendTextData  (_conn conn) (toStrict $ encode (S2C_SKL (_skl currSt)))
                  WS.sendTextData (_conn conn) (toStrict $ encode (S2C_Txx (_txx currSt)))

            Just (C2S_LangCommand (ucmd, pkey, sign)) ->
              case verifySignature (fromBase58 pkey) (toStrict (encode ucmd)) (fromBase58 sign) of
                False -> do
                  T.putStrLn ("Signature invalid." <> showt pkey)
                  WS.sendTextData (_conn conn)
                    (toStrict $ encode (S2C_ServerMessage ("Invalid signature for command " <> showt ucmd)))
                True -> do
                  -- add new connection
                  _ <- modifyMVar state (addClient conn (adr pkey))
                  case typeCheckCommand ucmd of
                    Left t -> do
                      T.putStrLn ("Requested command does not typecheck:" <> T.concat t)
                      WS.sendTextData (_conn conn)
                        (toStrict $ encode (S2C_ServerMessage ("Requested command does not type check: " <> showt ucmd)))
                    Right cmd -> evalCommand conn state (adr pkey) cmd
    forever loop

    where
      adr = toBase58 . deriveAddress . fromBase58



-- | NOTE Assumes that command issuer is already saved in serverstate !
evalCommand :: Conn -> MVar ServerState -> Base58Rep Address -> LangCommand -> IO ()
evalCommand conn srvr addr = \case
  BegCmd sklExpr -> do
    let s@(Skl addedSkl) = evalVal sklExpr
        newTx = showt s <> " transfered from the server."

    if addedSkl > 100
      then WS.sendTextData (_conn conn) (toStrict $ encode (S2C_ServerMessage "SKL amount too big"))
      else do
        clnst <- modifyMVar srvr
          (pure . duple .
           Map.adjust (\ClientState{..} -> ClientState{_skl=addSkl _skl s, _txx=newTx:_txx, ..}) addr)
        let currSt = fromJust (Map.lookup addr clnst)
            currConns = _conns currSt
            currSkl = _skl currSt
        sendResponse currConns (toStrict $ encode (S2C_ServerMessage newTx))
        sendResponse currConns (toStrict $ encode (S2C_SKL currSkl))

  TransferCmd sklExpr addrExpr -> do
    let s@(Skl sendSkl) = evalVal sklExpr
        recipAddr = evalVal addrExpr
        recipTx = showt addr <> " transfered " <> showt s
        senderTx = showt s <> " transfered to " <> showt recipAddr

    (clnst, err) <- modifyMVar srvr $ \ss -> pure $
      let sender = fromJust $ Map.lookup addr ss
      in
        if recipAddr == addr then (ss, (ss, Just "Recipient and sender must differ."))
        else if s == 0 then (ss, (ss, Just "SKL tokens count must be > 0."))
        else if _skl sender < s then (ss, (ss, Just "Not enough tokens for the transaction."))
        else
          let
            recip = fromMaybe (ClientState mempty (Skl 0) []) (Map.lookup recipAddr ss)
            updateRecip ClientState{..} = ClientState{_skl=addSkl _skl s, _txx=recipTx:_txx, ..}
            updateSender ClientState{..} = ClientState{_skl=subSkl _skl s, _txx=senderTx:_txx, ..}
            ss'' = Map.insert recipAddr (updateRecip recip) (Map.insert addr (updateSender sender) ss)
          in (ss'', (ss'', Nothing))

    case err of
      Just errMsg -> WS.sendTextData (_conn conn) (toStrict $ encode (S2C_ServerMessage errMsg))
      Nothing -> do
        let sender' = fromJust $ Map.lookup addr clnst
            recip' = fromJust $ Map.lookup recipAddr clnst

        sendResponse (_conns recip')
          (toStrict $ encode (S2C_SKL (_skl recip')))
        sendResponse (_conns sender')
          (toStrict $ encode (S2C_SKL (_skl sender')))
        sendResponse (_conns recip')
          (toStrict $ encode
            (S2C_ServerMessage recipTx))
        sendResponse (_conns sender')
          (toStrict $ encode
            (S2C_ServerMessage senderTx))
  where
    duple x = (x, x)
    incSKL skl (Just (ClientState conns s txx)) = Just (ClientState conns (addSkl skl s) txx)


    subSkl (Skl x) (Skl y) = Skl (x-y)
    addSkl (Skl x) (Skl y) = Skl (x+y)

    evalValT :: ValT x -> EvalT x
    evalValT = \case
      VAddr x -> x
      VSkl x -> x

    evalVal :: Val x -> EvalT x
    evalVal = \case
      ValId x -> evalValT x
      ValAdd x y -> addSkl (evalVal x) (evalVal y)
