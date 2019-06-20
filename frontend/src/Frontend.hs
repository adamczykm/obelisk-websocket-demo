{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Frontend where

import Debug.Trace


import Data.ByteString.Lazy (toStrict)
import Data.Maybe
import Data.Dependent.Map
import Data.Map.Strict (Map)
import Data.Functor.Sum
import Data.List.NonEmpty (nonEmpty, NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import qualified Data.Text as T
import Control.Arrow (left)
import Control.Monad
import Control.Monad.Identity
import Control.Monad.IO.Class
import Control.Monad.Fix
import Text.URI
import TextShow (showt)

import JSDOM.Types(MonadJSM)

import Reflex.Dom.Core hiding (mainWidget)
import Reflex.Dom.Time

import Obelisk.Frontend
-- import Obelisk.ExecutableConfig.Frontend
import qualified Obelisk.ExecutableConfig as Cfg
import Obelisk.Generated.Static
import Obelisk.Route

import Common.Api
import Common.Route
import Common.WsProtocol
import Common.Base58
import Common.Lang
import Common.Skl

import Common.Crypto (Keys(..))
import qualified Common.Crypto as Crypto


import qualified Data.Aeson
import qualified Data.ByteArray as BA
import qualified Data.ByteString as B
import Data.Text.Encoding
import qualified Data.ByteString.Base58 as B58


frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = el "title" $ text "Obelisk Minimal Example"
  , _frontend_body = do
      r <- liftIO $ Cfg.get "config/common/route"
      mainWidget r
  }



mainWidget :: forall t m js. (MonadFix m, PerformEvent t m, Prerender js t m) => Maybe Text -> m ()
mainWidget r = prerender_ (return ()) $ do

  -- server to client events
  ( c2sEvent, c2sEventTrigger ) <- newTriggerEvent
  s2cEvent <- webSocketClient r (traceEventWith (const "c2s event") c2sEvent)
  let s2cEventSel = fan (tagServerEvent <$> s2cEvent)
  -------------------------

  -- c2sNe <- chatWidget (select s2cEventSel S2C_MsgTag)

  mkeys <- genSecretKeyWidget
  el "hr" $ return ()
  commandExamples
  mcmd <- commandWidget
  cmdSignedEv <- signCommandWidget mkeys mcmd
  el "hr" $ return ()
  statusWidget (updated mkeys) s2cEventSel

  -- client to server events
  let
    msg = 1 -- "TODO"
    getSklState = C2S_PingForSKL . flip Crypto.signWithKeys msg <$> fmapMaybe id (updated mkeys)
    c2sEvents = mergeWith (<>) [(:[]) <$> getSklState,  (:[]) <$> cmdSignedEv]
  (performEvent_ $ liftIO . c2sEventTrigger <$> c2sEvents)


webSocketClient
  :: ( MonadJSM (Performable m), MonadJSM m, MonadHold t m, HasJSContext m
     , PerformEvent t m, TriggerEvent t m, PostBuild t m, DomBuilder t m)
  => Maybe Text -> Event t [C2S] -> m (Event t S2C)
webSocketClient r sendMsgEv =
  case checkEncoder backendRouteEncoder of
    Left err -> do
      el "div" $ text err
      return never
    Right encoder -> do
      let wsPath = fst $ encode encoder $ InL BackendRoute_WebSocket :/ ()
          mUri = do
            uri' <- mkURI =<< r
            pathPiece <- nonEmpty =<< mapM mkPathPiece wsPath
            wsScheme <- case uriScheme uri' of
              rtextScheme | rtextScheme == mkScheme "https" -> mkScheme "wss"
              rtextScheme | rtextScheme == mkScheme "http" -> mkScheme "ws"
              _ -> Nothing
            return $ uri'
              { uriPath = Just (False, pathPiece)
              , uriScheme = Just wsScheme
              }
      case mUri of
        Nothing -> return never
        Just uri -> do
          traceM (show (render uri))
          ws <- jsonWebSocket (render uri) $ def & webSocketConfig_send .~ (traceEventWith (const "sendMsgEv")sendMsgEv)
          return $ fmapMaybe id $ _webSocket_recv ws



genSecretKeyWidget
  :: (MonadFix m, TriggerEvent t m, MonadIO (Performable m), PerformEvent t m, PostBuild t m, MonadHold t m, DomBuilder t m)
  => m (Dynamic t (Maybe Crypto.Keys))
genSecretKeyWidget = el "div" $ do

  -- secret key input / generation
  btnClick <- el "div" $ do
    r <- button "Generate"
    el "span" $ text " or enter your Secret Key:"
    return r

  genSecretEv <- performEvent (liftIO Crypto.generateSecretKey <$ btnClick)
  skTextArea <- el "div" $ textAreaElement $ def
    & textAreaElementConfig_setValue .~ (showt . toBase58 <$> genSecretEv)

  newKeysEv <- fmap (fmap (Crypto.genBase58Keys . fromBase58) . fromBase58Text) <$>
    debounce 0.2 ( updated ( _textAreaElement_value skTextArea))

  -- public key generation
  pkText <- holdDyn "" (maybe "<invalid secret key>" (showt . _public) <$> newKeysEv)
  readonlyTextArea "Your public key:" pkText

  -- address generation
  adrText <- holdDyn "" (maybe "<invalid public key>" (showt . _address) <$> newKeysEv)
  readonlyTextArea "Your address:" adrText

  holdDyn Nothing newKeysEv


commandWidget
  :: (MonadFix m, TriggerEvent t m, MonadIO (Performable m)
     , PerformEvent t m, PostBuild t m, MonadHold t m, DomBuilder t m)
  => m (Dynamic t (Maybe LangCommand))
commandWidget = do
  el "div" $ text "Enter your command"

  commandTextArea <- el "div" $ textAreaElement def
  let parseCmd txt = case parseCommand txt of
        Nothing -> Left "Command parse error!"
        Just x -> left T.concat $ typeCheckCommand x
  eithCommand :: Dynamic t (Either Text LangCommand) <- holdDyn (Left "") =<< fmap parseCmd . traceEventWith show<$>
    debounce 0.2 ( updated $ _textAreaElement_value commandTextArea)

  el "div" $ dynText (showt <$> eithCommand)

  return (eithCommand <#> \case
             Left _ -> Nothing
             Right c -> Just c)




signCommandWidget :: (PostBuild t m, DomBuilder t m) => Dynamic t (Maybe Keys) -> Dynamic t (Maybe LangCommand) -> m (Event t C2S)
signCommandWidget mkeys mcmd = do

  let dynVals = ffor2 mkeys mcmd $ \k c -> case (k, c) of
        (Just ks, Just cmd) -> ( mempty
                               , Just $ C2S_LangCommand (Crypto.signWithKeys ks (dropTypesCommand cmd) ))

        _ -> ("disabled" =: "", Nothing)

  btnClick <- dynAttrButton (fst <$> dynVals) "Sign and send command"

  return $ fmapMaybe id $ traceEventWith show $ tag (current (snd <$> dynVals)) btnClick



(<#>) :: Functor f => f a -> (a->b) -> f b
(<#>) = flip (<$>)


dynAttrButton :: (PostBuild t m, DomBuilder t m) => Dynamic t (Map Text Text) -> Text -> m (Event t ())
dynAttrButton dynattrs t = do
  (e,_) <- elDynAttr' "button" dynattrs $ text t
  return $ domEvent Click e


readonlyTextArea
  :: (DomBuilder t m, PostBuild t m) => Text -> Dynamic t Text -> m ()
readonlyTextArea label content = el "div" $ do
  el "div" $ text label
  el "div" $ elAttr "textarea" ("readonly" =: "") $ dynText content


statusWidget :: (MonadFix m, PostBuild t m, MonadHold t m, DomBuilder t m) => Event t a -> EventSelector t S2CEventTag -> m ()
statusWidget resetE selector = do
  el "div" $ do
    skls <- holdDyn (Skl 0) (leftmost [Skl 0 <$ resetE, select selector S2C_SklTag])
    el "soan" $ text "Your tokens: "
    el "span" $ dynText (showt <$> skls)
  el "div" $ do
    el "div" $ text "Transactions:"
    el "ul" $ do
      let folder (Left tx) txx = tx : txx
          folder (Right txx) _ = txx
      txx <- foldDyn folder []
              ( leftmost [ Right <$> select selector S2C_TxxTag
                         , Left <$> select selector S2C_ServerMessageTag])
      simpleList txx (el "li" . dynText)
  return ()


commandExamples :: (DomBuilder t m) => m ()
commandExamples = el "div" $ do
  el "div" $ text "Command examples"
  el "ul" $ do
    el "li" $ text "beg SKL10"
    el "li" $ text "beg [SKL5 + SKL5]"
    el "li" $ text "transfer SKL2019 to $$2g3vv2uAqh7qhoEv7Rskz"
