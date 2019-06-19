{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Frontend where

import Debug.Trace

import Data.Maybe
import Data.Dependent.Map
import Data.Map.Strict (Map)
import Data.Functor.Sum
import Data.List.NonEmpty (nonEmpty, NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import qualified Data.Text as T
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
import qualified Common.Crypto as Crypto

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



mainWidget :: (MonadFix m, PerformEvent t m, Prerender js t m) => Maybe Text -> m ()
mainWidget r = prerender_ (return ()) $ do
  -- ( c2sEvent, c2sEventTrigger ) <- newTriggerEvent
  -- s2cEvent <- webSocketClient r c2sEvent
  -- let s2cEventSel = fan (tagEvent <$> s2cEvent)

  -- c2sNe <- chatWidget (select s2cEventSel S2C_MsgTag)
  -- let c2sEvents =  mergeWith (<>) [c2sNe]
  -- (performEvent_ $ liftIO . c2sEventTrigger . NE.toList <$> c2sEvents)

  genSecretKeyWidget
  el "hr" $ return ()
  commandWidget

  where
    tagEvent :: S2C -> DMap S2CEventTag Identity
    tagEvent = \case
      S2C_Welcome -> singleton S2C_WelcomeTag (Identity ())
      S2C_ServerMessage msg -> singleton S2C_ServerMessageTag (Identity msg)
      S2C_Msg msg -> singleton S2C_MsgTag (Identity msg)


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
          ws <- jsonWebSocket (render uri) $ def & webSocketConfig_send .~ sendMsgEv
          return $ fmapMaybe id $ _webSocket_recv ws


genSecretKeyWidget
  :: (MonadFix m, TriggerEvent t m, MonadIO (Performable m), PerformEvent t m, PostBuild t m, MonadHold t m, DomBuilder t m)
  => m ()
genSecretKeyWidget = el "div" $ do

  -- secret key input / generation
  btnClick <- el "div" $ do
    r <- button "Generate"
    el "span" $ text " or enter your Secret Key:"
    return r
  genKeyEv <- performEvent (liftIO Crypto.generateSecretKey <$ btnClick)
  skTextArea <- el "div" $ textAreaElement $ def
    & textAreaElementConfig_setValue .~ (showt . toBase58 <$> genKeyEv)

  skKeyChangedEv <- fmap (fmap fromBase58 . fromBase58Text) <$>
    debounce 0.2 ( updated ( _textAreaElement_value skTextArea))

  -- public key generation
  pkText <- holdDyn "" (maybe "<invalid secret key>" (showt . toBase58 . Crypto.publicKeyfromSecret) <$> skKeyChangedEv)
  readonlyTextArea "Your public key:" pkText

  -- address generation
  adrText <- holdDyn "" (maybe "<invalid public key>" (showt . toBase58 . Crypto.deriveAddress . Crypto.publicKeyfromSecret) <$> skKeyChangedEv)
  readonlyTextArea "Your address:" adrText

  return ()


commandWidget
  :: (MonadFix m, TriggerEvent t m, MonadIO (Performable m)
     , PerformEvent t m, PostBuild t m, MonadHold t m, DomBuilder t m)
  => m ()
commandWidget = do
  el "div" $ text "Enter your command"

  commandTextArea <- el "div" $ textAreaElement def
  let parseCmd txt = case parseCommand txt of
        Nothing -> Left "Command parse error!"
        Just x -> Right x
  eithCommand :: Dynamic t (Either Text UCommand) <- holdDyn (Left "") =<< fmap parseCmd . traceEventWith show<$>
    debounce 0.2 ( updated $ _textAreaElement_value commandTextArea)

  el "div" $ dynText (showt <$> eithCommand)

  let btnAttrs = ffor eithCommand $ \case
        Left _ -> ("disabled" =: "")
        Right _ -> mempty

  dynAttrButton btnAttrs "Sign and send command"

  -- let skKeyChangedEv = traceEventWith show $ fmap (fmap fromBase58 . fromBase58Text) (updated $ _textAreaElement_value commandTextArea)
  return ()


dynAttrButton :: (PostBuild t m, DomBuilder t m) => Dynamic t (Map Text Text) -> Text -> m (Event t ())
dynAttrButton dynattrs t = do
  (e,_) <- elDynAttr' "button" dynattrs $ text t
  return $ domEvent Click e


readonlyTextArea
  :: (DomBuilder t m, PostBuild t m) => Text -> Dynamic t Text -> m ()
readonlyTextArea label content = el "div" $ do
  el "div" $ text label
  el "div" $ elAttr "textarea" ("readonly" =: "") $ dynText content


-- chatWidget
--   :: ( MonadFix m, MonadHold t m, PerformEvent t m, DomBuilder t m)
--   => Event t (Nickname, Text) -> m (Event t (NonEmpty C2S))
-- chatWidget servMsg = do
--   nickLoginEv <- traceEventWith (const "nicklogin") . fmap (C2S_Join . Nickname) <$> simpleInputWidget "Your nickname" "Join chat"
--   recpNickEv <- simpleInputWidget "Recipient nickname" "Set recipient nickname"
--   recpNick <- fmap Nickname <$> holdDyn "" recpNickEv
--   sndMsgEv <- fmap C2S_Msg . attach (current recpNick) <$> simpleInputWidget "Nickname" "Join chat"
--   return $ mergeList [sndMsgEv, nickLoginEv]

-- simpleInputWidget :: ( DomBuilder t m , MonadFix m) => Text -> Text -> m (Event t Text)
-- simpleInputWidget plhText btnText = el "div" $ do
--   rec
--     tn <- inputElement $ def
--         & inputElementConfig_setValue .~ fmap (const "") subEv
--         & inputElementConfig_elementConfig . elementConfig_initialAttributes .~
--           ("placeholder" =: plhText)
--         & inputElementConfig_elementConfig . elementConfig_modifyAttributes .~
--           never

--     bn <- button btnText
--     let subEv = fmap T.strip
--           $ tag (current $ value tn)
--           $ leftmost [bn, keypress Enter tn]
--   return subEv


-- noPrerender
--   :: (Reflex t, MonadHold t m, Prerender js t m)
--   => (PrerenderClientConstraint js t (Client m) => Client m (Event t a)) -> m (Event t a)
-- noPrerender w = prerender (return never) w >>= switchHold never . updated
