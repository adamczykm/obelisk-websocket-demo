{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Frontend where

import qualified Data.Text as T
import Obelisk.Frontend
import Obelisk.Route
import Reflex.Dom.Core

import Common.Api
import Common.Route
import Common.Crypto
import Obelisk.Generated.Static

import Control.Monad
import Control.Monad.Fix


import Common.WsProtocol


subscribePubKeyWidget
  :: ( DomBuilder t m , MonadFix m , PostBuild t m , PerformEvent t m , Prerender js t m)
  => m (Event t Nickname)
subscribePubKeyWidget = el "div" $ do
  rec
    tn <- inputElement $ def
        & inputElementConfig_setValue .~ fmap (const "") subEv
        & inputElementConfig_elementConfig . elementConfig_initialAttributes .~
          ("placeholder" =: "Enter your RSA public key")
        & inputElementConfig_elementConfig . elementConfig_modifyAttributes .~
          never

    bn <- button "Subscribe with public key."
    let subEv = fmap (Nickname . T.strip)
          $ tag (current $ value tn)
          $ leftmost [bn, keypress Enter tn]
  return subEv



frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = el "title" $ text "Obelisk Minimal Example"
  , _frontend_body = do
      text "Welcome to Obelisk!"
      el "p" $ text $ T.pack commonStuff
      elAttr "img" ("src" =: static @"obelisk.jpg") blank
      el "div" $ do
        void $ subscribePubKeyWidget
  }
