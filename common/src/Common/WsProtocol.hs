{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}


module Common.WsProtocol where


import Common.Base58
import Common.Skl
import Common.Lang
import Common.Crypto
import Control.Monad.Identity
import Data.GADT.Compare
import Data.GADT.Show
import Data.GADT.Compare.TH
import Data.GADT.Show.TH
import Data.Dependent.Map
import TextShow hiding (singleton)


import           Data.Aeson (ToJSON, FromJSON, toEncoding, parseJSON,
                            defaultOptions, Options,
                            genericToEncoding, genericParseJSON)

import qualified Data.Text as T
import Data.Text (Text)
import           GHC.Generics (Generic)

-- temp

newtype Nickname = Nickname Text
  deriving(Eq,Show,Ord, ToJSON,FromJSON, TextShow)


data C2S = C2S_LangCommand (UCommand, Base58Rep PublicKey, Base58Rep Signature)
         | C2S_PingForSKL (Int, Base58Rep PublicKey, Base58Rep Signature)
  deriving (Eq, Show, Generic)


options :: Options
options = defaultOptions -- { tagSingleConstructors = True }

instance ToJSON C2S where toEncoding = genericToEncoding options
instance FromJSON C2S where parseJSON = genericParseJSON options

data S2C = S2C_SKL Skl
         | S2C_Txx [Text]
         | S2C_ServerMessage { _serverMsg :: Text }
         deriving (Eq,Show, Generic)

instance ToJSON S2C where toEncoding = genericToEncoding options
instance FromJSON S2C where parseJSON = genericParseJSON options


data S2CEventTag a where
  S2C_TxxTag :: S2CEventTag [Text]
  S2C_SklTag :: S2CEventTag Skl
  S2C_ServerMessageTag :: S2CEventTag Text

deriveGShow ''S2CEventTag
deriveGEq ''S2CEventTag
deriveGCompare ''S2CEventTag

data C2SEventTag a where
  C2S_LangCommandTag :: C2SEventTag (UCommand, Base58Rep PublicKey, Signature)

deriveGShow ''C2SEventTag
deriveGEq ''C2SEventTag
deriveGCompare ''C2SEventTag



tagServerEvent :: S2C -> DMap S2CEventTag Identity
tagServerEvent = \case
  S2C_SKL skl -> singleton S2C_SklTag (Identity skl)
  S2C_Txx txx -> singleton S2C_TxxTag (Identity txx)
  S2C_ServerMessage txt -> singleton S2C_ServerMessageTag (Identity txt)
