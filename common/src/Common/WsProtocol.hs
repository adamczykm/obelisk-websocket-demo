{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}


module Common.WsProtocol where


import Common.Crypto
import Data.GADT.Compare
import Data.GADT.Show
import Data.GADT.Compare.TH
import Data.GADT.Show.TH
import TextShow


import           Data.Aeson (ToJSON, FromJSON, toEncoding, parseJSON,
                            defaultOptions, Options,
                            genericToEncoding, genericParseJSON)

import qualified Data.Text as T
import Data.Text (Text)
import           GHC.Generics (Generic)

-- temp

newtype Nickname = Nickname Text
  deriving(Eq,Show,Ord, ToJSON,FromJSON, TextShow)

data C2S = C2S_Join Nickname
         | C2S_Msg (Nickname, Text)
         -- | C2S_msg EncryptedMsg
         | C2S_Close
         deriving (Eq, Show, Generic)


options :: Options
options = defaultOptions -- { tagSingleConstructors = True }

instance ToJSON C2S where toEncoding = genericToEncoding options
instance FromJSON C2S where parseJSON = genericParseJSON options

data S2C = S2C_Welcome
         | S2C_ServerMessage Text
         | S2C_Msg (Nickname, Text)
         deriving (Eq,Show, Generic)

instance ToJSON S2C where toEncoding = genericToEncoding options
instance FromJSON S2C where parseJSON = genericParseJSON options


data S2CEventTag a where
  S2C_WelcomeTag :: S2CEventTag ()
  S2C_ServerMessageTag :: S2CEventTag Text
  S2C_MsgTag :: S2CEventTag (Nickname, Text)

deriveGShow ''S2CEventTag
deriveGEq ''S2CEventTag
deriveGCompare ''S2CEventTag

data C2SEventTag a where
  C2S_JoinTag :: C2SEventTag Nickname
  C2S_MsgTag :: C2SEventTag (Nickname, Text)
  C2S_CloseTag :: C2SEventTag ()

deriveGShow ''C2SEventTag
deriveGEq ''C2SEventTag
deriveGCompare ''C2SEventTag
