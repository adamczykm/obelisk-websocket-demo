{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables, DeriveGeneric #-}


module Common.WsProtocol where


import Common.Crypto


import           Data.Aeson (ToJSON, FromJSON, toEncoding, parseJSON,
                            defaultOptions, Options,
                            genericToEncoding, genericParseJSON)

import qualified Data.Text as T
import Data.Text (Text)
import           GHC.Generics (Generic)

-- temp

newtype Nickname = Nickname Text
  deriving(Eq,Show,ToJSON,FromJSON)

data C2S = C2S_join Nickname
         | C2S_msg (Nickname, Text)
         -- | C2S_msg EncryptedMsg
         | C2S_close
         deriving (Eq,Show, Generic)

options :: Options
options = defaultOptions -- { tagSingleConstructors = True }

instance ToJSON C2S where toEncoding = genericToEncoding options
instance FromJSON C2S where parseJSON = genericParseJSON options

data S2C = S2C_welcome
         | S2C_msg Text
         deriving (Eq,Show, Generic)

instance ToJSON S2C where toEncoding = genericToEncoding options
instance FromJSON S2C where parseJSON = genericParseJSON options
