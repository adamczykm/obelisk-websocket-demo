{-# LANGUAGE PackageImports #-}
module Common.Crypto
 ( PublicKey
 , EncryptedMsg
 , decryptText
 , encryptText
 ) where

import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.ByteString.Char8 as B
import Data.ByteString.Char8 (ByteString)

import qualified Crypto.PubKey.RSA as RSA
import Crypto.PubKey.RSA.PKCS15
import "cryptonite" Crypto.Random (MonadRandom)


type PublicKey = RSA.PublicKey

data EncryptedMsg = EncryptedMsg PublicKey ByteString

decryptText :: MonadRandom m => RSA.PrivateKey -> Text -> m (Either RSA.Error ByteString)
decryptText = undefined

encryptText :: MonadRandom m => PublicKey -> Text -> m (Either RSA.Error EncryptedMsg)
encryptText = undefined
