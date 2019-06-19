{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PackageImports #-}

module Common.Crypto where

import Debug.Trace

import Data.Maybe (fromJust, isJust)
import Data.ByteArray (ByteArrayAccess)
import qualified Data.ByteArray as BA
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.ByteString as B
import Data.ByteString (ByteString)
import qualified Data.ByteString.Base58 as B58
import Data.Void
import Data.String (IsString)
import Control.DeepSeq
import Control.Monad (replicateM)

import Common.Base58.Internal
import Common.ParseUtils
import Crypto.Error

import Text.Megaparsec
import Text.Megaparsec.Char


import "cryptonite" Crypto.Random (MonadRandom)

import qualified Crypto.PubKey.Ed25519 as Ed25519

-- ===================== SECRET KEY =======================

newtype SecretKey = SecretKey {_unsecrettemp :: Ed25519.SecretKey}
  deriving(Eq,Show)

parseSecretKeyB58 :: (ShowErrorComponent e, Ord e) => Parsec e ByteString (Base58Rep SecretKey)
parseSecretKeyB58 = fmap (Base58Rep "sk$") $ do
  string "sk$"
  b58 <- parseB58 44
  let pk = do
        bts <- B58.decodeBase58 B58.bitcoinAlphabet b58
        maybeCryptoError (Ed25519.secretKey bts)
  case pk of
    Just pk -> return b58
    Nothing -> fail "Couldn't parse secret key."

instance HasBas58Rep SecretKey where
  toBase58 (SecretKey sk) = Base58Rep "sk$" (B58.encodeBase58 B58.bitcoinAlphabet (B.pack (BA.unpack sk)))
  fromBase58 (Base58Rep _ bts) = SecretKey $ unsafeFromBts $ fromJust (B58.decodeBase58 B58.bitcoinAlphabet bts)
    where
      unsafeFromBts bts = case Ed25519.secretKey bts of
        CryptoPassed a -> a
        _ -> error "Should not happend in fromBase58 SecretKey."

  fromBase58BString = parseMaybe @Void parseSecretKeyB58


generateSecretKey :: MonadRandom m => m SecretKey
generateSecretKey = SecretKey <$> Ed25519.generateSecretKey

sign :: ByteArrayAccess ba => SecretKey -> PublicKey -> ba -> Signature
sign (SecretKey sk) (PublicKey pk) ba = Signature (Ed25519.sign sk pk ba)

-- ===================== PUBLIC  KEY =======================

newtype PublicKey = PublicKey Ed25519.PublicKey
  deriving(Eq, Show, NFData)


parsePublicKeyB58 :: (ShowErrorComponent e, Ord e) => Parsec e ByteString (Base58Rep PublicKey)
parsePublicKeyB58 = fmap (Base58Rep "pk$") $ do
  string "pk$"
  b58 <- parseB58 44
  let pk = do
        bts <- B58.decodeBase58 B58.bitcoinAlphabet b58
        maybeCryptoError (Ed25519.publicKey bts)
  case pk of
    Just pk -> return b58
    Nothing -> fail "Couldn't parse public key."



instance HasBas58Rep PublicKey where
  toBase58 (PublicKey pk) = Base58Rep "pk$" (B58.encodeBase58 B58.bitcoinAlphabet (B.pack (BA.unpack pk)))
  fromBase58 (Base58Rep _ bts) = PublicKey $ unsafeFromBts $ fromJust (B58.decodeBase58 B58.bitcoinAlphabet bts)
    where
      unsafeFromBts bts = case Ed25519.publicKey bts of
        CryptoPassed a -> a
        _ -> error "Should not happend in fromBase58 PublicKey."

  fromBase58BString = parseMaybe @Void parsePublicKeyB58

publicKeyfromSecret :: SecretKey -> PublicKey
publicKeyfromSecret (SecretKey sk)= PublicKey $ Ed25519.toPublic sk


-- ==================== SIGNATURE =====================

newtype Signature = Signature Ed25519.Signature
  deriving(Eq, Show, NFData)

verifySignature :: ByteArrayAccess ba => PublicKey -> ba -> Signature -> Bool
verifySignature (PublicKey pk) ba (Signature sig) = Ed25519.verify pk ba sig

-- ===================== ADDRESS =======================

newtype Address = Address ByteString
  deriving(Eq)

lastBytesCount :: Int
lastBytesCount = 15

parseAddressB58 :: (ShowErrorComponent e, Ord e) => Parsec e ByteString (Base58Rep Address)
parseAddressB58 = Base58Rep "$$" <$> (string "$$" *> (try (parseB58 21) <|> parseB58 20))

parseAddressB58Text :: (ShowErrorComponent e, Ord e) => Parsec e Text (Base58Rep Address)
parseAddressB58Text = Base58Rep "$$" . B.pack . charsToWords . T.unpack <$> (string "$$" *> (try (parseB58Text 21) <|> parseB58Text 20))

instance HasBas58Rep Address where
  toBase58 (Address adr) = Base58Rep "$$" (B58.encodeBase58 B58.bitcoinAlphabet adr)
  fromBase58 (Base58Rep _ bts) = Address $ fromJust (B58.decodeBase58 B58.bitcoinAlphabet bts)
  fromBase58BString = parseMaybe @Void parseAddressB58

deriveAddress :: PublicKey -> Address
deriveAddress (PublicKey pk) = Address $ B.pack (drop (BA.length pk - lastBytesCount) (BA.unpack pk))

ownsAddress :: PublicKey -> Address -> Bool
ownsAddress pk addr = deriveAddress pk == addr
