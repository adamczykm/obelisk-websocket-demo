{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Common.Base58.Internal where

import Data.Aeson
import Data.Aeson.Parser
import Data.Text (Text)
import qualified Data.Text as T
import Data.Maybe (isJust)
import qualified Data.ByteString.Base58 as B58
import Data.ByteString.Char8
import qualified Data.ByteString as B
import Data.Text.Encoding
import Control.Monad (replicateM)
import           GHC.Generics (Generic)
import TextShow
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Text.Internal.Unsafe.Char (unsafeChr8)
import Common.ParseUtils

data Base58Rep a = Base58Rep { _hdr ::  ByteString, _base58 :: ByteString }
  deriving(Eq, Ord, Generic)

-- instance ToJSON (Base58Rep a) where toEncoding = genericToEncoding defaultOptions
instance FromJSON (Base58Rep a) where
  parseJSON = withObject "Base58Rep" $ \v -> Base58Rep <$>
    (fmap (B.pack . charsToWords . T.unpack) (v .: "_hdr")) <*>
    (fmap (B.pack . charsToWords . T.unpack) (v .: "_base58"))

instance ToJSON (Base58Rep a) where
  toJSON (Base58Rep h b) = object ["_hdr" .= decodeLatin1 h, "_base58" .= decodeLatin1 b]

instance Show (Base58Rep a) where
  show = T.unpack . showt

instance TextShow (Base58Rep a) where
  showb (Base58Rep h b) = fromText (T.pack $ unpack h) <> fromText (T.pack $ unpack b)

class HasBas58Rep t where
  fromBase58BString :: ByteString -> Maybe (Base58Rep t)
  toBase58 :: t -> Base58Rep t
  fromBase58 :: Base58Rep t -> t


-- | Returns ByteString of given length if its in B58
parseB58 :: (ShowErrorComponent e, Ord e) => Int -> Parsec e ByteString ByteString
parseB58 len = do
  tkns <- replicateM len anyChar
  if isJust $ B58.decodeBase58 B58.bitcoinAlphabet (B.pack tkns)
    then return (pack (unsafeChr8 <$> tkns))
    else fail "Could not parse address"

-- | Returns ByteString of given length if its in B58
parseB58Text :: (ShowErrorComponent e, Ord e) => Int -> Parsec e Text Text
parseB58Text len = dbg "parseb58" $ do
  tkns <- replicateM len anyChar
  if isJust $ B58.decodeBase58 B58.bitcoinAlphabet (B.pack (fromIntegral . fromEnum <$> tkns))
    then return (T.pack tkns)
    else fail "Could not parse address"

fromBase58Text :: HasBas58Rep t => T.Text -> Maybe (Base58Rep t)
fromBase58Text = fromBase58BString . pack . T.unpack
