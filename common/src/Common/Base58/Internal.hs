module Common.Base58.Internal where

import Data.Maybe (isJust)
import qualified Data.ByteString.Base58 as B58
import Data.ByteString.Char8
import qualified Data.ByteString as B
import qualified Data.Text as T
import Data.Text.Encoding
import Control.Monad (replicateM)
import TextShow
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Text.Internal.Unsafe.Char (unsafeChr8)

data Base58Rep a = Base58Rep { _hdr ::  ByteString, _base58 :: ByteString }
  deriving(Eq, Ord)

instance Show (Base58Rep a) where
  show = T.unpack . showt

instance TextShow (Base58Rep a) where
  showb (Base58Rep h b) = fromText (T.pack $ unpack h) <> fromText (T.pack $ unpack b)

class HasBas58Rep t where
  fromBase58BString :: ByteString -> Maybe (Base58Rep t)
  toBase58 :: t -> Base58Rep t
  fromBase58 :: Base58Rep t -> t


-- | Returns ByteString of given length if its in B58
parseB58 :: Ord e => Int -> Parsec e ByteString ByteString
parseB58 len = do
  tkns <- replicateM len anyChar
  if isJust $ B58.decodeBase58 B58.bitcoinAlphabet (B.pack tkns)
    then return (pack (unsafeChr8 <$> tkns))
    else fail "Could not parse address"

fromBase58Text :: HasBas58Rep t => T.Text -> Maybe (Base58Rep t)
fromBase58Text = fromBase58BString . pack . T.unpack
