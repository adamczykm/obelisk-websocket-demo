{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Common.Lang where

import Data.ByteString
import Data.Text
import Data.Text.Encoding
import Common.Crypto
import Common.Base58
import Common.Skl
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Byte.Lexer
import TextShow


data UVal
  = UAddr (Base58Rep Address)
  | USkl Skl
  | UBool Bool
  deriving(Eq, Ord, Show)


data UCommand
 = UBeg UVal
 | UTransfer UVal UVal
 | UDestroy
 | UChain UCommand UCommand
 | UNoop
 deriving(Eq,Show,Ord)


parseUVal :: Ord e => Parsec e ByteString UVal
parseUVal = try parseAddr <|> try parseSkl <|> parseBool
  where
    parseBool = UBool <$> (try (False <$ string "#f") <|> (False <$ string "#t"))
    parseSkl = USkl <$> (string "SKL" *> (Skl <$> decimal))
    parseAddr = UAddr <$> parseAddressB58


parseUValFromText :: Text -> Maybe UVal
parseUValFromText = parseMaybe @() parseUVal . encodeUtf8
