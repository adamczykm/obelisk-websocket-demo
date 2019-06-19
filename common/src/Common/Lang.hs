{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}

module Common.Lang where

import Data.ByteString
import Data.Text
import Data.Text.Encoding
import Data.Void
import Common.Crypto
import Common.Base58
import Common.Skl
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Byte.Lexer as BP
import qualified Text.Megaparsec.Char.Lexer as CP
import TextShow
import Common.ParseUtils


data UValT
  = UAddr (Base58Rep Address)
  | USkl Skl
  deriving(Eq, Ord, Show)

instance TextShow UValT where
  showb = \case
    UAddr adr -> fromText "(UAddr " <> showb adr <> fromText ")"
    USkl skl -> fromText "(USkl " <> showb skl <> fromText ")"

data UVal
  = UValId UValT
  | UValAdd UVal UVal
  deriving(Eq, Ord, Show)

instance TextShow UVal where
  showb = \case
    UValId uvalt -> fromText "(UValId " <> showb uvalt <> fromText ")"
    UValAdd uv1 uv2 -> fromText "(UValAdd " <> showb uv1 <> showbSpace <> showb uv2 <> fromText ")"

data UCommand
 = UBeg UVal
 | UTransfer UVal UVal
 deriving(Eq,Show,Ord)

instance TextShow UCommand where
  showb = \case
    UBeg uv -> fromText "(UBeg " <> showb uv <> fromText ")"
    UTransfer uv1 uv2 -> fromText "(UTransfer " <> showb uv1 <> showbSpace <> showb uv2 <> fromText ")"


parseUValT :: (ShowErrorComponent e, Ord e) => Parsec e Text UValT
parseUValT = parenthTerm (try parseAddr <|> parseSkl)
  where
    parseSkl = USkl <$> (string "SKL" *> (Skl <$> CP.decimal))
    parseAddr = UAddr <$> parseAddressB58Text


parseUVal :: (ShowErrorComponent e, Ord e) => Parsec e Text UVal
parseUVal = dbg "uval" $ ( try parseIdT <|> parseAdd )
  where
    parseAdd = UValAdd <$> (char '[' *> (parseUVal <* char '+')) <*> (parseUVal <* char ']')
    parseIdT = UValId <$> parseUValT


-- parseUValFromText :: Text -> Maybe UValT
-- parseUValFromText = parseMaybe @() parseUValT

commandParser :: (ShowErrorComponent e, Ord e) => Parsec e Text UCommand
commandParser = dbg "command " $ parenthTerm (try parseBeg <|> parseTransfer)
  where
    parseBeg = UBeg <$> (string' "beg" *> space1 *> parseUVal)
    parseTransfer = UTransfer <$>
      (string' "transfer" *> space1 *> parseUVal) <*>
      (space1 *> string' "to" *> space1 *> parseUVal)


parseCommand :: Text -> Maybe UCommand
parseCommand = parseMaybe @Void commandParser
