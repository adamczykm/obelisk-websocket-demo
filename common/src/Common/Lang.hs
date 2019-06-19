{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}

module Common.Lang where

import qualified Data.ByteString as B

import qualified Data.Text as T
import Data.Text (Text)
import Data.Text.Encoding
import Data.Void
import Control.Arrow (left)
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

-- ======================== typed lang ==========================

data Sing (t :: T) where
  SAddr :: Sing 'TAddr
  SSkl :: Sing 'TSkl

data T
  = TAddr
  | TSkl
  deriving(Eq, Ord, Show)


data ValT a where
  VAddr :: Base58Rep Address -> ValT 'TAddr
  VSkl :: Skl -> ValT 'TSkl

instance TextShow (ValT a) where
  showb = \case
    VAddr a -> showb a
    VSkl s -> showb s

data Val a where
  ValId :: ValT a -> Val a
  ValAdd :: Val 'TSkl -> Val 'TSkl -> Val 'TSkl

instance TextShow (Val a) where
  showb = \case
    ValId v -> showb v
    ValAdd v1 v2 -> fromText "[" <> showb v1 <> fromText " + " <> showb v2 <> "]"

data LangCommand
 = BegCmd (Val 'TSkl)
 | TransferCmd (Val 'TSkl) (Val 'TAddr)

instance TextShow LangCommand where
  showb = \case
    BegCmd v -> fromText "beg " <> showb v
    TransferCmd v a -> fromText "transfer " <> showb v <> fromText " to " <> showb a

typeCheckValT :: [Text] -> Sing t -> UValT -> Either [Text] (ValT t)
typeCheckValT errB SAddr v@(UAddr a) = Right $ VAddr a
typeCheckValT errB SSkl v@(USkl a) = Right $ VSkl a
typeCheckValT errB SAddr v@(USkl _) = Left (showt v <> " should be an address and is a SKL amount." : errB)
typeCheckValT errB SSkl v@(UAddr _) = Left (showt v <> " should be a SKL amount and is an address." : errB)
typeCheckValT errB _ _ = Left ("INTERNAL ERROR: unimplemented type checking" : errB)


typeCheckVal :: [Text] -> Sing t -> UVal -> Either [Text] (Val t)
typeCheckVal errB s v@(UValId x) = ValId <$> typeCheckValT errB s x
typeCheckVal errB s@SSkl v@(UValAdd x1 x2) = ValAdd <$>
  typeCheckVal ("in the first argument of [+]: " : errB) s x1 <*>
  typeCheckVal ( "in the second argument of [+]: " : errB) s x2
typeCheckVal errB s _ = Left ("INTERNAL ERROR: unimplemented type checking" : errB)

-- data UVal
--   = UValId UValT
--   | UValAdd UVal UVal
--   deriving(Eq, Ord, Show)


typeCheckCommand :: UCommand -> Either [Text] LangCommand
typeCheckCommand = left reverse .  \case
  UBeg a -> BegCmd <$>
    typeCheckVal ["Type error in the argument of BEG command:"] SSkl a
  UTransfer s a -> TransferCmd <$>
    typeCheckVal ["Type error in the first argument of TRANSFER command:"] SSkl s <*>
    typeCheckVal ["Type error in the second argument of TRANSFER command:"] SAddr a
