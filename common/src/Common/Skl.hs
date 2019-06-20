{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module Common.Skl where

import TextShow
import Data.Word
import Data.Aeson

newtype Skl = Skl Word64
  deriving(Eq,Ord,ToJSON,FromJSON, Enum, Num, Real, Integral)

instance Show Skl where
  show (Skl i) = "SKL" ++ show i

instance TextShow Skl where
  showb (Skl i) = fromText "SKL" <> showb i
