module Common.ParseUtils where

import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Word

parenthTerm t = space *> (try (char '(' *> space *> t <* space <* char ')') <|> t)


charsToWords :: [Char] -> [Word8]
charsToWords = fmap (fromIntegral . fromEnum)
