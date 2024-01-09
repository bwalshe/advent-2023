module Util where

import Control.Arrow (left)
import Data.Text (Text, pack)
import Data.Void (Void)
import Text.Megaparsec

type Task a = String -> Text -> Either Text a

type Parser = Parsec Void Text

runParserSimple :: Parser a -> String -> Text -> Either Text a
runParserSimple p f t = left (pack . errorBundlePretty) (runParser p f t)
