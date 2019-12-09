module Data.IMF.Parsers.Types
  ( HasParser(..)
  , parse
  , HasListParser(..)
  , parseList
  )
where

import Data.Attoparsec.Text (Parser, endOfInput, parseOnly)
import Data.Text            (Text)

class HasParser a where
    parser :: Parser a

parse :: HasParser a => Text -> Either String a
parse = parseOnly (parser <* endOfInput)

class HasListParser a where
    listParser :: Parser [a]

parseList :: HasListParser a => Text -> Either String [a]
parseList = parseOnly (listParser <* endOfInput)

