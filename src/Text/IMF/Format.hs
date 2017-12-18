{-# LANGUAGE OverloadedStrings #-}

module Text.IMF.Format
  ( ParseMessage
  , msgParser
  , parseMessage
  , parseMessageList
  , FormatMessage
  , formatMessage
  -- * Re-exports
  , ParseError
  ) where

import           Text.Parsec      (ParseError, ParsecT, eof, many, parse, sepBy)
import           Text.Parsec.Char (string)

class ParseMessage a where
  -- | Parser for a message component.
  msgParser :: Monad m => ParsecT String u m a

-- | Parse a message component from a string.
parseMessage :: ParseMessage a => String -> Either ParseError a
parseMessage = parse (msgParser <* eof) ""

-- | @parseMessageList sep input@ parses a list of message components from @input@ serperated by @sep@.
parseMessageList :: ParseMessage a => String -> String -> Either ParseError [a]
parseMessageList ""  = parse (many msgParser <* eof) ""
parseMessageList sep = parse (msgParser `sepBy` string sep <* eof) ""

class FormatMessage a where
  -- | Format a message component into a string.
  formatMessage :: a -> String
