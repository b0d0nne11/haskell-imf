{-# LANGUAGE AllowAmbiguousTypes #-}

module Text.IMF.Format
  ( Parser
  , ParseMessage
  , parseMessage
  , FormatMessage
  , formatMessage
  , validateMessage
  , concatMany
  -- * Re-exports
  , ParseError
  )
where

import           Text.Parsec                    ( ParseError
                                                , Parsec
                                                , many
                                                )

type Parser = Parsec String ()

class ParseMessage a where
    -- | Parse a message component from a string.
    parseMessage :: String -> Either ParseError a

class FormatMessage a where
    -- | Format a message component into a string.
    formatMessage :: a -> String

-- | Check the validity of a message component by formatting and re-parsing it.
validateMessage :: (ParseMessage a, FormatMessage a) => a -> Either ParseError a
validateMessage = parseMessage . formatMessage

-- | Apply string parser 0 or more times and concatenate the results.
concatMany :: Parser String -> Parser String
concatMany = fmap concat . many
