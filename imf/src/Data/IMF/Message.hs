{-# LANGUAGE OverloadedStrings #-}

module Data.IMF.Message
  ( Message(..)
  , ContentPart(..)
  , buildMessage
  )
where

import           Data.Text        (Text)
import qualified Data.Text        as T

import           Data.IMF.Header
import           Data.IMF.Mailbox
import           Data.IMF.Types

data Message = Message
    { msgHeaders :: [Header]
    , msgBody    :: Text
    }
  deriving (Show, Eq)

instance HasFormatter Message where
    format (Message headers body) = formatList headers <> "\r\n" <> body

data ContentPart = ContentPart
    { contentType :: Text
    , content     :: Text
    }

buildMessage :: HeaderOptions -> ContentPart -> IO (Text, [Mailbox], Message)
buildMessage headerOpts (ContentPart _ content) = do
    (domain, recipients, headers) <- buildHeaders headerOpts
    return (domain, recipients, Message headers content)

