{-# LANGUAGE OverloadedStrings #-}

module Data.IMF.Command
  ( Command(..)
  , Param(..)
  )
where

import Data.Text (Text)
import Data.IMF.Types

data Command = EHLO Text
             | HELO Text
             | MAIL Text Text
             | RCPT Text Text
             | DATA
             | RSET
             | VRFY Text Text
             | NOOP
             | QUIT
             | STARTTLS
             | AUTH Text
  deriving (Show, Eq)

instance HasFormatter Command where
    format (HELO a)   = "HELO " <> format a
    format (EHLO a)   = "EHLO " <> format a
    format (MAIL a _) = "MAIL FROM: <" <> format a <> ">"
    format (RCPT a _) = "RCPT TO: <" <> format a <> ">"
    format  DATA      = "DATA"
    format  RSET      = "RSET"
    format (VRFY a _) = "VRFY " <> format a
    format  NOOP      = "NOOP"
    format  QUIT      = "QUIT"
    format  STARTTLS  = "STARTTLS"
    format (AUTH a)   = "AUTH " <> format a

data Param = SIZE Int
           | BODY Text
           | SMTPUTF8
           | AUTHP Text
           | LOGIN Text
           | PLAIN Text
           | Unsupported (Text, Text)
  deriving (Show, Eq)

