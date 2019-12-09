module Data.IMF.Command
  ( Command(..)
  , Param(..)
  )
where

import Data.Text (Text)

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

data Param = SIZE Int
           | BODY Text
           | SMTPUTF8
           | AUTHP Text
           | LOGIN Text
           | PLAIN Text
           | Unsupported (Text, Text)
  deriving (Show, Eq)

