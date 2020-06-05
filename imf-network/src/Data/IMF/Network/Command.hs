{-# LANGUAGE OverloadedStrings #-}

module Data.IMF.Network.Command
  ( Command(..)
  , pCommand
  )
where

import Control.Applicative  ((<|>))
import Data.Attoparsec.Text (Parser)

data Command = EHLO
             | HELO
             | MAIL
             | RCPT
             | DATA
             | RSET
             | VRFY
             | NOOP
             | QUIT
             | STARTTLS
             | AUTH

-- | Parser for SMTP command
pCommand :: Parser Command
pCommand = EHLO     <$ "EHLO"
       <|> HELO     <$ "HELO"
       <|> MAIL     <$ "MAIL FROM:"
       <|> RCPT     <$ "RCPT TO:"
       <|> DATA     <$ "DATA"
       <|> RSET     <$ "RSET"
       <|> VRFY     <$ "VRFY"
       <|> NOOP     <$ "NOOP"
       <|> QUIT     <$ "QUIT"
       <|> STARTTLS <$ "STARTTLS"
       <|> AUTH     <$ "AUTH"
