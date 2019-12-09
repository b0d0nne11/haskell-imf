{-# LANGUAGE OverloadedStrings #-}

module Data.IMF.Parsers.Command
  ( pCommand
  , pVrfyParams
  , pMailParams
  , pRcptParams
  , pAuthParams
  )
where

import           Prelude                  hiding (takeWhile)

import           Control.Applicative      (liftA2, (<|>))
import           Data.Attoparsec.Text     (Parser, decimal, sepBy, takeWhile, takeWhile1, (<?>))
import           Data.Char                (isAlphaNum)
import qualified Data.Text                as T

import           Data.IMF
import           Data.IMF.Parsers.Core
import           Data.IMF.Parsers.Mailbox

pCommand :: Parser Command
pCommand = EHLO <$> ("EHLO" *> pWSP *> pDomain)
       <|> HELO <$> ("HELO" *> pWSP *> pDomain)
       <|> liftA2 MAIL ("MAIL FROM:" *> optional pWSP *> pVCHAR <* optional pWSP) (optional pParamsText)
       <|> liftA2 RCPT ("RCPT TO:"   *> optional pWSP *> pVCHAR <* optional pWSP) (optional pParamsText)
       <|> DATA <$ "DATA"
       <|> RSET <$ "RSET"
       <|> liftA2 VRFY ("VRFY" *> pWSP *> pString <* optional pWSP) (optional pParamsText)
       <|> NOOP <$ ("NOOP" *> optional (pWSP *> pString))
       <|> QUIT <$ "QUIT"
       <|> STARTTLS <$ "STARTTLS"
       <|> AUTH <$> ("AUTH" *> pWSP *> pParamsText)

pParamsText :: Parser T.Text
pParamsText = takeWhile1 isAnyChar <?> "esmtp parameter text"

pVrfyParams :: Parser [Param]
pVrfyParams = pParam `sepBy` " "
  where
    pParam :: Parser Param
    pParam = SMTPUTF8 <$ "SMTPUTF8"
         <|> Unsupported <$> liftA2 (,) pKeyword (optional $ "=" *> pValue)

pMailParams :: Parser [Param]
pMailParams = pParam `sepBy` " "
  where
    pParam :: Parser Param
    pParam = SIZE <$> ("SIZE=" *> decimal)
         <|> BODY <$> ("BODY=" *> ("7BIT" <|> "8BITMIME"))
         <|> SMTPUTF8 <$ "SMTPUTF8"
         <|> AUTHP <$> ("AUTH=" *> pVCHAR)
         <|> Unsupported <$> liftA2 (,) pKeyword (optional $ "=" *> pValue)

pRcptParams :: Parser [Param]
pRcptParams = pParam `sepBy` " "
  where
    pParam :: Parser Param
    pParam = Unsupported <$> liftA2 (,) pKeyword (optional $ "=" *> pValue)

pAuthParams :: Parser Param
pAuthParams = PLAIN <$> ("PLAIN" *> optional (" " *> pVCHAR))
          <|> LOGIN <$> ("LOGIN" *> optional (" " *> pVCHAR))

pKeyword :: Parser T.Text
pKeyword = takeWhile1 isAlphaNum `append` optional (takeWhile1 isKeywordText) <?> "esmtp parameter keyword"
  where
    isKeywordText :: Char -> Bool
    isKeywordText c | isAlphaNum c = True
                    | c == '-'     = True
                    | otherwise    = False

pValue :: Parser T.Text
pValue = takeWhile1 isValueText <?> "esmtp parameter value"
  where
    isValueText :: Char -> Bool
    isValueText c | c == '='    = False
                  | c == ' '    = False
                  | isAnyChar c = True
                  | otherwise   = False

pString :: Parser T.Text
pString = pAtom <|> pQuotedString
