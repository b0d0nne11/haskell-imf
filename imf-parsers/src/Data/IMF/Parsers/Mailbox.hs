{-# LANGUAGE OverloadedStrings #-}

module Data.IMF.Parsers.Mailbox
  ( pAddrSpec
  , pMailbox
  , pMailboxList
  , pAddress
  , pAddressList
  , pDText
  , pDomain
  , pDomainLiteral
  )
where

import           Control.Applicative    (liftA2, (<|>))
import           Data.Attoparsec.Text   (Parser, endOfInput, parseOnly, sepBy1, takeWhile1, (<?>))
import           Data.String            (IsString, fromString)
import qualified Data.Text              as T

import           Data.IMF
import           Data.IMF.Parsers.Core
import           Data.IMF.Parsers.Types

instance HasParser Mailbox where
    parser = pMailbox

instance HasListParser Mailbox where
    listParser = pMailboxList

instance HasParser AddrSpec where
    parser = fmap AddrSpec pAddrSpec

instance HasParser Domain where
    parser = fmap Domain pDomain

instance IsString Mailbox where
    fromString = either (error "invalid mailbox") id . parse . T.pack

------------------------------------------------------------------------------
-- Addr-Spec Specification <https://tools.ietf.org/html/rfc5322#section-3.4.1>
------------------------------------------------------------------------------

-- | Address specification
--
-- > addr-spec = local-part "@" domain
--
pAddrSpec :: Parser Mailbox
pAddrSpec = liftA2 (Mailbox "") pLocalPart ("@" *> pDomain)

-- | Local part
--
-- > local-part = dot-atom / quoted-string
--
pLocalPart :: Parser T.Text
pLocalPart = pDotAtom <|> pQuotedString

-- | Domain
--
-- > domain = dot-atom / domain-literal
--
pDomain :: Parser T.Text
pDomain = T.toLower <$> pDotAtom <|> pDomainLiteral

-- | Domain literal
--
-- > domain-literal = [CFWS] "[" *([FWS] dtext) [FWS] "]" [CFWS]
--
pDomainLiteral :: Parser T.Text
pDomainLiteral = optional pCFWS *> "[" `append` foldMany (optional pFWS `append` pDText) `append` optional pFWS `append` "]" <* optional pCFWS

-- | Domain literal text
--
-- > dtext = %d33-90 / %d94-126
--
pDText :: Parser T.Text
pDText = takeWhile1 isDText <?> "domain text"

-- | Match domain literal text
isDText :: Char -> Bool
isDText c | c == '['  = False
          | c == ']'  = False
          | c == '\\' = False
          | isVCHAR c = True
          | otherwise = False
{-# INLINE isDText #-}

--------------------------------------------------------------------------
-- Address Specification <https://tools.ietf.org/html/rfc5322#section-3.4>
--------------------------------------------------------------------------

-- | Address
--
-- > address = mailbox / group
--
pAddress :: Parser [Mailbox]
pAddress = pure <$> pMailbox <|> pGroup

-- | Mailbox
--
-- > mailbox = name-addr / addr-spec
--
pMailbox :: Parser Mailbox
pMailbox = pNameAddr <|> pAddrSpec

-- | Name address
--
-- > name-addr = [display-name] angle-addr
--
pNameAddr :: Parser Mailbox
pNameAddr = liftA2 (\d m -> m { mboxDisplay = d }) (optional pDisplayName) pAngleAddr

-- | Angle address
--
-- > angle-addr = [CFWS] "<" addr-spec ">" [CFWS]
--
pAngleAddr :: Parser Mailbox
pAngleAddr = optional pCFWS *> "<" *> pAddrSpec <* ">" <* optional pCFWS

-- | Group
--
-- > group = display-name ":" [group-list] ";" [CFWS]
--
pGroup :: Parser [Mailbox]
pGroup = pDisplayName *> ":" *> optional pGroupList <* ";" <* optional pCFWS

-- | Display name
--
-- > display-name = phrase
--
pDisplayName :: Parser T.Text
pDisplayName = pPhrase

-- | Mailbox list
--
-- > mailbox-list = (mailbox *("," mailbox))
--
pMailboxList :: Parser [Mailbox]
pMailboxList = pMailbox `sepBy1` ","

-- | Address list
--
-- > address-list = address *("," address)
--
pAddressList :: Parser [Mailbox]
pAddressList = concat <$> (pAddress `sepBy1` ",")

-- | Group list
--
-- > group-list = mailbox-list / CFWS
--
pGroupList :: Parser [Mailbox]
pGroupList = pMailboxList <|> ([] <$ pCFWS)
