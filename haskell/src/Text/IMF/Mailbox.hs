{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Text.IMF.Mailbox
  ( Mailbox(..)
  , Display
  , Local
  , Domain
  , validateDomain
  , pMailbox
  , pMailboxList
  , pAddress
  , pAddressList
  , pDText
  )
where

import           Control.Applicative            ( liftA2 )
import           Data.Char                      ( toLower )
import           Data.Foldable                  ( fold )
import           Data.List                      ( intercalate )
import           Text.Parsec                    ( many1
                                                , option
                                                , optional
                                                , sepBy1
                                                , try
                                                , (<|>)
                                                , (<?>)
                                                , parse
                                                , sepBy1
                                                , eof
                                                )
import           Text.Parsec.Char               ( satisfy
                                                , string
                                                )

import           Text.IMF.Format
import           Text.IMF.Primatives

type Display = String

type Local = String

type Domain = String

-- | Validate the normal (dot-atom) form of a domain
validateDomain :: Domain -> Bool
validateDomain = either (const False) (const True) . parse (pDotAtom <* eof) ""

data Mailbox = Mailbox Display Local Domain
  deriving (Show)

instance Eq Mailbox where
    (Mailbox _ l1 d1) == (Mailbox _ l2 d2) = l1 == l2 && map toLower d1 == map toLower d2

instance ParseMessage Mailbox where
    parseMessage = parse (pMailbox <* eof) ""

instance FormatMessage Mailbox where
    formatMessage (Mailbox "" l d) = l ++ "@" ++ d
    formatMessage (Mailbox dn l d) = dn ++ " <" ++ l ++ "@" ++ d ++ ">"

instance ParseMessage [Mailbox] where
    parseMessage = parse (pMailbox `sepBy1` string "," <* eof) ""

instance FormatMessage [Mailbox] where
    formatMessage = intercalate ", " . map formatMessage

------------------------------------------------------------------------------
-- Addr-Spec Specification <https://tools.ietf.org/html/rfc5322#section-3.4.1>
------------------------------------------------------------------------------

-- | Address specification
--
-- > addr-spec = local-part "@" domain
--
pAddrSpec :: Parser Mailbox
pAddrSpec = liftA2 (Mailbox "") pLocalPart (string "@" *> pDomain)

-- | Local part
--
-- > local-part = dot-atom / quoted-string
--
pLocalPart :: Parser String
pLocalPart = try pDotAtom <|> pQuotedString

-- | Domain
--
-- > domain = dot-atom / domain-literal
--
pDomain :: Parser String
pDomain = try pDotAtom <|> pDomainLiteral

-- | Domain literal
--
-- > domain-literal = [CFWS] "[" *([FWS] dtext) [FWS] "]" [CFWS]
--
pDomainLiteral :: Parser String
pDomainLiteral = optional pCFWS *> fold [string "[", concatMany (pDText <|> pFWS), string "]"] <* optional pCFWS

-- | Domain literal text
--
-- > dtext = %d33-90 / %d94-126
--
pDText :: Parser String
pDText = many1 (satisfy isDomainText) <?> "domain text"

-- | Match domain literal text
isDomainText :: Char -> Bool
isDomainText c | c == '['    = False
               | c == ']'    = False
               | c == '\\'   = False
               | isVisible c = True
               | otherwise   = False
{-# INLINE isDomainText #-}

--------------------------------------------------------------------------
-- Address Specification <https://tools.ietf.org/html/rfc5322#section-3.4>
--------------------------------------------------------------------------

-- | Address
--
-- > address    = mailbox / group
-- > group      = display-name ":" [group-list] ";" [CFWS]
-- > group-list = mailbox-list / CFWS
--
-- Note that this parser returns a list of mailboxes because groups are valid
-- addresses. To parse an individual mailbox use pMailbox instead.
--
pAddress :: Parser [Mailbox]
pAddress = try (pure <$> pMailbox) <|> pGroup

-- | Mailbox
--
-- > mailbox        = name-addr / addr-spec
-- > name-addr      = [display-name] angle-addr
-- > angle-addr     = [CFWS] "<" addr-spec ">" [CFWS]
-- > addr-spec      = local-part "@" domain
-- > display-name   = phrase
-- > local-part     = dot-atom / quoted-string
-- > domain         = dot-atom / domain-literal
-- > domain-literal = [CFWS] "[" *([FWS] dtext) [FWS] "]" [CFWS]
--
pMailbox :: Parser Mailbox
pMailbox = try pNameAddr <|> pAddrSpec

-- | Name address
--
-- > name-addr = [display-name] angle-addr
--
pNameAddr :: Parser Mailbox
pNameAddr = liftA2 mailbox' (option "" $ try pDisplayName) pAngleAddr
    where mailbox' dn (Mailbox _ l d) = Mailbox dn l d

-- | Angle address
--
-- > angle-addr = [CFWS] "<" addr-spec ">" [CFWS]
--
pAngleAddr :: Parser Mailbox
pAngleAddr = optional pCFWS *> string "<" *> pAddrSpec <* string ">" <* optional pCFWS

-- | Group
--
-- > group = display-name ":" [group-list] ";" [CFWS]
--
pGroup :: Parser [Mailbox]
pGroup = pDisplayName *> string ":" *> option [] pGroupList <* string ";" <* optional pCFWS

-- | Display name
--
-- > display-name = phrase
--
pDisplayName :: Parser String
pDisplayName = pPhrase

-- | Mailbox list
--
-- > mailbox-list = (mailbox *("," mailbox))
--
pMailboxList :: Parser [Mailbox]
pMailboxList = pMailbox `sepBy1` string ","

-- | Address list
--
-- > address-list = address *("," address)
--
pAddressList :: Parser [Mailbox]
pAddressList = concat <$> (pAddress `sepBy1` string ",")

-- | Group list
--
-- > group-list = mailbox-list / CFWS
--
pGroupList :: Parser [Mailbox]
pGroupList = try pMailboxList <|> ([] <$ pCFWS)
