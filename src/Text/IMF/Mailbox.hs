{-# LANGUAGE OverloadedStrings #-}

module Text.IMF.Mailbox
    ( Mailbox(..)
    , mailbox
    , mailboxList
    , address
    , addressList
    , dtext
    ) where

import           Control.Applicative (liftA2)
import           Data.Char           (toLower)
import           Text.Parsec         (ParsecT, many1, option, optional, sepBy1,
                                      try, (<|>))
import           Text.Parsec.Char    (oneOf, string)

import           Text.IMF.Combinator
import           Text.IMF.Format
import           Text.IMF.Primatives

data Mailbox = Mailbox { addrDisplay :: Maybe String -- ^ Display name
                       , addrLocal   :: String       -- ^ Local part
                       , addrDomain  :: String       -- ^ Domain part
                       }
  deriving (Show, Eq)

instance ParseMessage Mailbox where
  msgParser = mailbox

instance FormatMessage Mailbox where
  formatMessage (Mailbox Nothing lp d) = concat [lp, "@", d]
  formatMessage (Mailbox (Just dn) lp d) = concat [dn, " <", lp, "@", d, ">"]

------------------------------------------------------------------------------
-- Addr-Spec Specification <https://tools.ietf.org/html/rfc5322#section-3.4.1>
------------------------------------------------------------------------------

-- addr-spec = local-part "@" domain
addrSpec :: Monad m => ParsecT String u m Mailbox
addrSpec = liftA2 (Mailbox Nothing) localPart (string "@" *> domain)

-- local-part = dot-atom / quoted-string
localPart :: Monad m => ParsecT String u m String
localPart = dotAtom <|> quotedString

-- domain = dot-atom / domain-literal
domain :: Monad m => ParsecT String u m String
domain = map toLower <$> (try dotAtom <|> domainLiteral)

-- domain-literal = [CFWS] "[" *([FWS] dtext) [FWS] "]" [CFWS]
domainLiteral :: Monad m => ParsecT String u m String
domainLiteral = optional cfws *> collect [string "[", collectMany [option "" fws, many1 dtext], option "" fws, string "]"] <* optional cfws

-- | Domain literal text. Exported for use in message IDs.
--
-- > dtext = %d33-90 / %d94-126
--
dtext :: Monad m => ParsecT String u m Char
dtext = oneOf $ ['\33'..'\90'] ++ ['\94'..'\126']

--------------------------------------------------------------------------
-- Address Specification <https://tools.ietf.org/html/rfc5322#section-3.4>
--------------------------------------------------------------------------

-- address = mailbox / group
-- | Parser for address. This returns a list of mailboxes because groups are valid here. If you want an individual mailbox use @mailbox@ instead.
address :: Monad m => ParsecT String u m [Mailbox]
address = try (pure <$> mailbox) <|> group

-- mailbox = name-addr / addr-spec
-- | Parser for mailbox.
mailbox :: Monad m => ParsecT String u m Mailbox
mailbox = try nameAddr <|> addrSpec

-- name-addr = [display-name] angle-addr
nameAddr :: Monad m => ParsecT String u m Mailbox
nameAddr = liftA2 (\dn addr -> addr { addrDisplay = dn }) (option Nothing $ Just <$> try displayName) angleAddr

-- angle-addr = [CFWS] "<" addr-spec ">" [CFWS]
angleAddr :: Monad m => ParsecT String u m Mailbox
angleAddr = optional cfws *> string "<" *> addrSpec <* string ">" <* optional cfws

-- group = display-name ":" [group-list] ";" [CFWS]
group :: Monad m => ParsecT String u m [Mailbox]
group = displayName *> string ":" *> option [] groupList <* string ";" <* optional cfws

-- display-name = phrase
displayName :: Monad m => ParsecT String u m String
displayName = phrase

-- mailbox-list = (mailbox *("," mailbox))
-- | Parser for mailbox list.
mailboxList :: Monad m => ParsecT String u m [Mailbox]
mailboxList = mailbox `sepBy1` string ","

-- address-list = address *("," address)
-- | Parser for address list. May include groups.
addressList :: Monad m => ParsecT String u m [Mailbox]
addressList = concat <$> (address `sepBy1` string ",")

-- group-list = mailbox-list / CFWS
groupList :: Monad m => ParsecT String u m [Mailbox]
groupList = try mailboxList <|> ([] <$ cfws)
