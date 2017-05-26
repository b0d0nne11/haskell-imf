{-# LANGUAGE OverloadedStrings #-}

module Text.IMF.Mailbox
    ( Mailbox(..)
    , mailboxAddrSpec
    , parseMailbox
    , parseMailboxList
    , mailbox
    , mailboxList
    ) where

import           Data.List        (intercalate)
import qualified Data.Text        as T
import           Text.Parsec      (ParseError, ParsecT, between, eof, many,
                                   many1, option, optional, parse, sepBy1, try,
                                   (<|>))
import           Text.Parsec.Char (char, crlf, digit, letter, oneOf, satisfy,
                                   string)
import           Text.Printf      (printf)

-- | Entity for a mailbox
data Mailbox = Mailbox { mailboxDisplayName :: T.Text -- ^ Display name
                       , mailboxLocalPart   :: T.Text -- ^ Local part
                       , mailboxDomain      :: T.Text -- ^ Domain
                       }

instance Show Mailbox where
    show (Mailbox "" lp d) = printf "%v@%v" lp d
    show (Mailbox dn lp d) = printf "%v <%v@%v>" dn lp d

instance Eq Mailbox where
    (Mailbox _ lp1 d1) == (Mailbox _ lp2 d2) =
        lp1 == lp2 && T.toLower d1 == T.toLower d2

-- | Mailbox address (@foo\@example.com@)
mailboxAddrSpec :: Mailbox -> T.Text
mailboxAddrSpec (Mailbox _ lp d) = T.pack $ show $ Mailbox "" lp d

-- | Parse a mailbox
parseMailbox :: T.Text -> Either ParseError Mailbox
parseMailbox = parse' . T.unpack
  where
    parse' = parse (do{ a <- mailbox; eof; return a }) ""

-- | Parse a comma seperated list of mailboxes
parseMailboxList :: T.Text -> Either ParseError [Mailbox]
parseMailboxList = parse' . T.unpack
  where
    parse' = parse (do{ a <- mailboxList; eof; return a }) ""

-------------
-- Primitives
-------------

-- Visable ASCII
vchar :: Monad m => ParsecT String u m Char
vchar = satisfy (\c -> c >= '\x21' &&
                       c <= '\x7E' )

-- Quoted pair
quotedPair :: Monad m => ParsecT String u m String
quotedPair = do{ p1 <- char '\\'
               ; p2 <- vchar <|> wsp
               ; return $ p1:[p2]
               }

-- Whitespace
wsp :: Monad m => ParsecT String u m Char
wsp = char ' ' <|> char '\t'

-- UTF-8
utf8 :: Monad m => ParsecT String u m Char
utf8 = satisfy $ \c -> ('\x80' <= c && c <= '\x10FFFF') && not ('\xD800' <= c && c <= '\xDFFF')

---------------------------------------------------------------------------------------
-- Folding White Space and Comments <https://tools.ietf.org/html/rfc5322#section-3.2.2>
---------------------------------------------------------------------------------------

-- FWS = ([*WSP CRLF] 1*WSP) / obs-FWS
fws :: Monad m => ParsecT String u m String
fws = do{ optional $ try $ do{ _ <- many wsp; crlf }
        ; many1 wsp
        }

-- ctext = %d33-39 / %d42-91 / %d93-126 / obs-ctext
ctext :: Monad m => ParsecT String u m Char
ctext = satisfy (\c -> c >= '\x21' &&
                       c <= '\x7E' &&
                       c /= '\x28' && -- '('
                       c /= '\x29' && -- ')'
                       c /= '\x5C' )  -- '\'
    <|> utf8

-- ccontent = ctext / quoted-pair / comment
ccontent :: Monad m => ParsecT String u m String
ccontent = try (many1 ctext) <|> try quotedPair <|> comment

-- comment = "(" *([FWS] ccontent) [FWS] ")"
comment :: Monad m => ParsecT String u m String
comment = do{ _ <- string "("
            ; _ <- many $ try $ do{ optional fws; ccontent }
            ; _ <- optional fws
            ; _ <- string ")"
            ; return ""
            }

-- CFWS = (1*([FWS] comment) [FWS]) / FWS
cfws :: Monad m => ParsecT String u m String
cfws = try $ do{ ps <- many1 $ try $ do{ a <- option "" fws; _ <- comment; return a }
               ; pN <- option "" fws
               ; return $ concat $ ps ++ [pN]
               }
   <|> fws

-----------------------------------------------------------
-- Atom <https://tools.ietf.org/html/rfc5322#section-3.2.3>
-----------------------------------------------------------

-- atext =
--     ALPHA / DIGIT /
--     "!" / "#" / "$" / "%" / "&" / "'" / "*" / "+" / "-" / "/" /
--     "=" / "?" / "^" / "_" / "`" / "{" / "|" / "}" / "~"
atext :: Monad m => ParsecT String u m Char
atext = letter
    <|> digit
    <|> oneOf "!#'$%&'*+-/=?^_`{|}~"
    <|> utf8

-- atom = [CFWS] 1*atext [CFWS]
atom :: Monad m => ParsecT String u m String
atom = between (optional cfws) (optional cfws) (many1 atext)

-- dot-atom-text = 1*atext *("." 1*atext)
dotAtomText :: Monad m => ParsecT String u m String
dotAtomText = do{ p0 <- many1 atext
                ; ps <- many $ try $ do{ _ <- string "."; many1 atext }
                ; return $ intercalate "." $ p0:ps
                }

-- dot-atom = [CFWS] dot-atom-text [CFWS]
dotAtom :: Monad m => ParsecT String u m String
dotAtom = between (optional cfws) (optional cfws) dotAtomText

---------------------------------------------------------------------
-- Quoted Strings <https://tools.ietf.org/html/rfc5322#section-3.2.4>
---------------------------------------------------------------------

-- qtext = %d33 / %d35-91 / %d93-126 / obs-qtext
qtext :: Monad m => ParsecT String u m Char
qtext = satisfy (\c -> c >= '\x21' &&
                       c <= '\x7E' &&
                       c /= '\x22' && -- '"'
                       c /= '\x5C' )  -- '\'
    <|> utf8

-- qcontent = qtext / quoted-pair
qcontent :: Monad m => ParsecT String u m String
qcontent = try (many1 qtext) <|> quotedPair

-- quoted-string = [CFWS] DQUOTE *([FWS] qcontent) [FWS] DQUOTE [CFWS]
quotedString :: Monad m => ParsecT String u m String
quotedString = do{ _ <- optional cfws
                 ; _ <- string "\""
                 ; ps <- many $ try $ do{ a <- option "" fws
                                        ; b <- qcontent
                                        ; return $ a ++ b
                                        }
                 ; pN <- option "" fws
                 ; _ <- string "\""
                 ; _ <- optional cfws
                 ; return $ "\"" ++ concat (ps ++ [pN]) ++ "\""
                 }


----------------------------------------------------------------------------------
-- Miscellaneous Tokens <https://tools.ietf.org/html/rfc5322#section-3.2.5>
-- Miscellaneous Obsolete Tokens <https://tools.ietf.org/html/rfc5322#section-4.1>
----------------------------------------------------------------------------------

-- word = atom / quoted-string
word :: Monad m => ParsecT String u m String
word = try atom <|> quotedString

-- obs-phrase = word *(word / "." / CFWS)
obsPhrase :: Monad m => ParsecT String u m String
obsPhrase = do{ p1 <- word
              ; ps <- many $ try word <|> try (string ".") <|> cfws
              ; return $ unwords $ p1:ps
              }

------------------------------------------------------------------------------
-- Addr-Spec Specification <https://tools.ietf.org/html/rfc5322#section-3.4.1>
------------------------------------------------------------------------------

-- addr-spec = local-part "@" domain
addrSpec :: Monad m => ParsecT String u m Mailbox
addrSpec = do{ lp <- localPart
             ; _ <- string "@"
             ; d <- domain
             ; if length lp + 1 + length d > 254
                  then fail "address exceeds maximum length of 254 characters"
                  else return $ Mailbox "" (T.pack lp) (T.pack d)
             }

-- local-part = dot-atom / quoted-string / obs-local-part
localPart :: Monad m => ParsecT String u m String
localPart = do{ lp <- try dotAtom <|> quotedString
              ; if length lp > 64
                   then fail "local part exceeds maximum length of 64 characters"
                   else return lp
              }

-- domain = dot-atom / domain-literal / obs-domain
domain :: Monad m => ParsecT String u m String
domain = do{ d <- try dotAtom <|> domainLiteral
           ; if length d > 255
                then fail "domain exceeds maximum length of 255 characters"
                else return d
           }

-- domain-literal = [CFWS] "[" *([FWS] dtext) [FWS] "]" [CFWS]
domainLiteral :: Monad m => ParsecT String u m String
domainLiteral = do{ _ <- optional cfws
                  ; _ <- string "["
                  ; ps <- many $ try $ do{ a <- option "" fws
                                         ; b <- many1 dtext
                                         ; return $ a ++ b
                                         }
                  ; pN <- option "" fws
                  ; _ <- string "]"
                  ; _ <- optional cfws
                  ; return $ "[" ++ concat (ps ++ [pN]) ++ "]"
                  }

-- dtext = %d33-90 / %d94-126 / obs-dtext
dtext :: Monad m => ParsecT String u m Char
dtext = satisfy (\c -> c >= '\x21' &&
                       c <= '\x7E' &&
                       c /= '\x5B' && -- '['
                       c /= '\x5D' && -- ']'
                       c /= '\x5C' )  -- '\'
    <|> utf8

--------------------------------------------------------------------------
-- Address Specification <https://tools.ietf.org/html/rfc5322#section-3.4>
--------------------------------------------------------------------------

-- mailbox = name-addr / addr-spec
-- | Mailbox parser
mailbox :: Monad m => ParsecT String u m Mailbox
mailbox = try nameAddr <|> addrSpec

-- name-addr = [display-name] angle-addr
nameAddr :: Monad m => ParsecT String u m Mailbox
nameAddr = do{ dn <- option "" $ try displayName
             ; (Mailbox _ lp d) <- angleAddr
             ; return $ Mailbox (T.strip . T.pack $ dn) lp d
             }

-- angle-addr = [CFWS] "<" addr-spec ">" [CFWS] / obs-angle-addr
angleAddr :: Monad m => ParsecT String u m Mailbox
angleAddr = do{ _ <- optional cfws
              ; _ <- string "<"
              ; mbox <- addrSpec
              ; _ <- string ">"
              ; _ <- optional cfws
              ; return mbox
              }

-- display-name = phrase
displayName :: Monad m => ParsecT String u m String
displayName = obsPhrase

-- mailbox-list = (mailbox *("," mailbox)) / obs-mbox-list
-- | Mailbox list parser
mailboxList :: Monad m => ParsecT String u m [Mailbox]
mailboxList = sepBy1 mailbox $ string ","
