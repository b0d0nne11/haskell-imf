{-# LANGUAGE OverloadedStrings #-}

module Text.IMF.Primatives
    ( -- * Charater Primitives
      vchar
    , quotedPair
    , wsp
    , utf8
      -- * Folding White Space and Comments
    , fws
    , ctext
    , ccontent
    , comment
    , cfws
      -- * Atoms
    , atext
    , atom
    , dotAtomText
    , dotAtom
      -- * Quoted Strings
    , qtext
    , qcontent
    , quotedString
      -- * Miscellaneous Tokens
    , word
    , phrase
    , unstructured
    ) where


import           Data.List           (intercalate)
import           Text.Parsec         (ParsecT, count, many, many1, option,
                                      optional, sepBy1, try, (<|>))
import           Text.Parsec.Char    (char, crlf, digit, letter, oneOf, satisfy,
                                      string)

import           Text.IMF.Combinator

-------------
-- Primitives
-------------

-- | Printable US-ASCII characters
--
-- > VCHAR = %x21-7E
--
vchar :: Monad m => ParsecT String u m Char
vchar = oneOf ['\x21'..'\x7e']

-- | Quoted pair
--
-- > "\" (VCHAR / WSP)
--
quotedPair :: Monad m => ParsecT String u m String
quotedPair = collect [string "\\", count 1 vchar <|> count 1 wsp]

-- | Whitespace
--
-- > WSP  = SP / HTAB
-- > SP   = %x20
-- > HTAB = %x09
--
wsp :: Monad m => ParsecT String u m Char
wsp = char '\x20' <|> char '\x09'

-- | UTF-8
utf8 :: Monad m => ParsecT String u m Char
utf8 = satisfy $ \c -> ('\x80' <= c && c <= '\x10FFFF') && not ('\xD800' <= c && c <= '\xDFFF')

---------------------------------------------------------------------------------------
-- Folding White Space and Comments <https://tools.ietf.org/html/rfc5322#section-3.2.2>
---------------------------------------------------------------------------------------

-- | Folding white space
--
-- > FWS = [*WSP CRLF] 1*WSP
--
fws :: Monad m => ParsecT String u m String
fws = (" " :: String) <$ try (many wsp *> crlf *> many1 wsp) <|> many1 wsp

-- | Printable US-ASCII characters not including "(", ")", or "\"
--
-- > ctext = %d33-39 / %d42-91 / %d93-126
--
ctext :: Monad m => ParsecT String u m Char
ctext = oneOf $ ['\33'..'\39'] ++ ['\42'..'\91'] ++ ['\93'..'\126']

-- | Comment content
--
-- > ccontent = ctext / quoted-pair / comment
--
ccontent :: Monad m => ParsecT String u m String
ccontent = count 1 ctext <|> quotedPair <|> comment

-- | Comment
--
-- > comment = "(" *([FWS] ccontent) [FWS] ")"
--
comment :: Monad m => ParsecT String u m String
comment = ("" :: String) <$ string "(" *> many (try (option "" fws *> ccontent)) *> option "" fws *> string ")"

-- | Comment or folding white space
--
-- > CFWS = (1*([FWS] comment) [FWS]) / FWS
--
cfws :: Monad m => ParsecT String u m String
cfws = (" " :: String) <$ try (many1 (try (option "" fws *> comment)) *> option "" fws) <|> fws

-----------------------------------------------------------
-- Atom <https://tools.ietf.org/html/rfc5322#section-3.2.3>
-----------------------------------------------------------

-- | Printable US-ASCII characters not including specials.
--
-- > atext =
-- >     ALPHA / DIGIT /
-- >     "!" / "#" / "$" / "%" / "&" / "'" / "*" / "+" / "-" / "/" /
-- >     "=" / "?" / "^" / "_" / "`" / "{" / "|" / "}" / "~"
--
atext :: Monad m => ParsecT String u m Char
atext = letter <|> digit <|> oneOf "!#'$%&'*+-/=?^_`{|}~"

-- | Atom
--
-- > atom = [CFWS] 1*atext [CFWS]
--
atom :: Monad m => ParsecT String u m String
atom = optional cfws *> many1 atext <* optional cfws

-- | Dot-atom text
--
-- > dot-atom-text = 1*atext *("." 1*atext)
--
dotAtomText :: Monad m => ParsecT String u m String
dotAtomText = intercalate "." <$> (many1 atext `sepBy1` string ".")

-- | Dot-atom
--
-- > dot-atom = [CFWS] dot-atom-text [CFWS]
--
dotAtom :: Monad m => ParsecT String u m String
dotAtom = optional cfws *> dotAtomText <* optional cfws

---------------------------------------------------------------------
-- Quoted Strings <https://tools.ietf.org/html/rfc5322#section-3.2.4>
---------------------------------------------------------------------

-- | Printable US-ASCII characters not including "\" or the quote character.
--
-- > qtext = %d33 / %d35-91 / %d93-126
--
qtext :: Monad m => ParsecT String u m Char
qtext = oneOf $ ['\33'] ++ ['\35'..'\91'] ++ ['\93'..'\126']

-- | Quoted string content
--
-- > qcontent = qtext / quoted-pair
--
qcontent :: Monad m => ParsecT String u m String
qcontent = count 1 qtext <|> quotedPair

-- | Quoted string
--
-- > quoted-string = [CFWS] DQUOTE *([FWS] qcontent) [FWS] DQUOTE [CFWS]
--
quotedString :: Monad m => ParsecT String u m String
quotedString = optional cfws *> collect [string "\"", collectMany [option "" fws, qcontent], option "" fws, string "\""] <* optional cfws

---------------------------------------------------------------------------
-- Miscellaneous Tokens <https://tools.ietf.org/html/rfc5322#section-3.2.5>
---------------------------------------------------------------------------

-- | Word
--
-- > word = atom / quoted-string
--
word :: Monad m => ParsecT String u m String
word = try atom <|> quotedString

-- | Phrase
--
-- > phrase = 1*word
--
phrase :: Monad m => ParsecT String u m String
phrase = unwords <$> many1 (try word)

-- | Unstructured
--
-- > unstructured = *([FWS] VCHAR) *WSP
--
unstructured :: Monad m => ParsecT String u m String
unstructured = optional fws *> collectMany [option "" fws, count 1 vchar] <* many wsp
