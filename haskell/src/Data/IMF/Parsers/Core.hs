{-# LANGUAGE OverloadedStrings #-}

module Data.IMF.Parsers.Core
  ( -- * Combinators
    foldMany
  , foldMany1
  , append
  , optional
    -- * Core Rules
  , pVCHAR
  , pQuotedPair
  , pWSP
  , pEOL
  , isVCHAR
  , isAnyChar
    -- * Folding Whitespace and Comments
  , pFWS
  , pComment
  , pCFWS
    -- * Atoms
  , pAtom
  , pDotAtomText
  , pDotAtom
    -- * Quoted Strings
  , pQuotedString
    -- * Miscellaneous Tokens
  , pPhrase
  , pUnstructured
  )
where

import           Control.Applicative            ( (<|>)
                                                , many
                                                , liftA2
                                                )
import           Data.Attoparsec.Text           ( Parser
                                                , (<?>)
                                                , many1
                                                , option
                                                , satisfy
                                                , takeWhile1
                                                )
import           Data.Char                      ( isAlphaNum
                                                , isMark
                                                , isPunctuation
                                                , isSymbol
                                                )
import           Data.Foldable                  ( fold )
import qualified Data.Text                     as T

foldMany :: Monoid a => Parser a -> Parser a
foldMany = (fold <$>) . many

foldMany1 :: Monoid a => Parser a -> Parser a
foldMany1 = (fold <$>) . many1

append :: Semigroup a => Parser a -> Parser a -> Parser a
append = liftA2 (<>)

optional :: Monoid a => Parser a -> Parser a
optional = option mempty

----------------------------------------------------------------
-- Core Rules <https://tools.ietf.org/html/rfc5234#appendix-B.1>
----------------------------------------------------------------

-- | Visiable characters
--
-- > VCHAR = %x21-7E
--
pVCHAR :: Parser T.Text
pVCHAR = takeWhile1 isVCHAR <?> "visible character"

-- | Quoted pair
--
-- > "\" (VCHAR / WSP)
--
pQuotedPair :: Parser T.Text
pQuotedPair = "\\" `append` (T.singleton <$> satisfy isAnyChar) <?> "quoted pair"

-- | Whitespace characters
--
-- > WSP  = SP / HTAB
-- > SP   = %x20
-- > HTAB = %x09
--
pWSP :: Parser T.Text
pWSP = takeWhile1 isWSP <?> "whitespace"

-- | End of line
pEOL :: Parser T.Text
pEOL = "\r\n" <?> "end of line"

-- | Match visible characters
isVCHAR :: Char -> Bool
isVCHAR c | isAlphaNum c    = True
          | isMark c        = True
          | isPunctuation c = True
          | isSymbol c      = True
          | otherwise       = False
{-# INLINE isVCHAR #-}

-- | Match whitespace characters
isWSP :: Char -> Bool
isWSP c | c == ' '  = True
        | c == '\t' = True
        | otherwise = False
{-# INLINE isWSP #-}

-- | Match any characters
isAnyChar :: Char -> Bool
isAnyChar c = isVCHAR c || isWSP c
{-# INLINE isAnyChar #-}

---------------------------------------------------------------------------------------
-- Folding Whitespace and Comments <https://tools.ietf.org/html/rfc5322#section-3.2.2>
---------------------------------------------------------------------------------------

-- | Folding whitespace
--
-- > FWS = [*WSP CRLF] 1*WSP
--
pFWS :: Parser T.Text
pFWS = optional (optional pWSP `append` pEOL) `append` pWSP

-- | Comment text
--
-- > Printable characters not including "(", ")", or "\"
-- > ctext = %d33-39 / %d42-91 / %d93-126
--
pCText :: Parser T.Text
pCText = takeWhile1 isCText <?> "comment text"

-- | Comment content
--
-- > ccontent = ctext / quoted-pair / comment
--
pCContent :: Parser T.Text
pCContent = pCText <|> pQuotedPair <|> pComment

-- | Comment
--
-- > comment = "(" *([FWS] ccontent) [FWS] ")"
--
pComment :: Parser T.Text
pComment = "(" `append` foldMany (optional pFWS `append` pCContent) `append` optional pFWS `append` ")"

-- | Comment or folding whitespace
--
-- > CFWS = (1*([FWS] comment) [FWS]) / FWS
--
pCFWS :: Parser T.Text
pCFWS = foldMany1 (optional pFWS `append` pComment) `append` optional pFWS <|> pFWS

-- | Match comment text
isCText :: Char -> Bool
isCText c | c == '('  = False
          | c == ')'  = False
          | c == '\\' = False
          | isVCHAR c = True
          | otherwise = False
{-# INLINE isCText #-}

-----------------------------------------------------------
-- Atom <https://tools.ietf.org/html/rfc5322#section-3.2.3>
-----------------------------------------------------------

-- | Atom text
--
-- > Printable characters not including specials.
-- > atext = ALPHA / DIGIT /
-- >         "!" / "#" / "$" / "%" /
-- >         "&" / "'" / "*" / "+" /
-- >         "-" / "/" / "=" / "?" /
-- >         "^" / "_" / "`" / "{" /
-- >         "|" / "}" / "~"
--
pAText :: Parser T.Text
pAText = takeWhile1 isAText <?> "atom text"

-- | Atom
--
-- > atom  = [CFWS] 1*atext [CFWS]
--
pAtom :: Parser T.Text
pAtom = optional pCFWS *> pAText <* optional pCFWS

-- | Dot-atom text
--
-- > dot-atom-text = 1*atext *("." 1*atext)
--
pDotAtomText :: Parser T.Text
pDotAtomText = pAText `append` foldMany ("." `append` pAText)

-- | Dot-atom
--
-- > dot-atom = [CFWS] dot-atom-text [CFWS]
--
pDotAtom :: Parser T.Text
pDotAtom = optional pCFWS *> pDotAtomText <* optional pCFWS

-- | Match atom text
isAText :: Char -> Bool
isAText c | isAlphaNum c = True
          | c == '!'     = True
          | c == '#'     = True
          | c == '$'     = True
          | c == '%'     = True
          | c == '&'     = True
          | c == '\''    = True
          | c == '*'     = True
          | c == '+'     = True
          | c == '-'     = True
          | c == '/'     = True
          | c == '='     = True
          | c == '?'     = True
          | c == '^'     = True
          | c == '_'     = True
          | c == '`'     = True
          | c == '{'     = True
          | c == '|'     = True
          | c == '}'     = True
          | c == '~'     = True
          | otherwise    = False
{-# INLINE isAText #-}

---------------------------------------------------------------------
-- Quoted Strings <https://tools.ietf.org/html/rfc5322#section-3.2.4>
---------------------------------------------------------------------

-- | Quoted text
--
-- > Printable characters not including "\" or the quote character.
-- > qtext = %d33 / %d35-91 / %d93-126
--
pQText :: Parser T.Text
pQText = takeWhile1 isQText <?> "quoted text"

-- | Quoted string content
--
-- > qcontent = qtext / quoted-pair
--
pQContent :: Parser T.Text
pQContent = pQText <|> pQuotedPair

-- | Quoted string
--
-- > quoted-string = [CFWS] DQUOTE *([FWS] qcontent) [FWS] DQUOTE [CFWS]
--
pQuotedString :: Parser T.Text
pQuotedString = optional pCFWS *> "\"" `append` foldMany (optional pFWS `append` pQContent) `append` optional pFWS `append` "\"" <* optional pCFWS

-- | Match quoted string text
isQText :: Char -> Bool
isQText c | c == '"'  = False
          | c == '\\' = False
          | isVCHAR c = True
          | otherwise = False
{-# INLINE isQText #-}

---------------------------------------------------------------------------
-- Miscellaneous Tokens <https://tools.ietf.org/html/rfc5322#section-3.2.5>
---------------------------------------------------------------------------

-- | Word
--
-- > word = atom / quoted-string
--
pWord :: Parser T.Text
pWord = pAtom <|> pQuotedString

-- | Phrase
--
-- > phrase = 1*word
--
pPhrase :: Parser T.Text
pPhrase = T.unwords <$> many1 pWord

-- | Unstructured
--
-- > unstructured = *([FWS] VCHAR) *WSP
--
pUnstructured :: Parser T.Text
pUnstructured = foldMany (optional pFWS `append` pVCHAR) `append` optional pWSP

