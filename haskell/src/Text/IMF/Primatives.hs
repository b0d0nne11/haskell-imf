{-# LANGUAGE OverloadedStrings #-}

module Text.IMF.Primatives
  ( -- * Charater Primitives
    vchar
  , quotedPair
  , wsp
  , eol
  , isVisible
  , isSpace
    -- * Folding White Space and Comments
  , pFWS
  , pCFWS
    -- * Atoms
  , pAtom
  , pDotAtomText
  , pDotAtom
    -- * Quoted Strings
  , pQuotedString
  , smartQuote
    -- * Miscellaneous Tokens
  , pPhrase
  , pUnstructured
  )
where


import           Data.Char                      ( isAlpha
                                                , isDigit
                                                , isMark
                                                , isPunctuation
                                                , isSymbol
                                                )
import           Data.Foldable                  ( fold )
import           Text.Parsec                    ( count
                                                , many
                                                , many1
                                                , optional
                                                , try
                                                , (<|>)
                                                , (<?>)
                                                , satisfy
                                                , parse
                                                , eof
                                                )
import           Text.Parsec.Char               ( string )

import           Text.IMF.Format

-------------
-- Primitives
-------------

-- | Printable characters
--
-- > VCHAR = %x21-7E
--
vchar :: Parser Char
vchar = satisfy isVisible <?> "visible character"

-- | Quoted pair
--
-- > "\" (VCHAR / WSP)
--
quotedPair :: Parser String
quotedPair = fold [string "\\", count 1 vchar <|> count 1 wsp]

-- | Whitespace characters
--
-- > WSP  = SP / HTAB
-- > SP   = %x20
-- > HTAB = %x09
--
wsp :: Parser Char
wsp = satisfy isSpace <?> "whitespace"

-- | End of line
eol :: Parser String
eol = try $ string "\r\n" <|> string "\n"

-- | Match visible characters
isVisible :: Char -> Bool
isVisible c | isAlpha c       = True
            | isDigit c       = True
            | isMark c        = True
            | isPunctuation c = True
            | isSymbol c      = True
            | otherwise       = False
{-# INLINE isVisible #-}

-- | Match whitespace characters
isSpace :: Char -> Bool
isSpace c | c == ' '  = True
          | c == '\t' = True
          | otherwise = False
{-# INLINE isSpace #-}

---------------------------------------------------------------------------------------
-- Folding White Space and Comments <https://tools.ietf.org/html/rfc5322#section-3.2.2>
---------------------------------------------------------------------------------------

-- | Folding white space
--
-- > FWS = [*WSP CRLF] 1*WSP
--
pFWS :: Parser String
pFWS = (" " :: String) <$ try (many wsp *> eol *> many1 wsp) <|> many1 wsp

-- | Printable characters not including "(", ")", or "\"
--
-- > ctext = %d33-39 / %d42-91 / %d93-126
--
pCText :: Parser String
pCText = many1 (satisfy isCommentText) <?> "comment text"

-- | Comment content
--
-- > ccontent = ctext / quoted-pair / comment
--
pCContent :: Parser String
pCContent = pCText <|> quotedPair <|> pComment

-- | Comment
--
-- > comment = "(" *([FWS] ccontent) [FWS] ")"
--
pComment :: Parser String
pComment = ("" :: String) <$ string "(" *> concatMany (pCContent <|> pFWS) <* string ")"

-- | Comment or folding white space
--
-- > CFWS     = (1*([FWS] comment) [FWS]) / FWS
-- > comment  = "(" *([FWS] ccontent) [FWS] ")"
-- > ccontent = ctext / quoted-pair / comment
-- > ctext    = %d33-39 / %d42-91 / %d93-126
--
pCFWS :: Parser String
pCFWS = (" " :: String) <$ try (many1 $ try (optional pFWS *> pComment) <* optional pFWS) <|> pFWS

-- | Match comment text
isCommentText :: Char -> Bool
isCommentText c | c == '('    = False
                | c == ')'    = False
                | c == '\\'   = False
                | isVisible c = True
                | otherwise   = False
{-# INLINE isCommentText #-}

-----------------------------------------------------------
-- Atom <https://tools.ietf.org/html/rfc5322#section-3.2.3>
-----------------------------------------------------------

-- | Printable characters not including specials.
--
-- > atext = ALPHA / DIGIT /
-- >         "!" / "#" / "$" / "%" /
-- >         "&" / "'" / "*" / "+" /
-- >         "-" / "/" / "=" / "?" /
-- >         "^" / "_" / "`" / "{" /
-- >         "|" / "}" / "~"
--
pAText :: Parser String
pAText = many1 (satisfy isAtomText) <?> "atom text"

-- | Atom
--
-- > atom  = [CFWS] 1*atext [CFWS]
-- > atext = ALPHA / DIGIT /
-- >         "!" / "#" / "$" / "%" /
-- >         "&" / "'" / "*" / "+" /
-- >         "-" / "/" / "=" / "?" /
-- >         "^" / "_" / "`" / "{" /
-- >         "|" / "}" / "~"
--
pAtom :: Parser String
pAtom = optional pCFWS *> pAText <* optional pCFWS

-- | Dot-atom text
--
-- > dot-atom-text = 1*atext *("." 1*atext)
-- > atext         = ALPHA / DIGIT /
-- >                 "!" / "#" / "$" / "%" /
-- >                 "&" / "'" / "*" / "+" /
-- >                 "-" / "/" / "=" / "?" /
-- >                 "^" / "_" / "`" / "{" /
-- >                 "|" / "}" / "~"
--
pDotAtomText :: Parser String
pDotAtomText = fold [pAText, concatMany $ fold [string ".", pAText]]

-- | Dot-atom
--
-- > dot-atom = [CFWS] dot-atom-text [CFWS]
--
pDotAtom :: Parser String
pDotAtom = optional pCFWS *> pDotAtomText <* optional pCFWS

-- | Match atom text
isAtomText :: Char -> Bool
isAtomText c | isAlpha c = True
             | isDigit c = True
             | c == '!'  = True
             | c == '#'  = True
             | c == '$'  = True
             | c == '%'  = True
             | c == '&'  = True
             | c == '\'' = True
             | c == '*'  = True
             | c == '+'  = True
             | c == '-'  = True
             | c == '/'  = True
             | c == '='  = True
             | c == '?'  = True
             | c == '^'  = True
             | c == '_'  = True
             | c == '`'  = True
             | c == '{'  = True
             | c == '|'  = True
             | c == '}'  = True
             | c == '~'  = True
             | otherwise = False
{-# INLINE isAtomText #-}

---------------------------------------------------------------------
-- Quoted Strings <https://tools.ietf.org/html/rfc5322#section-3.2.4>
---------------------------------------------------------------------

-- | Printable characters not including "\" or the quote character.
--
-- > qtext = %d33 / %d35-91 / %d93-126
--
pQText :: Parser String
pQText = many1 (satisfy isQuotedText) <?> "quoted text"

-- | Quoted string content
--
-- > qcontent = qtext / quoted-pair
--
pQContent :: Parser String
pQContent = pQText <|> quotedPair

-- | Quoted string
--
-- > quoted-string = [CFWS] DQUOTE *([FWS] qcontent) [FWS] DQUOTE [CFWS]
-- > qcontent      = qtext / quoted-pair
-- > qtext         = %d33 / %d35-91 / %d93-126
--
pQuotedString :: Parser String
pQuotedString = optional pCFWS *> fold [string "\"", concatMany (pQContent <|> pFWS), string "\""] <* optional pCFWS

-- | Match quoted string text
isQuotedText :: Char -> Bool
isQuotedText c | c == '"'    = False
               | c == '\\'   = False
               | isVisible c = True
               | otherwise   = False
{-# INLINE isQuotedText #-}

-- | WIP: Wrap a string in quotes only if it needs quoting
smartQuote :: String -> String
smartQuote input =
    case parse (pDotAtom <* eof) "" input of
        Left  _ -> "\"" ++ escape input ++ "\""
        Right _ -> input
  where
    escape []       = []
    escape (c : cs) = case c of
        '"'  -> "\\\"" ++ escape cs
        '\\' -> "\\\\" ++ escape cs
        _    -> c : escape cs

---------------------------------------------------------------------------
-- Miscellaneous Tokens <https://tools.ietf.org/html/rfc5322#section-3.2.5>
---------------------------------------------------------------------------

-- | Word
--
-- > word = atom / quoted-string
--
pWord :: Parser String
pWord = try pAtom <|> pQuotedString

-- | Phrase
--
-- > phrase = 1*word
-- > word   = atom / quoted-string
--
pPhrase :: Parser String
pPhrase = unwords <$> many1 pWord

-- | Unstructured
--
-- > unstructured = *([FWS] VCHAR) *WSP
--
pUnstructured :: Parser String
pUnstructured = optional pFWS *> concatMany (many1 vchar <|> pFWS) <* many wsp
