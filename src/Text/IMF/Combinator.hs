{-# LANGUAGE OverloadedStrings #-}

module Text.IMF.Combinator
    ( collect
    , collectMany
    , range
    , strings
    ) where

import           Text.Parsec      (ParsecT, choice, count, many, try)
import           Text.Parsec.Char (string)

-- | Collect the results of a list of parsers with @concat@.
collect :: Monad m => [ParsecT String u m String] -> ParsecT String u m String
collect ps = concat <$> sequence ps

-- | Collect the results of a repeating a list of parsers with @concat@.
collectMany :: Monad m => [ParsecT String u m String] -> ParsecT String u m String
collectMany ps = concat <$> many (try (collect ps))

-- | @range ns p@ applies @count n p@ for each value of @ns@ in order and returns the first match.
range :: Monad m => [Int] -> ParsecT String u m Char -> ParsecT String u m String
range ns p = choice $ map (\n -> try $ count n p) ns

-- | @strings ss@ applies @string s@ to each value of @ss@ in order and returns the first match.
strings :: Monad m => [String] -> ParsecT String u m String
strings = choice . map (try . string)
