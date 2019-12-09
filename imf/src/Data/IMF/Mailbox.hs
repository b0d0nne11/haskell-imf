{-# LANGUAGE OverloadedStrings #-}

module Data.IMF.Mailbox
  ( Mailbox(..)
  , Domain(..)
  , AddrSpec(..)
  )
where

import           Data.Text      (Text)
import qualified Data.Text      as T

import           Data.IMF.Types

data Mailbox = Mailbox
    { mboxDisplay :: Text
    , mboxLocal   :: Text
    , mboxDomain  :: Text
    }
  deriving (Show)

instance Eq Mailbox where
    (Mailbox _ l1 d1) == (Mailbox _ l2 d2) = d1 == d2 && l1 == l2

instance Ord Mailbox where
    compare (Mailbox _ l1 d1) (Mailbox _ l2 d2) = compare d1 d2 <> compare l1 l2

instance HasFormatter Mailbox where
    format (Mailbox "" local domain)      = local <> "@" <> domain
    format (Mailbox display local domain) = display <> " <" <> local <> "@" <> domain <> ">"

instance HasListFormatter Mailbox where
    formatList = T.intercalate ", " . map format

newtype AddrSpec = AddrSpec Mailbox
  deriving (Show, Eq, Ord)

instance HasFormatter AddrSpec where
    format (AddrSpec (Mailbox _ local domain)) = local <> "@" <> domain

instance HasListFormatter AddrSpec where
    formatList = T.intercalate ", " . map format

newtype Domain = Domain Text
  deriving (Show, Eq, Ord)

instance HasFormatter Domain where
    format (Domain domain) = domain

