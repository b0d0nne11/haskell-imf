{-# LANGUAGE OverloadedStrings #-}

module Data.IMF.Types
  ( HasFormatter(..)
  , HasListFormatter(..)
  )
where

import           Data.Text (Text)
import qualified Data.Text as T

class HasFormatter a where
    format :: a -> Text

instance HasFormatter Text where
    format = id

class HasListFormatter a where
    formatList :: [a] -> Text

instance HasListFormatter Text where
    formatList = T.intercalate ", "

