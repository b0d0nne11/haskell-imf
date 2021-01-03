module MailBin.Config
  ( Config
  , loadConfig
  , subconfig
  , lookupDefault
  )
where

import           Data.Configurator
import           Data.Configurator.Types
import           Data.String             (fromString)
import qualified Data.Text               as T
import           Data.Time.Clock         (NominalDiffTime)
import           Network.Simple.TCP      (HostPreference)

instance Configured NominalDiffTime where
    convert (Number n) = Just $ fromRational n
    convert _          = Nothing

instance Configured HostPreference where
    convert (String t) = Just $ fromString $ T.unpack t
    convert _          = Nothing

loadConfig :: IO Config
loadConfig = load [Optional "mailbin.cfg"]
