module Text.IMF.Network.Errors
  ( ClientException(..)
  )
where

import           Control.Exception              ( Exception )
import           Network.TLS                    ( TLSException )

data ClientException = DNSLookupFailed
                     | DNSLookupNotFound
                     | ConnectFailure IOError
                     | SocketFailure IOError
                     | TLSSocketFailure TLSException
                     | ResponseTimeout
                     | BadResponseFormat
                     | TemporaryFailure
                     | PermanentFailure
                     | TLSNotSupported
                     | TLSNegotiationFailure TLSException
                     | AuthNotSupported
                     | AuthMethodNotSupported
                     | AuthNotEncrypted
  deriving (Show, Eq)

instance Exception ClientException

