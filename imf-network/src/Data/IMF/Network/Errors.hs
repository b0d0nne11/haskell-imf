module Data.IMF.Network.Errors
    ( ChatException(..)
    )
where

import           Control.Monad.Catch            ( Exception )
import           Network.TLS                    ( TLSException )

data ChatException = OK
                   | DNSLookupFailed
                   | DNSLookupNotFound
                   | ConnectFailure IOError
                   | SocketFailure IOError
                   | TLSSocketFailure TLSException
                   | InvalidSocketOperation
                   | PeerConnectionClosed
                   | Timeout
                   | ParseFailure
                   | SizeExceeded
                   | TemporaryFailure
                   | PermanentFailure
                     -- client-only exceptions
                   | TLSNotSupported
                   | TLSNegotiationFailure TLSException
                   | AuthNotSupported
                   | AuthMethodNotSupported
                   | AuthNotEncrypted
                     -- server-only exceptions
                   | Shutdown
                   | CommandParseFailure
                   | CommandSizeExceeded
                   | ParameterParseFailure
                   | CommandNotImplemented
                   | CommandOutOfOrder
                   | ParameterNotImplemented
                   | ShutdownInProgress
                   | MailboxParseFailure
                   | TooManyRecipients
                   | MessageParseFailure
                   | MessageSizeExceeded
                   | NoValidRecipients
                   | CredentialsParseFailure
                   | CredentialsSizeExceeded
                   | AuthenticationRequired
                   | EncryptionRequired
  deriving (Show, Eq)

instance Exception ChatException

