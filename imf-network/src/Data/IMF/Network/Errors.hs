module Data.IMF.Network.Errors
    ( Timeout(..)
    , cuttoff
    , ReplyException(..)
    , throwReply
    , catchReply
    , handleReply
    , ParseException(..)
    )
where

import Control.Monad.IO.Unlift (MonadIO, MonadUnliftIO)
import Data.Text               (Text)
import Data.Time.Clock         (NominalDiffTime)
import UnliftIO.Exception
import UnliftIO.Timeout        (timeout)

data Timeout = Timeout
  deriving (Show, Eq)

instance Exception Timeout

cuttoff :: MonadUnliftIO m => NominalDiffTime -> m a -> m a
cuttoff t f = timeout (floor $ t * 1e6) f >>= maybe (throwIO Timeout) return

newtype ReplyException = ReplyException (Int, [Text])
  deriving (Show, Eq)

instance Exception ReplyException

throwReply :: MonadIO m => (Int, [Text]) -> m a
throwReply r = throwIO $ ReplyException r

catchReply :: MonadUnliftIO m => m a -> ((Int, [Text]) -> m a) -> m a
catchReply = catchJust (\(ReplyException r) -> Just r)

handleReply :: MonadUnliftIO m => ((Int, [Text]) -> m a) -> m a -> m a
handleReply = flip catchReply

data ParseException = ParseSizeExceeded
                    | ParseFailure String
  deriving (Show, Eq)

instance Exception ParseException
