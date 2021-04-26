module Data.IMF.Network.Errors
    ( Timeout(..)
    , cuttoff
    , ReplyException(..)
    , throwReply
    , catchReply
    , handleReply
    )
where

import Control.Exception.Safe
import Data.Text              (Text)
import Data.Time              (NominalDiffTime)
import System.Timeout         (timeout)

data Timeout = Timeout
  deriving (Show, Eq)

instance Exception Timeout

cuttoff :: NominalDiffTime -> IO a -> IO a
cuttoff t f = timeout (floor $ t * 1e6) f >>= maybe (throw Timeout) return

newtype ReplyException = ReplyException (Int, [Text])
  deriving (Show, Eq)

instance Exception ReplyException

throwReply :: MonadThrow m => (Int, [Text]) -> m a
throwReply = throw . ReplyException

catchReply :: MonadCatch m => m a -> ((Int, [Text]) -> m a) -> m a
catchReply = catchJust (\(ReplyException r) -> Just r)

handleReply :: MonadCatch m => ((Int, [Text]) -> m a) -> m a -> m a
handleReply = flip catchReply
