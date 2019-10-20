{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Text.IMF.Network.ClientPool
  ( ClientLimits(..)
  , ClientPool
  , getClientPool
  , closeClientPool
  , deliverViaPool
  )
where

import           Data.Time.Clock                ( NominalDiffTime
                                                , UTCTime
                                                , diffUTCTime
                                                , getCurrentTime
                                                )
import           Control.Exception              ( bracketOnError )
import           Control.Monad                  ( void )
import qualified Data.ByteString.Lazy          as LB
import           Data.Time.Clock                ( NominalDiffTime )
import           Data.Pool

import           Text.IMF.Network.Client

data ClientLimits = ClientLimits
    { maxConns        :: Int
    , maxConnMessages :: Int
    , maxConnIdleTime :: NominalDiffTime
    , maxConnTime     :: NominalDiffTime
    }
  deriving (Show, Eq)

isStale :: ClientLimits -> Client -> IO Bool
isStale ClientLimits{..} Client{..} = do
    now <- getCurrentTime
    return $ diffUTCTime now createdAt >= maxConnTime || messagesSent >= maxConnMessages

type ClientPool = (ClientLimits, Pool Client)

getClientPool :: ClientLimits -> ClientParams -> IO ClientPool
getClientPool limits@ClientLimits{..} params = do
    pool <- createPool (fst <$> getClient params) (void . closeClient) 1 maxConnIdleTime maxConns
    return (limits, pool)

closeClientPool :: ClientPool -> IO ()
closeClientPool (_, pool) = destroyAllResources pool

deliverViaPool :: Envelope
               -> LB.ByteString
               -> ClientPool
               -> IO (Maybe (Client, ClientLog))
deliverViaPool envelope body (limits, pool) = do
    bracketOnError (tryTakeResource pool) handleError $
        maybe (return Nothing) $ \(client, lpool) -> do
            resp@(client', _) <- deliver envelope body client
            isStale <- isStale limits client'
            if isStale || lastErrors client' /= []
                then destroyResource pool lpool client'
                else putResource lpool client'
            return $ Just resp
  where
    handleError (Just (client, lpool)) = destroyResource pool lpool client
    handleError Nothing                = return ()

