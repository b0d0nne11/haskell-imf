{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Data.IMF.Network.ClientPool
  ( ClientPool
  , ClientLimits(..)
  , getClientPool
  , closeClientPool
  , deliverViaPool
  )
where

import           Data.Time.Clock                ( NominalDiffTime
                                                , diffUTCTime
                                                , getCurrentTime
                                                )
import           Control.Exception              ( bracketOnError )
import           Control.Monad                  ( void )
import qualified Data.ByteString.Lazy          as LB
import           Data.Pool

import           Data.IMF.Network.Chat
import           Data.IMF.Network.Client
import           Data.IMF.Network.Errors

data ClientLimits = ClientLimits
    { maxClients        :: Int
    , maxClientMessages :: Int
    , maxClientIdleTime :: NominalDiffTime
    , maxClientTime     :: NominalDiffTime
    }
  deriving (Show, Eq)

isStale :: ClientLimits -> Client -> IO Bool
isStale ClientLimits{..} Client{..} = do
    now <- getCurrentTime
    return $ diffUTCTime now clientCreatedAt >= maxClientTime || clientMessagesSent >= maxClientMessages

type ClientPool = (ClientLimits, Pool Client)

getClientPool :: ClientLimits -> ClientParams -> IO ClientPool
getClientPool limits@ClientLimits{..} params = do
    pool <- createPool (fst <$> getClient params) (void . closeClient) 1 maxClientIdleTime maxClients
    return (limits, pool)

closeClientPool :: ClientPool -> IO ()
closeClientPool (_, pool) = destroyAllResources pool

deliverViaPool :: Envelope
               -> LB.ByteString
               -> ClientPool
               -> IO (Maybe (Client, ChatLog))
deliverViaPool envelope msg (limits, pool) = do
    bracketOnError (tryTakeResource pool) handleError $
        maybe (return Nothing) $ \(client, lpool) -> do
            resp@(client', _) <- deliver envelope msg client
            isStale <- isStale limits client'
            if isStale || clientError client' /= OK
                then destroyResource pool lpool client'
                else putResource lpool client'
            return $ Just resp
  where
    handleError (Just (client, lpool)) = destroyResource pool lpool client
    handleError Nothing                = return ()

