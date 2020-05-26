{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Data.IMF.Header
  ( Header(..)
  , headerToTuple
  , HeaderOptions(..)
  , buildHeaders
  )
where

import           Control.Monad       (when)
import           Data.Maybe          (catMaybes, isNothing, listToMaybe)
import           Data.Text           (Text)
import qualified Data.Text           as T

import           Data.IMF.Mailbox
import           Data.IMF.MessageId
import           Data.IMF.Time
import           Data.IMF.Types

data Header = HeaderDate ZonedTime
            | HeaderFrom [Mailbox]
            | HeaderSender Mailbox
            | HeaderReplyTo [Mailbox]
            | HeaderTo [Mailbox]
            | HeaderCc [Mailbox]
            | HeaderBcc [Mailbox]
            | HeaderMessageId MessageId
            | HeaderInReplyTo [MessageId]
            | HeaderReferences [MessageId]
            | HeaderSubject Text
            | HeaderComments Text
            | HeaderKeywords [Text]
            | HeaderOptional (Text, Text)
  deriving (Show, Eq)

instance HasFormatter Header where
    format = (\(k, v) -> k <> ": " <> v) . headerToTuple

instance HasListFormatter Header where
    formatList = T.concat . map (\h -> format h `T.append` "\r\n")

headerToTuple :: Header -> (Text, Text)
headerToTuple (HeaderDate       ztime   ) = ("Date", format ztime)
headerToTuple (HeaderFrom       mboxes  ) = ("From", formatList mboxes)
headerToTuple (HeaderSender     mbox    ) = ("Sender", format mbox)
headerToTuple (HeaderReplyTo    mboxes  ) = ("Reply-To", formatList mboxes)
headerToTuple (HeaderTo         []      ) = ("To", "Empty:;")
headerToTuple (HeaderTo         mboxes  ) = ("To", formatList mboxes)
headerToTuple (HeaderCc         []      ) = ("Cc", "Empty:;")
headerToTuple (HeaderCc         mboxes  ) = ("Cc", formatList mboxes)
headerToTuple (HeaderBcc        []      ) = ("Bcc", "")
headerToTuple (HeaderBcc        mboxes  ) = ("Bcc", formatList mboxes)
headerToTuple (HeaderMessageId  msgid   ) = ("Message-ID", format msgid)
headerToTuple (HeaderInReplyTo  msgids  ) = ("In-Reply-To", formatList msgids)
headerToTuple (HeaderReferences msgids  ) = ("References", formatList msgids)
headerToTuple (HeaderSubject    subject ) = ("Subject", subject)
headerToTuple (HeaderComments   comments) = ("Comments", comments)
headerToTuple (HeaderKeywords   keywords) = ("Keywords", formatList keywords)
headerToTuple (HeaderOptional   (k, v)  ) = (k, v)

data HeaderOptions = HeaderOptions
    { optFrom    :: [Mailbox]
    , optSender  :: Maybe Mailbox
    , optReplyTo :: Maybe [Mailbox]
    , optTo      :: Maybe [Mailbox]
    , optCc      :: Maybe [Mailbox]
    , optBcc     :: Maybe [Mailbox]
    , optSubject :: Text
    , optHeaders :: [Header]
    }

buildHeaders :: HeaderOptions -> IO (Text, [Mailbox], [Header])
buildHeaders HeaderOptions{..} = do
    when (null optFrom) $
        fail "empty from list"
    when (length optFrom > 1 && isNothing optSender) $
        fail "missing sender"
    when (any ((/= domain) . mboxDomain) originators) $
        fail "multipule originating domains"
    when (null recipients) $
        fail "empty recipient list"
    when (any headerIsRestricted optHeaders) $
        fail "restricted header"
    headers <- (++ baseHeaders) <$> initHeaders
    return (domain, recipients, headers)
  where
    domain = maybe "" mboxDomain $ listToMaybe originators
    originators = concat $ Just optFrom <> (pure <$> optSender) <> optReplyTo
    recipients = concat $ optTo <> optCc <> optBcc

    initHeaders :: IO [Header]
    initHeaders = do
        msgid <- newMessageId domain
        now <- getZonedTime
        return [ HeaderMessageId msgid
               , HeaderDate now
               ]

    baseHeaders :: [Header]
    baseHeaders =
        catMaybes [ HeaderFrom <$> Just optFrom
                  , HeaderSender <$> optSender
                  , HeaderReplyTo <$> optReplyTo
                  , HeaderTo <$> optTo
                  , HeaderCc <$> optCc
                  , HeaderBcc <$> ([] <$ optBcc)
                  , HeaderSubject <$> Just optSubject
                  ] ++ optHeaders

    headerIsRestricted :: Header -> Bool
    headerIsRestricted h@(HeaderComments _) = False
    headerIsRestricted h@(HeaderKeywords _) = False
    headerIsRestricted h@(HeaderOptional _) = False
    headerIsRestricted _                    = True
