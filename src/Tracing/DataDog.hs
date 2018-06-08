{-# LANGUAGE ScopedTypeVariables #-}

module Tracing.DataDog (
    publishDataDog,
    DataDogSpan(..)
    ) where

import Tracing.Core (Span(..), SpanId(..), OpName(..), TraceId(..), SpanContext(..),
    SpanRelation(..), SpanTag(..))

import Control.Monad.Trans (liftIO, MonadIO)
import Control.Monad (void)
import Data.Monoid ((<>), mempty)
import Data.Aeson
import Data.Maybe (fromMaybe)
import Data.Int (Int64)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lex.Integral as BS
import qualified Data.HashMap.Strict as HM
import Network.HTTP.Client
import Network.HTTP.Types.Header (Header)


-- | Publish 'Span' in the <https://docs.datadoghq.com/api/?lang=bash#send-traces DataDog format> . No call is made
-- on an empty span list
publishDataDog :: MonadIO m =>
    String -- ^ The address of the backend server
    -> Manager
    -> [Header]
    -> [Span] -- ^ The traced spans to send to a DataDog backend
    -> m (Maybe (Response T.Text))
publishDataDog _ _ _ [] = pure Nothing
publishDataDog destination manager additionalHeaders spans =
    liftIO . fmap (Just . fmap decode) $ httpLbs ddReq manager
    where
        decode = T.decodeUtf8 . LBS.toStrict
        req = parseRequest_ destination
        body = RequestBodyLBS . encode $ DataDogSpan <$> spans
        ddReq = req { method = "POST",
                      requestBody = body,
                      requestHeaders = [("content-type", "application/json")] <> additionalHeaders
                    }

newtype DataDogSpan = DataDogSpan Span
instance ToJSON DataDogSpan where
    toJSON (DataDogSpan span) = object $ [
        "trace_id" .= (unTrace . traceId $ context span),
        "span_id" .= (unSpan . spanId $ context span),
        "name" .=  unOp (operationName span),
        "resource" .= unOp (operationName span),
        "start" .= (floor . toNanos $ timestamp span :: Int64),
        "type" .= ("web"::T.Text),
        "duration" .= (toNanos $ duration span),
        "service" .= (serviceName span),
        "meta" .= (unTag <$> tags span)
        ] <>
        parentId (relations span)
        where
            toNanos = (*) 1000000000
            unOp (OpName n) = n
            unSpan (SpanId sid) = sid
            unTrace (TraceId tid) = tid
            parentId :: [SpanRelation] -> [(T.Text, Value)]
            parentId (ChildOf ctx:_) = ["parent_id" .= (unSpan $ spanId ctx)]
            parentId (FollowsFrom ctx:_) = ["parent_id" .= (unSpan $ spanId ctx)]
            parentId _ = []
            padLeft 0 txt = txt
            padLeft n txt
                | T.length txt < n = padLeft n ("0"<>txt)
                | otherwise = txt
            unTag (TagString a) = toJSON a
            unTag (TagBool a) = toJSON a
            unTag (TagInt a) = toJSON a
            unTag (TagDouble a) = toJSON a
