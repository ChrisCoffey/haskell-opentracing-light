{-# LANGUAGE ScopedTypeVariables #-}

module Tracing.Zipkin (
    publishZipkin,
    ZipkinSpan(..)
    ) where

import Tracing.Core (Span(..), SpanId(..), OpName(..), TraceId(..), SpanContext(..),
    SpanRelation(..))

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


-- | Publish 'Span' in the Zipkin format (TODO add link to zipkin spec here). No call is made
-- on an empty
publishZipkin :: MonadIO m =>
    String -- The address of the backend server
    -> Manager
    -> [Span] -- ^ The traced spans to send to a Zipkin backend
    -> m (Maybe (Response T.Text))
publishZipkin _ _ [] = pure Nothing
publishZipkin destination manager spans =
    liftIO . fmap (Just . fmap decode) $ httpLbs zipkinReq manager
    where
        decode = T.decodeUtf8 . LBS.toStrict
        req = parseRequest_ destination
        body = RequestBodyLBS . encode $ ZipkinSpan <$> spans
        zipkinReq = req { method = "POST", requestBody = body, requestHeaders = [("content-type", "application/json")]}

newtype ZipkinSpan = ZipkinSpan Span
instance ToJSON ZipkinSpan where
    toJSON (ZipkinSpan span) = object $ [
        "traceId" .= unTrace (traceId $ context span),
        "id" .=  unSpan (spanId $ context span),
        "name" .=  unOp (operationName span),
        "timestamp" .= (floor . toMicros $ timestamp span :: Int64),
        "kind" .= ("CLIENT"::T.Text),
        "duration" .= (toMicros $ duration span),
        "debug" .= (debug span),
        "localEndpoint" .= (object ["serviceName" .= (serviceName span)])
        ] <>
        parentId (relations span)
        where
            toMicros = (*) 1000000
            unOp (OpName n) = n
            zipkinFormatId = padLeft 16 . T.pack . BS.unpack . fromMaybe "-1"
            unTrace (TraceId t) = zipkinFormatId $ BS.packHexadecimal t
            unSpan (SpanId s) = zipkinFormatId $ BS.packHexadecimal s
            parentId :: [SpanRelation] -> [(T.Text, Value)]
            parentId (ChildOf ctx:_) = ["parentId" .= (unSpan $ spanId ctx)]
            parentId (FollowsFrom ctx:_) = ["parentId" .= (unSpan $ spanId ctx)]
            parentId _ = []
            padLeft 0 txt = txt
            padLeft n txt
                | T.length txt < n = padLeft n ("0"<>txt)
                | otherwise = txt
