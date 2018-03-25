{-# LANGUAGE ScopedTypeVariables #-}

module Tracing.Jaeger (
    publishJaeger
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


-- | Forces a dependency on `Req`
publishJaeger :: MonadIO m =>
    String
    -> Manager
    -> [Span]
    -> m (Maybe (Response T.Text))
publishJaeger _ _ [] = pure Nothing
publishJaeger destination manager spans =
    liftIO . fmap (Just . fmap decode) $ httpLbs jaegerReq manager
    where
        decode = T.decodeUtf8 . LBS.toStrict
        req = parseRequest_ destination
        body = RequestBodyLBS . encode $ ZipkinSpan <$> spans
        jaegerReq = req { method = "POST", requestBody = body, requestHeaders = [("content-type", "application/json")]}

newtype ZipkinSpan = ZipkinSpan Span
instance ToJSON ZipkinSpan where
    toJSON (ZipkinSpan span) = object $ [
        "traceId" .= unTrace (traceId $ context span),
        "id" .=  unSpan (spanId $ context span),
        "name" .=  unOp (operationName span),
        "timestamp" .= (floor $ timestamp span :: Int64),
        "kind" .= ("CLIENT"::T.Text),
        "duration" .= (toMicros $ duration span),
        "debug" .= (debug span),
        "localEndpoint" .= (object ["serviceName" .= (serviceName span)])
        ] <>
        parentId (relations span)
        where
            toMicros = (*) 1000000
            unOp (OpName n) = n
            unTrace (TraceId t) = padLeft 16 . T.pack . BS.unpack . fromMaybe "-1" $ BS.packHexadecimal t
            unSpan (SpanId s) = padLeft 16 . T.pack . BS.unpack . fromMaybe "-1" $ BS.packHexadecimal s
            parentId :: [SpanRelation] -> [(T.Text, Value)]
            parentId (ChildOf ctx:_) = ["parentId" .= (unSpan $ spanId ctx)]
            parentId (FollowsFrom ctx:_) = ["parentId" .= (unSpan $ spanId ctx)]
            parentId _ = []
            padLeft 0 txt = txt
            padLeft n txt
                | T.length txt < n = padLeft n ("0"<>txt)
                | otherwise = txt
