module Tracing.Core (
    Span(..),
    SpanRelation(..),
    SpanRelationTag(..),
    SpanContext(..),
    SpanTag(..),
    OpName(..),
    SpanId(..),
    TraceId(..),
    Tracer(..),
    TracingInstructions(..),
    MonadTracer(..),

    recordSpan,
    debugPrintSpan
    ) where

import Control.Exception.Lifted (bracket)
import Control.Monad.Trans (liftIO, MonadIO)
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (NominalDiffTime, UTCTime, getCurrentTime, diffUTCTime)
import Data.Time.Clock.POSIX (POSIXTime, utcTimeToPOSIXSeconds)
import Data.Int
import Data.Maybe (isJust)
import Data.Monoid ((<>))
import Data.String (IsString)
import System.Random (randomRIO)
import qualified Data.Sequence as Seq
import Data.IORef (IORef, atomicModifyIORef',readIORef)
import qualified Data.Map.Strict as M
import Web.HttpApiData (FromHttpApiData)

newtype OpName = OpName Text
    deriving (Eq, Ord, Show, IsString)

newtype SpanId = SpanId Int64
    deriving (Eq, Ord, Show, FromHttpApiData)

newtype TraceId = TraceId Int64
    deriving (Eq, Ord, Show, FromHttpApiData)

-- | Indicates that the current monad can provide a 'Tracer'.
-- It assumes some form of environment
class Monad m => MonadTracer m where
    getTracer :: m Tracer
    currentTrace :: m TraceId
    currentSpan :: m (IORef SpanId)
    isDebug :: m Bool

-- | Wraps a computation & writes it to the 'Tracer''s IORef. Doesn't support parallel computations yet
recordSpan :: (MonadIO m, MonadBaseControl IO m, MonadTracer m) =>
    Maybe SpanRelationTag
    -> OpName
    -> m a
    -> m a
recordSpan spanType opName action = do
    tracer <- getTracer
    currentSpanCell <- currentSpan
    parent <- liftIO $ readIORef currentSpanCell
    trace <- currentTrace
    debug <- isDebug

    bracket (startSpan debug currentSpanCell tracer opName parent trace)
            (closeSpan tracer parent currentSpanCell)
            (const action)

    where
        startSpan debug ref (Tracer {svcName=serviceName}) opName parentId traceId = do
            now <- liftIO getCurrentTime
            newSpanId <- fmap SpanId . liftIO $ randomRIO (0, maxBound)
            let relType =
                    case spanType of
                        Just Child -> [ChildOf $ SpanContext traceId parentId]
                        Just Follows -> [FollowsFrom $ SpanContext traceId parentId]
                        Nothing -> []
                sId = if isJust spanType then newSpanId else parentId
                makeSpan ts =
                    Span {
                        operationName = opName,
                        context = SpanContext traceId sId,
                        timestamp = utcTimeToPOSIXSeconds now,
                        relations = relType,
                        tags = M.empty, -- TODO Allow adding these
                        baggage = M.empty, -- TODO Allow adding these
                        duration = diffUTCTime ts now,
                        debug,
                        serviceName
                        }
            liftIO $ atomicModifyIORef' ref (const (newSpanId, ()))
            pure $ ActiveSpan makeSpan

        closeSpan (Tracer {spanBuffer}) prevSpanId ref (ActiveSpan finishSpan) = do
            now <- liftIO getCurrentTime
            let span = finishSpan now
                sid = spanId (context span :: SpanContext)
            liftIO $ atomicModifyIORef' spanBuffer (\xs -> (span Seq.<| xs, ()))
            liftIO $ atomicModifyIORef' ref (const (prevSpanId, ()))

-- | Instructions that are specific to a single trace
data TracingInstructions =
    TracingInstructions {
        traceId :: !TraceId,
        spanId :: !SpanId,
        parentSpanId :: !SpanId,
        debug :: !Bool,
        sample :: !Bool
    } deriving (Eq, Show)

newtype ActiveSpan =
    ActiveSpan {finishSpan :: UTCTime -> Span}

-- | Global context required for tracing
data Tracer =
    Tracer {
        spanBuffer :: IORef (Seq.Seq Span),
        svcName :: T.Text
    }

data SpanContext =
    SpanContext {
        traceId :: !TraceId,
        spanId :: !SpanId
    } deriving (Eq, Show)

data SpanRelation =
    ChildOf !SpanContext | FollowsFrom !SpanContext
    deriving (Eq, Show)

data SpanRelationTag = Child | Follows

data Span = Span {
    operationName :: !OpName,
    context :: !SpanContext,
    timestamp :: !POSIXTime,
    duration :: !NominalDiffTime,
    relations :: ![SpanRelation],
    tags :: !(M.Map Text SpanTag),
    baggage:: !(M.Map Text Text),
    debug :: !Bool,
    serviceName :: !Text
    }

debugPrintSpan ::
    Span
    -> Text
debugPrintSpan span =
    "Span: " <>
    "id ["<>(unSpan $ spanId (context span :: SpanContext))<>"] "<>
    "op ["<>(unOp $ operationName span)<>"] "<>
    "duration ["<>(T.pack . show $ duration span)<> "] "<>
    "relations "<>(T.pack . show $ relations span)
    where
        unOp (OpName o) = o
        unSpan (SpanId s) = T.pack $ show s

data SpanTag
    = TagString !Text
    | TagBool !Bool
    | TagInt !Int64
    deriving (Eq, Show)
