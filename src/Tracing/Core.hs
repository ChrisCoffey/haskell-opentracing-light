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
    MonadTracer(..),

    recordSpan
    ) where

import Control.Exception.Lifted (bracket)
import Control.Monad.Trans (liftIO, MonadIO)
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Text (Text)
import Data.Time.Clock (NominalDiffTime, UTCTime, getCurrentTime, diffUTCTime)
import Data.Time.Clock.POSIX (POSIXTime, utcTimeToPOSIXSeconds)
import Data.Int
import Data.String (IsString)
import System.Random (randomIO)
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

    bracket (startSpan opName parent trace)
            (closeSpan tracer currentSpanCell)
            (const action)

    where
        startSpan opName parentId traceId = do
            now <- liftIO getCurrentTime
            newSpanId <- fmap SpanId . liftIO $ randomIO
            let relType =
                    case spanType of
                        Just Child -> [ChildOf $ SpanContext traceId parentId]
                        Just Follows -> [FollowsFrom $ SpanContext traceId parentId]
                        Nothing -> []
                makeSpan ts =
                    Span {
                        operationName = opName,
                        context = SpanContext traceId newSpanId,
                        timestamp = utcTimeToPOSIXSeconds now,
                        relations = relType,
                        tags = M.empty, -- TODO Allow adding these
                        baggage = M.empty, -- TODO Allow adding these
                        duration = diffUTCTime ts now
                        }
            pure $ ActiveSpan makeSpan

        closeSpan (Tracer {spanBuffer}) ref (ActiveSpan finishSpan) = do
            now <- liftIO getCurrentTime
            let span = finishSpan now
                sid = spanId $ context span
            liftIO $ atomicModifyIORef' spanBuffer (\xs -> (span Seq.<| xs, ()))
            liftIO $ atomicModifyIORef' ref (const (sid, ()))


newtype ActiveSpan =
    ActiveSpan {finishSpan :: UTCTime -> Span}

-- | A mutable buffer of 'Span's. Alternatively, this can just be a function
data Tracer =
    Tracer {
        spanBuffer :: IORef (Seq.Seq Span)
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
    baggage:: !(M.Map Text Text)
    }

data SpanTag
    = TagString !Text
    | TagBool !Bool
    | TagInt !Int64
    deriving (Eq, Show)
