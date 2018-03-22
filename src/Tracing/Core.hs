module Tracing.Core (
    Span(..),
    SpanRelation(..),
    SpanContext(..),
    SpanTag(..),

    recordSpan
) where

import Control.Exception (bracket)
import Control.Monad.Trans (liftIO, MonadIO)
import UnliftIO (MonadUnliftIO, askUnliftIO, unliftIO)
import Data.Text (Text)
import Data.Time.Clock (NominalDiffTime)
import Data.Time.Clock.POSIX (POSIXTime)
import Data.Int
import qualified Data.Sequence as Seq
import Data.IORef (IORef, atomicModifyIORef',readIORef)
import qualified Data.Map.Strict as M

newtype OpName = OpName Text
    deriving (Eq, Ord, Show)
newtype SpanId = SpanId Int64
    deriving (Eq, Ord, Show)
newtype TraceId = TraceId Int64
    deriving (Eq, Ord, Show)

-- | Indicates that the current monad can provide a 'Tracer'.
-- It assumes some form of environment
class Monad m => MonadTracer m where
    getTracer :: m Tracer
    currentTrace :: m TraceId
    currentSpan :: m SpanId

recordSpan :: (MonadUnliftIO m, MonadTracer m) =>
    OpName
    -> m a
    -> m a
recordSpan opName action = do
    tracer <- getTracer
    parent <- currentSpan

    -- Push 'm' down into 'IO'
    unliftedContext <- askUnliftIO
    let startInIO = unliftIO unliftedContext $ startSpan opName parent
        closeInIO = unliftIO unliftedContext . closeSpan
        actionInIO = unliftIO unliftedContext action
    liftIO . unliftIO unliftedContext . liftIO $ bracket startInIO closeInIO (const actionInIO)

    where
        startSpan :: OpName -> SpanId -> m SpanId
        startSpan = undefined
        closeSpan :: SpanId -> m ()
        closeSpan = undefined


-- | A mutable buffer of 'Span's. Alternatively, this can just be a function
newtype Tracer =
    Tracer {
        spanBufer :: IORef (Seq.Seq Span)
    }

data SpanContext =
    SpanContext {
        traceId :: !TraceId,
        spanId :: !SpanId
    } deriving (Eq, Show)

data SpanRelation =
    ChildOf !SpanContext | FollowsFrom !SpanContext
    deriving (Eq, Show)


data Span = Span {
    operationName :: !OpName,
    spanId :: !SpanId,
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


