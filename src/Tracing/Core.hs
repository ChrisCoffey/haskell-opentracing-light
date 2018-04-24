{-# LANGUAGE ExistentialQuantification, RankNTypes, UndecidableInstances #-}

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
    ToSpanTag(..),
    Tag(..),

    recordSpan,
    debugPrintSpan
    ) where

import Control.Arrow ((&&&))
import Control.Exception.Lifted (bracket)
import Control.Monad.Trans (liftIO, MonadIO)
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Lazy as BSL
import Data.Time.Clock (NominalDiffTime, UTCTime, getCurrentTime, diffUTCTime)
import Data.Time.Clock.POSIX (POSIXTime, utcTimeToPOSIXSeconds)
import Data.Int
import Data.Aeson (ToJSON, encode)
import Data.Maybe (isJust)
import Data.Monoid ((<>))
import Data.String (IsString)
import System.Random (randomRIO)
import Data.IORef (IORef, atomicModifyIORef',readIORef)
import qualified Data.Map.Strict as M
import Web.HttpApiData (FromHttpApiData)

-- | Human-readable name for the span
newtype OpName = OpName Text
    deriving (Eq, Ord, Show, IsString)

-- | An opaque & unique identifier for a trace segment, called a Span
newtype SpanId = SpanId Int64
    deriving (Eq, Ord, Show, FromHttpApiData)

-- | An opaque & unique identifier for a logical operation. Traces are composed of many 'Span's
newtype TraceId = TraceId Int64
    deriving (Eq, Ord, Show, FromHttpApiData)

-- | Indicates that the current monad can provide a 'Tracer' and related context.
-- It assumes some form of environment. While this exposes some mutable state, all
-- of it is hidden away behind the `recordSpan` api.
class Monad m => MonadTracer m where
    getTracer :: m Tracer -- ^ 'Tracer' is global to the process
    currentTrace :: m TraceId -- ^ Set during the initial request from the outside world, this is propagated across all nodes in the call
    currentSpan :: m (IORef SpanId) -- ^ Set via 'recordSpan'
    isDebug :: m Bool -- ^ Set during the initial request from the outside world, this is propagated across all nodes in the call

-- | Wraps a computation & writes it to the 'Tracer''s IORef. To start a new top-level span, and therefore
-- a new trace, call this function with *spanType* == 'Nothing'. Otherwise, this will create a child span.
--
-- Doesn't support parallel computations yet
recordSpan :: (MonadIO m, MonadBaseControl IO m, MonadTracer m) =>
    Maybe SpanRelationTag
    -> [Tag]
    -> OpName
    -> m a
    -> m a
recordSpan spanType tags opName action = do
    Tracer {svcName=serviceName, spanBuffer} <- getTracer
    currentSpanCell <- currentSpan
    activeSpanId <- liftIO $ readIORef currentSpanCell
    traceId <- currentTrace
    debug <- isDebug

    -- generates a thunk that completes once the action provided to 'recordSpan' finishes.
    -- While this is running, there is a new "activeSpanId" that any children will use. Nested calls
    -- generate a stack of spans.
    let startSpan = do
            now <- liftIO getCurrentTime
            newSpanId <- fmap SpanId . liftIO $ randomRIO (0, maxBound)
            let loggedSpanId = resolveSpanId activeSpanId newSpanId
                rel = newSpanRelation traceId activeSpanId
                makeSpan ts =
                    Span {
                        operationName = opName,
                        context = SpanContext traceId loggedSpanId,
                        timestamp = utcTimeToPOSIXSeconds now,
                        relations = rel,
                        tags = M.fromList $ (\(Tag key t) -> (key, toSpanTag t) ) <$> tags,
                        baggage = M.empty, -- TODO Allow adding these
                        duration = diffUTCTime ts now,
                        debug,
                        serviceName
                        }
            liftIO $ atomicModifyIORef' currentSpanCell (const (newSpanId, ()))
            pure $ ActiveSpan makeSpan


        closeSpan (ActiveSpan finishSpan) = do
            now <- liftIO getCurrentTime
            let span = finishSpan now
                sid = spanId (context span :: SpanContext)
            liftIO $ atomicModifyIORef' spanBuffer (\xs -> (span:xs, ()))
            liftIO $ atomicModifyIORef' currentSpanCell (const (activeSpanId, ()))

    bracket startSpan
            closeSpan
            (const action)

    where
        -- When this is a top level span, there should be no SpanRelationTag. These two functions work
        -- together to ensure the spans nest properly
        resolveSpanId activeSpanId newSpanId =
            if isJust spanType
            then newSpanId
            else activeSpanId
        newSpanRelation traceId activeSpanId =
            case spanType of
                Just Child -> [ChildOf $ SpanContext traceId activeSpanId]
                Just Follows -> [FollowsFrom $ SpanContext traceId activeSpanId]
                Nothing -> []

-- | Instructions that are specific to a single trace
data TracingInstructions =
    TracingInstructions {
        traceId :: !TraceId,
        spanId :: !SpanId,
        parentSpanId :: !(Maybe SpanId),
        debug :: !Bool,
        sample :: !Bool
        } deriving (Eq, Show)

newtype ActiveSpan =
    ActiveSpan {finishSpan :: UTCTime -> Span}

-- | Global context required for tracing. The `spanBuffer` should be manually drained by library users.
data Tracer =
    Tracer {
        spanBuffer :: IORef [Span],
        svcName :: T.Text
    }

-- | Uniquely identifies a given 'Span' & points to its encompasing trace
data SpanContext =
    SpanContext {
        traceId :: !TraceId,
        spanId :: !SpanId
    } deriving (Eq, Show)

-- | Spans may be top level, a child, or logically follow from a given span.
data SpanRelation =
    ChildOf !SpanContext | FollowsFrom !SpanContext
    deriving (Eq, Show)

-- | Indicates the type of relation this span represents
data SpanRelationTag = Child | Follows

-- | A timed section of code with a logical name and 'SpanContext'. Individual spans will be reconstructed by an
-- OpenTracing backend into a single trace.
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
    } deriving Show

-- | Dump the details of a span. Used for debugging or logging
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

-- | Used to embed additional information into a Span for consumption & viewing in a tracing backend
data SpanTag
    = TagString !Text
    | TagBool !Bool
    | TagInt !Int64
    | TagDouble !Double
    deriving (Eq, Show)

-- | Allows for easily representing multiple types in a tag list
data Tag = forall a. ToSpanTag a => Tag T.Text a

-- | The type in question may be converted into a 'SpanTag'
class ToSpanTag a where
    toSpanTag :: a -> SpanTag

instance ToSpanTag SpanTag where
    toSpanTag = id

instance ToJSON a => ToSpanTag a where
    toSpanTag = TagString . T.decodeUtf8 . BSL.toStrict . encode
