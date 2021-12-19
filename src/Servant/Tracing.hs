{-# LANGUAGE RankNTypes #-}
module Servant.Tracing (
    WithTracing,
    TracingInstructions(..),
    instructionsToHeader,
    getInstructions
    ) where

import Tracing.Core (Tracer, TraceId(..), SpanId(..), MonadTracer, TracingInstructions(..))

import Control.Arrow (first)
import Control.Monad.Trans (liftIO, MonadIO)
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lex.Integral as BS
import Data.Text.Read(hexadecimal)
import Data.Bits (testBit, (.|.))
import Data.Maybe (catMaybes, fromMaybe)
import Data.Monoid ((<>))
import Servant.API.Header (Header)
import System.Random (randomRIO)
import Web.HttpApiData (FromHttpApiData(..))

-- | Constrain the 'ServerT''s base monad such that it provides an instance of 'MonadTracer'
type WithTracing = Header "uber-trace-id" TracingInstructions


-- | Jaeger format: http://jaeger.readthedocs.io/en/latest/client_libraries/#propagation-format
-- This allows the trace backend to reassemble downstream traces.
instructionsToHeader :: TracingInstructions -> T.Text
instructionsToHeader TracingInstructions {traceId=(TraceId tid), spanId, parentSpanId, sample, debug} =
    toField tid<>":"<>
    (toField $ unSpanId spanId) <> ":"<>
    (fromMaybe "" $ (toField . unSpanId) <$> parentSpanId) <> ":" <>
    (T.pack $ show setFlags)
    where
        unSpanId (SpanId sid) = sid
        toField = T.pack . BS.unpack . fromMaybe "" . BS.packHexadecimal
        setFlags :: Int
        setFlags = (if debug then 2 else 0) .|. (if sample then 1 else 0) .|. 0


instance FromHttpApiData TracingInstructions where
    parseUrlPiece ::
        T.Text
        -> Either T.Text TracingInstructions
    parseUrlPiece raw =
        case T.splitOn ":" raw of
            [rawTraceId, rawSpanId, rawParentId, flags] -> let
                res = do
                    traceId <- TraceId . fromIntegral . fst <$> hexadecimal rawTraceId
                    spanId <- SpanId . fromIntegral . fst <$> hexadecimal rawSpanId
                    let resolvedPid = if T.null rawParentId
                                      then pure (Nothing, "")
                                      else first Just <$> hexadecimal rawParentId
                    parentId <- fmap (SpanId . fromIntegral) . fst <$> resolvedPid
                    flagField <- fromIntegral . fst <$> hexadecimal flags
                    let [sample, debug]= [sampleFlag, debugFlag] <*> [flagField]
                    pure TracingInstructions {
                       traceId = traceId,
                       spanId = spanId,
                       parentSpanId = parentId,
                       sample = sample,
                       debug = debug
                    }
                in case res of
                    Left err -> Left $ T.pack err
                    Right val -> Right  val
            _ -> Left $ raw <> " is not a valid uber-trace-id header"
        where
            sampleFlag :: Int -> Bool
            sampleFlag = (`testBit` 0)
            debugFlag :: Int -> Bool
            debugFlag = (`testBit` 1)


-- TODO write a monad that wraps servant & determines if it should sample or not. Takes a sampling determinant. Only evaluates if the header is not present

-- | In the event that there are no 'TracingInstructions' for this call, generate new instructions.
--
-- This has a
getInstructions :: MonadIO m =>
    Bool
    -> Maybe TracingInstructions
    -> m TracingInstructions
getInstructions debug Nothing = do
    newTraceId <- liftIO $ randomRIO (0, maxBound)
    newSpanId <- liftIO $ randomRIO (0, maxBound)
    sample <- liftIO $ randomRIO (0, 1000)
    pure TracingInstructions {
        traceId = TraceId newTraceId,
        spanId = SpanId newSpanId,
        parentSpanId = Nothing,
        debug,
        sample = sample == (1::Int)
        }
getInstructions _ (Just inst) = pure inst
