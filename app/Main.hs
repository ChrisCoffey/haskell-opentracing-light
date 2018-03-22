{-# LANGUAGE UndecidableInstances #-}
module Main where

import Servant.Tracing (TracingInstructions(..), getInstructions, WithTracing)
import Tracing.Core (recordSpan, SpanRelationTag(..), Tracer(..), MonadTracer(..), SpanId(..),
    TraceId(..))

import Control.Concurrent (threadDelay, forkIO)
import Control.Monad (forever)
import Control.Monad.Trans (MonadIO, liftIO)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Reader (ReaderT(..), ask, MonadReader)
import Data.IORef (IORef, newIORef, atomicModifyIORef')
import Data.Maybe (maybe)
import Data.Proxy (Proxy(..))
import qualified Data.Sequence as Seq
import Servant
import Servant.Server
import System.Environment (lookupEnv)
import qualified Data.Text as T
import Network.Wai.Handler.Warp (run)


main :: IO ()
main = do
    debug <- maybe False (== "TRUE") <$> lookupEnv "TRACE_DEBUG"
    tracer <- Tracer <$> newIORef Seq.empty
    forkIO $ dumpToConsole tracer
    run 8080 . serve (Proxy :: Proxy ExampleAPI) $ server tracer
    where
        dumpToConsole Tracer {spanBuffer} = forever $ do
            threadDelay 15000000
            buffer <- atomicModifyIORef' spanBuffer (\b -> (Seq.empty, b))
            print $ ("Buffer Size: " ++ show (Seq.length buffer))


--
-- API Definition
--

type ExampleAPI =
    WithTracing :> MyAPI

type MyAPI =
    Header "Auth" T.Text :>
        (
        "fast" :> Get '[JSON] Int
        :<|>
        "slow" :> Get '[JSON] T.Text
        )


--
-- Server logic
--
server :: Tracer -> Server ExampleAPI
server tracer inst auth =
    runFast
    :<|>
    runSlow
    where
        loadCtx = do
            instructions <- getInstructions True inst
            currSpan <- liftIO $ newIORef (spanId instructions)
            pure Ctx {
                tracer,
                currSpan,
                instructions
                }
        runFast = do
            ctx <- loadCtx
            runStack ctx $
                recordSpan (Just Child) "Run Fast" . liftIO $
                    threadDelay 1000000 *> pure 42
        runSlow = do
            ctx <- loadCtx
            runStack ctx $
                recordSpan (Just Child) "Run Slow" . liftIO $
                    threadDelay 10000000 *> pure ("Boo" :: T.Text)

runStack :: Ctx -> ReaderT Ctx Handler a -> Handler a
runStack ctx action = runReaderT action ctx

data Ctx = Ctx {
    tracer :: Tracer,
    currSpan :: IORef SpanId,
    instructions :: TracingInstructions
    }

instance (Monad m, MonadBaseControl IO m, MonadIO m, MonadReader Ctx m) => MonadTracer m where
    getTracer = tracer <$> ask
    currentTrace = (traceId . instructions) <$> ask
    currentSpan = currSpan <$> ask
