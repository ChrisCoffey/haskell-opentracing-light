{-# LANGUAGE UndecidableInstances #-}
module Main where

import Servant.Tracing ( getInstructions, WithTracing)
import Tracing.Core (recordSpan,TracingInstructions(..), SpanRelationTag(..), Tracer(..), MonadTracer(..), SpanId(..),
    TraceId(..), debugPrintSpan)
import Tracing.Zipkin (publishZipkin)

import Control.Concurrent (threadDelay, forkIO)
import Control.Monad (forever, mapM_)
import Control.Monad.Trans (MonadIO, liftIO)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Reader (ReaderT(..), ask, MonadReader)
import Data.IORef (IORef, newIORef, atomicModifyIORef')
import Data.Maybe (maybe)
import Data.Proxy (Proxy(..))
import Data.Foldable (toList)
import Data.ByteString.Char8 as BS
import Servant
import Servant.Server
import System.Environment (lookupEnv, getEnv)
import qualified Data.Text as T
import Network.Wai.Handler.Warp (run)
import Network.HTTP.Client (Manager, newManager, defaultManagerSettings, responseStatus, responseBody)


main :: IO ()
main = do
    debug <- maybe False (== "TRUE") <$> lookupEnv "TRACE_DEBUG"
    destinationPath <- getEnv "TRACING_ENDPOINT"
    svcName <- T.pack <$> getEnv "TRACING_SERVICE"
    httpManager <- newManager defaultManagerSettings
    cell <- newIORef []
    let tracer = Tracer cell svcName
    forkIO $ publishLoop destinationPath httpManager tracer
    run 8080 . serve (Proxy :: Proxy ExampleAPI) $ server tracer


publishLoop ::
    String
    -> Manager
    -> Tracer
    -> IO ()
publishLoop destination manager (Tracer {spanBuffer}) = forever $ do
    threadDelay 5000000
    buffer <- atomicModifyIORef' spanBuffer (\b -> ([], b))
    mResp <- publishZipkin destination manager $ toList buffer
    case mResp of
        Nothing -> pure ()
        Just resp -> do
            print $ "Ran Loop " ++ (show $ responseStatus resp) ++ " " ++ (show . fmap debugPrintSpan $ toList buffer)
            print $ "       " ++ (T.unpack $ responseBody resp)



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
                recordSpan (const Child <$> inst) "Run Fast" . liftIO $
                    threadDelay 1000000 *> pure 42
        runSlow = do
            ctx <- loadCtx
            runStack ctx $
                recordSpan (const Child <$> inst) "Run Slow" $ do
                    liftIO $ threadDelay 500000
                    let action = liftIO $ threadDelay 1500000 *> pure "Boo"
                    recordSpan (Just Child) "Slow Child" action

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
    isDebug = (debug . instructions) <$> ask
