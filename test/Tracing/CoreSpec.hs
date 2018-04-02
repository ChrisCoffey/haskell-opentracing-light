{-# LANGUAGE NamedFieldPuns, OverloadedStrings, UndecidableInstances, DuplicateRecordFields, FlexibleInstances #-}

module Tracing.CoreSpec (
    coreProps,
    coreSpec
    ) where

import Tracing.Core
import Instances()

import Control.Monad.Trans (liftIO, MonadIO)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Reader
import Data.Maybe (fromMaybe, maybe, isJust)
import Data.Aeson
import Data.Aeson.Types (parseMaybe)
import Data.IORef (IORef, newIORef, readIORef)
import qualified Data.Text as T
import Test.HUnit
import Test.QuickCheck hiding (sample)
import Test.Tasty
import Test.Tasty.HUnit (testCase)
import Test.Tasty.QuickCheck (testProperty)
import Web.HttpApiData (parseUrlPiece)

coreProps :: TestTree
coreProps = testGroup "Tracing Core Properties" $ [
    ]

coreSpec :: TestTree
coreSpec = testGroup "Tracing Core Specification" $ [
    testCase "logged span id == fresh id for top level span" $ do
        ctx <- newContext
        flip runReaderT ctx . recordSpan Nothing [] "Top Level Test" $ pure (7 + 8)
        traces <- readIORef . spanBuffer $ tracer ctx
        let h = head traces
        length traces @=? 1
        relations h @=? []
        (sid $ context h) @=? SpanId 1234
    , testCase "logged span id == new id for child span" $ do
        ctx <- newContext
        flip runReaderT ctx . recordSpan (Just Child) [] "Child Test" $ pure (7 + 8)
        traces <- readIORef . spanBuffer $ tracer ctx
        let h = head traces
        length traces @=? 1
        (length $ relations h) @=? 1
        ((sid $ context h) /= SpanId 1234)  @? "Parent Id is still the child span's Id"
    , testCase "logged span id == new id for successor span" $ do
        ctx <- newContext
        flip runReaderT ctx . recordSpan (Just Follows) [] "Successor Test" $ pure (7 + 8)
        traces <- readIORef . spanBuffer $ tracer ctx
        let h = head traces
        length traces @=? 1
        ((sid $ context h) /= SpanId 1234)  @? "Parent Id is still the successor span's Id"
    , testCase "child span sets parent relation" $ do
        ctx <- newContext
        flip runReaderT ctx . recordSpan (Just Child) [] "Foo" $ pure (7 + 8)
        traces <- readIORef . spanBuffer $ tracer ctx
        let h = head traces
        length traces @=? 1
        ((\(ChildOf s) -> sid s ) . head $ relations h) @=?  SpanId 1234
    , testCase "successor span sets precursor relation" $ do
        ctx <- newContext
        flip runReaderT ctx . recordSpan (Just Follows) [] "Foo" $ pure (7 + 8)
        traces <- readIORef . spanBuffer $ tracer ctx
        let h = head traces
        ((\(FollowsFrom s) -> sid s ) . head $ relations h) @=?  SpanId 1234
    , testCase "sibling spans share same parent" $ do
        ctx <- newContext
        flip runReaderT ctx . recordSpan (Just Child) [] "Foo" $ pure (7 + 8)
        flip runReaderT ctx . recordSpan (Just Child) [] "Foo" $ pure (7 + 8)
        [x,y] <- readIORef . spanBuffer $ tracer ctx
        (parentId x == parentId y) @? "Sibling spans must share a parent"
    , testCase "sibling successor spans share same parent" $ do
        ctx <- newContext
        flip runReaderT ctx . recordSpan (Just Follows) [] "Foo" $ pure (7 + 8)
        flip runReaderT ctx . recordSpan (Just Follows) [] "Foo" $ pure (7 + 8)
        [x,y] <- readIORef . spanBuffer $ tracer ctx
        (parentId x == parentId y) @? "Sibling spans must share a parent"
    , testCase "sibling heterogeneous spans share same parent" $ do
        ctx <- newContext
        flip runReaderT ctx . recordSpan (Just Follows) [] "Foo" $ pure (7 + 8)
        flip runReaderT ctx . recordSpan (Just Child) [] "Foo" $ pure (7 + 8)
        [x,y] <- readIORef . spanBuffer $ tracer ctx
        (parentId x == parentId y) @? "Sibling spans must share a parent"
    , testCase "sibling spans have distinct ids" $ do
        ctx <- newContext
        flip runReaderT ctx . recordSpan (Just Follows) [] "Foo" $ pure (7 + 8)
        flip runReaderT ctx . recordSpan (Just Child) [] "Foo" $ pure (7 + 8)
        [x,y] <- readIORef . spanBuffer $ tracer ctx
        ((sid $ context x) /= (sid $ context y)) @? "Sibling spans must have different ids"
   {- , testCase "nested calls chain" $ do
        ctx <- newContext
        flip runReaderT ctx . recordSpan Nothing [] "Foo" $
            recordSpan (Just Child) [] "Bar" $
            recordSpan (Just Child) [] "Baz"
        -- This test will break if 'recordSpan' changes significantly
        [x, y, z] <- readIORef . spanBuffer $ tracer ctx
        let pid =
        ((sid $ context x) /= (sid $ context y)) @? "Sibling spans must have different ids"
        -}
    ]

newContext :: IO Ctx
newContext = do
    cell <- newIORef []
    currSpan <- newIORef $ SpanId 1234
    pure Ctx {
        tracer = Tracer cell "UNIT TESTING",
        currSpan = currSpan,
        dbg = True,
        currTrace = TraceId 9876
    }

sid :: SpanContext -> SpanId
sid SpanContext {spanId} = spanId

parentId :: Span -> Maybe SpanId
parentId Span {relations=((ChildOf c):_)} = Just $ sid c
parentId Span {relations=((FollowsFrom c):_)} = Just $ sid c
parentId _ =  Nothing

data Ctx = Ctx {
    tracer :: Tracer,
    currSpan :: IORef SpanId,
    dbg :: Bool,
    currTrace :: TraceId
    }

instance (Monad m, MonadBaseControl IO m, MonadIO m, MonadReader Ctx m) => MonadTracer m where
    getTracer = tracer <$> ask
    currentTrace = currTrace <$> ask
    currentSpan = currSpan <$> ask
    isDebug = dbg <$> ask
