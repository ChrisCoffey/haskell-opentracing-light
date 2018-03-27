{-# LANGUAGE NamedFieldPuns, OverloadedStrings #-}

module Zipkin.ClientSpec (
    zipkinProps,
    zipkinSpec
    ) where

import Tracing.Zipkin
import Tracing.Core
import Instances()

import Data.Maybe (fromMaybe, maybe, isJust)
import Data.Aeson
import Data.Aeson.Types (parseMaybe)
import qualified Data.Text as T
import Test.HUnit
import Test.QuickCheck hiding (sample)
import Test.Tasty
import Test.Tasty.HUnit (testCase)
import Test.Tasty.QuickCheck (testProperty)
import Web.HttpApiData (parseUrlPiece)

zipkinProps :: TestTree
zipkinProps = testGroup "ZipkinProps" $  [
      testProperty "traceId always at least 16 chars long" $
        \s -> maybe False ((<=) 16 . T.length) . parseMaybe ( withObject "" ( .: "traceId") ) $ toJSON (ZipkinSpan (s :: Span))
    ,  testProperty "traceId never longer than 32 chars" $
        \s -> maybe False ((>=) 32 . T.length) . parseMaybe ( withObject "" ( .: "traceId") ) $ toJSON (ZipkinSpan (s :: Span))
    , testProperty "spanId always 16 chars long" $
        \s -> maybe False ((==) 16 . T.length) . parseMaybe ( withObject "" (.: "id") ) $ toJSON (ZipkinSpan (s :: Span))
    ]


zipkinSpec :: TestTree
zipkinSpec = testGroup "Zipkin Spec" $ [
    testCase "timestamp is in microseconds" $ do
        s <- generate arbitrary
        let fromMicros = maybe 0 (/ 1000000) . parseMaybe ( withObject "" (.: "timestamp") ) $ toJSON (ZipkinSpan (s :: Span))
        fromMicros @=? timestamp s
    , testCase "parentId set when a child Relation is present" $ do
        s <- generate arbitrary :: IO Span
        c <- generate arbitrary :: IO SpanContext
        let s' = s {relations = [ChildOf c]}
        let isPresent = maybe False (not . T.null) . parseMaybe ( withObject "" (.: "parentId") ) $ toJSON (ZipkinSpan s')
        isPresent @=? True
    , testCase "parentId set when a FollowsFrom Relation is present" $ do
        s <- generate arbitrary :: IO Span
        c <- generate arbitrary :: IO SpanContext
        let s' = s {relations = [FollowsFrom c]}
        let isPresent = maybe False (not . T.null) . parseMaybe ( withObject "" (.: "parentId") ) $ toJSON (ZipkinSpan s')
        isPresent @=? True
    , testCase "parentId unset when no Relation is present" $ do
        s <- generate arbitrary :: IO Span
        let s' = s {relations = []}
        let isPresent = maybe False (not . T.null) . parseMaybe ( withObject "" (.: "parentId") ) $ toJSON (ZipkinSpan s')
        isPresent @=? False
    ]

