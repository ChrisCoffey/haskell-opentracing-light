{-# LANGUAGE NamedFieldPuns, OverloadedStrings #-}

module DataDog.ClientSpec (
    ddSpec
    ) where

import Tracing.DataDog
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
import Data.Int

ddSpec :: TestTree
ddSpec = testGroup "DataDog Spec" $ [
    testCase "start is in nanoseconds" $ do
        s <- generate arbitrary
        let fromNanos = maybe 0 (/ 1000000000) . parseMaybe ( withObject "" (.: "start") ) $ toJSON (DataDogSpan (s :: Span))
        fromNanos @=? timestamp s
    , testCase "parentId set when a child Relation is present" $ do
        s <- generate arbitrary :: IO Span
        c <- generate arbitrary :: IO SpanContext
        let s' = s {relations = [ChildOf c]}
        let isPresent = maybe False (> (0:: Int64)) . parseMaybe ( withObject "" (.: "parent_id") ) $ toJSON (DataDogSpan s')
        isPresent @=? True
    , testCase "parentId set when a FollowsFrom Relation is present" $ do
        s <- generate arbitrary :: IO Span
        c <- generate arbitrary :: IO SpanContext
        let s' = s {relations = [FollowsFrom c]}
        let isPresent = maybe False (> (0::Int64)) . parseMaybe ( withObject "" (.: "parent_id") ) $ toJSON (DataDogSpan s')
        isPresent @=? True
    , testCase "parentId unset when no Relation is present" $ do
        s <- generate arbitrary :: IO Span
        let s' = s {relations = []}
        let isPresent = maybe False (> (0::Int64)) . parseMaybe ( withObject "" (.: "parent_id") ) $ toJSON (DataDogSpan s')
        isPresent @=? False
    ]
