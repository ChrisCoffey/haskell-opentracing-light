{-# LANGUAGE NamedFieldPuns, OverloadedStrings #-}

module Servant.TracingSpec (
    tracingProps,
    tracingSpecs
    ) where

import Servant.Tracing (TracingInstructions(..), instructionsToHeader)
import Tracing.Core (TraceId(..), SpanId(..))

import Data.Maybe (fromMaybe, maybe)
import qualified Data.Text as T
import qualified Data.Text.Read as T
import Test.HUnit
import Test.QuickCheck hiding (sample)
import Test.Tasty
import Test.Tasty.HUnit (testCase)
import Test.Tasty.QuickCheck (testProperty)
import Web.HttpApiData (parseUrlPiece)

import Debug.Trace (trace)

tracingProps :: TestTree
tracingProps = testGroup "Servant Properties" [
    instructionProps
    ]

tracingSpecs :: TestTree
tracingSpecs = testGroup "Servant Specification" [
    testCase "Sample field controls low bit" $ let
        header = instructionsToHeader $ dummyInst1 {debug = False}
        fields =  extractFields header
        in fields @=? 1,
    testCase "Both fields set == 3" $ let
        header = instructionsToHeader dummyInst1
        fields = extractFields header
        in fields @=? 3,
    testCase "TraceId converts to hex representation" $ let
        header = instructionsToHeader $ dummyInst1 {traceId = TraceId 15}
        traceId = extractId . head $ T.splitOn ":" header
        in traceId @=? 0xF,
    testCase "SpanId converts to hex representation" $ let
        header = instructionsToHeader $ dummyInst1 {spanId = SpanId 255}
        traceId = extractId . head . tail $ T.splitOn ":" header
        in traceId @=? 0xFF,
    testCase "4:3:4:3 == TracingInstructions 4 3 4 True True" $ let
        inst = parseUrlPiece "4:3:4:3"
        in case inst of
            Right inst -> inst @=? (TracingInstructions (TraceId 4) (SpanId 3) (SpanId 4) True True)
            Left _ -> assertFailure $ "Failed: "++ show inst
    ]
    where
        dummyInst1 = TracingInstructions (TraceId 1) (SpanId 1) (SpanId 0) True True
        extractId = maybe (-1) fst . toMaybe . T.hexadecimal
        extractFields = maybe (-1) fst . toMaybe . T.hexadecimal . last . T.splitOn ":"
        toMaybe (Left _) = Nothing
        toMaybe (Right a) = Just a

instructionProps :: TestTree
instructionProps = testGroup "TracingInstructions" [
    testProperty "parseUrlPiece . toHeader == pure" $
        \ti -> pure ti == (parseUrlPiece $ instructionsToHeader ti)
    ]


instance Arbitrary TracingInstructions where
    arbitrary = do
        traceId <- TraceId <$> pa
        spanId <- SpanId <$> pa
        parentSpanId <- SpanId <$> pa
        debug <- arbitrary
        sample <- arbitrary
        pure TracingInstructions {
            traceId,
            spanId,
            parentSpanId,
            debug,
            sample
            }
        where
            pa = suchThat arbitrary (>= 0)

