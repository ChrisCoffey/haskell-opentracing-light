{-# LANGUAGE NamedFieldPuns, OverloadedStrings, DuplicateRecordFields #-}

module Instances where

import Servant.Tracing
import Tracing.Core

import Data.Char (isAscii)
import Test.QuickCheck hiding (sample)
import Data.Time.Clock.POSIX
import qualified Data.Map as M
import qualified Data.Text as T

instance Arbitrary TracingInstructions where
    arbitrary = do
        tid <- arbitrary
        sid <- arbitrary
        parentSpanId <- arbitrary
        dbg <- arbitrary
        sample <- arbitrary
        pure TracingInstructions {
            traceId=tid,
            spanId=sid,
            parentSpanId,
            debug=dbg,
            sample
            }
        where

positiveArb :: (Integral a, Arbitrary a) => Gen a
positiveArb = suchThat arbitrary (>= 0)

instance Arbitrary TraceId where
    arbitrary = TraceId <$> positiveArb

instance Arbitrary SpanId where
    arbitrary = SpanId <$> positiveArb

instance Arbitrary SpanContext where
    arbitrary = SpanContext <$> arbitrary <*> arbitrary

instance Arbitrary OpName where
    arbitrary = OpName <$> arbitrary

instance Arbitrary SpanRelation where
    arbitrary = do
        rel <- elements [ChildOf, FollowsFrom]
        rel <$> arbitrary

instance Arbitrary T.Text where
    arbitrary =
        T.pack <$> listOf (suchThat arbitrary isAscii)

-- TODO add tags and baggage once they're supported
instance Arbitrary Span where
    arbitrary = do
        o <- arbitrary
        c <- arbitrary
        rels <- arbitrary
        dbg <- arbitrary
        svc <- arbitrary
        pure Span {
            operationName = o,
            context = c,
            timestamp = 1522024571,
            duration = 123,
            relations = rels,
            tags = M.empty,
            baggage = M.empty,
            debug = dbg,
            serviceName = svc
            }

