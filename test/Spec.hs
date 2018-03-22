
import Servant.TracingSpec (tracingProps, tracingSpecs)
import Test.Tasty

main :: IO ()
main = defaultMain $ testGroup "Tracing"  [
    tracingProps,
    tracingSpecs
    ]
