
import Servant.TracingSpec (tracingProps, tracingSpecs)
import Jaeger.ClientSpec (jaegerProps, jaegerSpec)
import Test.Tasty

main :: IO ()
main = defaultMain $ testGroup "Tracing"  [
    tracingProps,
    tracingSpecs,
    jaegerProps,
    jaegerSpec
    ]
