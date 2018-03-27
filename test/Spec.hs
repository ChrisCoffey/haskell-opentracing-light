
import Servant.TracingSpec (tracingProps, tracingSpecs)
import Zipkin.ClientSpec (zipkinProps, zipkinSpec)
import Tracing.CoreSpec (coreSpec, coreProps)
import Test.Tasty

main :: IO ()
main = defaultMain $ testGroup "Tracing"  [
    tracingProps,
    tracingSpecs,
    zipkinProps,
    zipkinSpec,
    coreSpec
    ]
