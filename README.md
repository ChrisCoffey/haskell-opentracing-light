# servant-tracing

[![Build Status](https://travis-ci.org/ChrisCoffey/haskell-opentracing-light.svg?branch=master)](https://travis-ci.org/ChrisCoffey/haskell-opentracing-light)

This repository is the minimum required for publishing trace data to Zipkin or Jaeger. It adheres to the [Open Tracing Standard](https://github.com/opentracing/specification) but is missing a few features. See the documentation on Hackage for module-level details.


### Using the library

The OpenTracing standard revolves around a single function, `recordSpan`. `recordSpan` is responsible for creating new spans (see the standard for the definition of a span) and ensuring child spans use the new id. In order to properly build this tree of calls library users must provide the necessary environment via a `MonadTracer` instance (see haddocks). Library users are responsible for defining their own publish loop. There is a default `Zipkin` publisher in `Tracing.Zipkin` which works with Jaeger & Zipkin, but the loop to drain the `spanBuffer` must be provided by the user.

```
foo :: (MonadIO m, MonadTracer m) =>
    Int
    -> m String
foo str = recordSpan
    Nothing
    [Tag "Ultimate Answer to Life, The Universe and Everything", Tag 42]
    "Compute Ultimate Question"
    $ pure "Oops"
```

The code above logs a new span to the `spanBuffer`, where it will sit until published. If it turns out that `foo` is called from an active span, then it will be recorded as a child of said higher span.

### Testing Locally with the Demo App

You can start up a compatible server for [Zipkin](https://zipkin.io/pages/quickstart.html) or [Jaeger](https://jaegertracing.netlify.com/docs/deployment/) via a standalone docker container. From there its a matter of seting the following environment variables:
- *TRACING_ENDPOINT*: a `String` with the fully url to a running tracing server. For example, `http://localhost:9411/api/v2/spans` to publish to a Zipkin endpoint.
- *TRACING_SERVICE*: a `String` name for your service

Once the a tracing server & the example service are running, you can interact with it via your favorite REST client. The api expects a header named `Auth`, and has two top level endpoints: `fast` & `slow`. Here's an example request: `curl localhost:8080/slow -HAUTH="foo"`


#### Pending Features
- Thrift support
- Additional clients
- Pluggable samplers
