---
name: Opentracing support
about: Adopt industry standard distributed tracing solution
title: 'Opentracing support'
labels: rfc, discussion
assignees: ''

---

Adopt an industry standard vendor-neutral APIs and instrumentation for distributed tracing.

# Introduction

Collecting profiling data is very tricky at the moment. 
Developers have to run generic profiling tools which are not aware of CouchDB specifics. 
This makes it hard to do the performance optimization work. We need a tool which would 
allow us to get profiling data from specific points in the codebase. 
This means code instrumentation. 

## Abstract

There is an https://opentracing.io/ project, which is a vendor-neutral API and instrumentation
for distributed tracing. In Erlang it is implemented by one of the following libraries:
 - [otters](https://github.com/project-fifo/otters) extended and more performant version of `otter`
 - [opentracing-erlang](https://github.com/opentracing-contrib/opentracing-erlang) `otter` version donated to opentracing project.
 - [original otter](https://github.com/Bluehouse-Technology/otter)
 - [passage](https://github.com/sile/jaeger_passage)
 
The opentracing philosophy is founded on three pillars:
- Low overhead: the tracing system should have a negligible performance impact on running services.
- Application-level transparency: programmers should not need to be aware of the tracing system
- Scalability

The main addition is to include one of the above mentioned libraries and add instrumentation points into the codebase.
In initial implementation, there would be a new span started on every HTTP request.
The following HTTP headers would be used to link tracing span with application specific traces.
- X-B3-ParentSpanId
- X-B3-TraceId
- b3

More information about the use of these headers can be found [here](https://github.com/openzipkin/b3-propagation).
Open tracing [specification](https://github.com/opentracing/specification/blob/main/specification.md) 
has a number of [conventions](https://github.com/opentracing/specification/blob/main/semantic_conventions.md) 
which would be good to follow.

In a nutshell the idea is:
- Take the reference to Parent span from one of the supported header and pass it to `span_start` call.
- Construct action name to use in `span_start` call.
- Call `span_start` from `chttpd:handle_request_int/1`.
- Pass span in `#httpd{}` record
- Pass `trace_id` and `parent_span_id` through the stack (extend records if needed)
- Attach span tags to better identify trace events.
- Attach span logs at important instrumentation points.
- Forward spans to external service.

## Requirements Language

The key words "MUST", "MUST NOT", "REQUIRED", "SHALL", "SHALL NOT",
"SHOULD", "SHOULD NOT", "RECOMMENDED",  "MAY", and "OPTIONAL" in this
document are to be interpreted as described in
[RFC 2119](https://www.rfc-editor.org/rfc/rfc2119.txt).

## Terminology

- [span](https://github.com/opentracing/specification/blob/1.1/specification.md#the-opentracing-data-model): The "span"
  is the primary building block of a distributed trace, representing an individual unit of work done in a distributed system.
  Each Span encapsulates the following state:
   - An operation name
   - A start timestamp
   - A finish timestamp
   - A set of zero or more key:value `Span Tags`. 
   - A set of zero or more structured logs (key:value `Span Logs`).
   - A `SpanContext`
   - `References` to zero or more causally-related `Spans`

---

# Detailed Description

## Selection of a library

As mentioned earlier, there are two flavours of libraries. None of them is perfect for all use cases.
The biggest differences in between `otters` and `passage` are:

|                                | otters      | passage                   |
| ------------------------------ | ----------- | ------------------------- |
| reporting protocol             | http        | udp                       |
| filtering                      | custom DSL  | sampling callback module  |
| reporter                       | zipkin only | jaeger or plugin          |
| functional API                 |      +      |             +             |
| process dictionary             |      +      |             +             |
| process based span storage     |      +      |             -             |
| send event in batches          |      +      |             -             |
| sender overload detection      |      -      |             +             |
| report batches based on        | timer       | spans of single operation |
| design for performance         |      +      |             -             |
| design for robustness at scale |      -      |             +             |
| counters                       |      +      |             -             |
| sampling based on duration     |      +      |             -             |
| number of extra dependencies   |      1      |             3             |

In order to allow future replacement of a tracing library it would be desirable to create an interface module `couch_trace`.
The `otters` library would be used for the first iteration.

## Configuration

The `otters` library uses application environment to store its configuration. 
It also has a facility to compile filtering DSL into a beam module.
The filtering DSL looks like following: `<name>([<condition>]) -> <action>.`. 
The safety of DSL compiler is unknown. Therefore a modification of tracing settings via configuration over HTTP wouldn't be possible.
The otter related section of the config `tracing.filters` would be protected by BLACKLIST_CONFIG_SECTIONS.
The configuration of tracing would only be allowed from remsh or modification of the ini file.
The configuration for otter filters would be stored in couch_config as follows:
```
[tracing.filters]

<name> = ([<condition>]) -> <action>.
```

## Tracing related HTTP headers

Following headers on the request would be supported 
- X-B3-ParentSpanId : 16 lower-hex characters
- X-B3-TraceId      :  32 lower-hex characters
- X-B3-SpanId       : 16 lower-hex characters
- b3 : {TraceId}-{SpanId}-{SamplingState}-{ParentSpanId}
  - the `SamplingState` would be ignored

Following headers on the response would be supported 
- X-B3-ParentSpanId : 16 lower-hex characters
- X-B3-TraceId      :  32 lower-hex characters
- X-B3-SpanId       : 16 lower-hex characters

## Conventions

The conventions below are based on [conventions from opentracing](https://github.com/opentracing/specification/blob/main/semantic_conventions.md#standard-span-tags-and-log-fields).
All tags are optional since it is just a recomendation from open tracing to hint visualization and filtering tools.

### Span tags

| Span tag name    | Type    | Notes and examples                                  |
| ---------------- | ------- | --------------------------------------------------- |
| component        | string  | couchdb.<app> (e.g. couchdb.chttpd, couchdb.fabric) |
| db.instance      | string  | for fdb-layer would be fdb connection string        |
| db.type          | string  | for fdb-layer would be fdb                          |
| error            | bool    | `true` if operation failed                          |
| http.method      | string  | HTTP method of the request for the associated Span  |
| http.status_code | integer | HTTP response status code for the associated Span   |
| http.url         | string  | sanitized URL of the request in URI format          |
| span.kind        | string  | Either `client` or `server` (RPC roles).            |
| user             | string  | Authenticated user name                             |
| db.name          | string  | Name of the accessed database                       |
| db.shard         | string  | Name of the accessed shard                          |
| nonce            | string  | Nonce used for the request                          |
 

### Log fields

| Span log field name | Type    | Notes and examples                          |
| ------------------- | ------- | ------------------------------------------- |
| error.kind          | string  | The "kind" of an error (error, exit, throw) |
| message             | string  | human-readable, one-line message            |
| stack               | string  | A stack trace (\n between lines)            |

## Multicomponent traces

CouchDB has complex architecture. The request handling crosses layers' and components' boundaries.
Every component or layer would start a new span. It *MUST* specify its parent span in order
for visualization tools to work. The value of a TraceId *MUST* be included in every span start.
The value of TraceId and SpanId *MAY* be passed to FDB when
[foundationdb#2085](https://github.com/apple/foundationdb/issues/2085) is resolved.

## Roadmap

- initial implementation as described in this document
- extend rexi to pass traceid and parentspanid
- redo otter configuration
- add tracing to server initiated jobs (compaction, replication)
- rewrite `otters_conn_zipkin:send_buffer/0` to make it more robust
- switch `otters_conn_zipkin` from `thrift` to `gRPC`


# Advantages and Disadvantages

## Drawbacks

Specifically for `otters` library there are following concerns:
- safety of configuration mechanism
- the robustness of the zipkin sender

## Advantages

- Ability to forward tracing events to external system for further analysis
- Low overhead
- Structured logging for span logs
- Link all events to same parent trace id

# Key Changes

- New configuration section
- New dependencies
- Additional HTTP headers
- Additional fields in some records

## Applications and Modules affected

- chttpd
- couch_trace (new module)

## HTTP API additions

Support for following headers would be added:
- X-B3-ParentSpanId
- X-B3-TraceId
- b3

## HTTP API deprecations

N/A

# Security Considerations

The security risk of injecting malicious payload into ini config is mitigated via placing the section into BLACKLIST_CONFIG_SECTIONS. 

# References

- [opentracing specification](https://github.com/opentracing/specification/blob/main/specification.md)
- https://opentracing.io/
- https://www.jaegertracing.io/docs/1.14/
- https://zipkin.io
- [opentracing conventions](https://github.com/opentracing/specification/blob/main/semantic_conventions.md) 


# Acknowledgements

[TIP]:  # ( Who helped you write this RFC? )
