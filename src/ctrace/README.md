Overview
========

This application provides an interface to opentracing compatible
tracing systems.

Open Tracing
------------

[//]: # (taken from https://github.com/opentracing/specification/blob/master/specification.md)
Traces in OpenTracing are defined implicitly by their Spans.
In particular, a Trace can be thought of as a directed acyclic
graph (DAG) of Spans, where the edges between Spans are called
References.

Each Span encapsulates the following state:

- An operation name
- A start timestamp
- A finish timestamp
- A set of zero or more key:value Span Tags.
- A set of zero or more Span Logs, each of which is
  itself a key:value map paired with a timestamp.
- A SpanContext
- References to zero or more causally-related Spans

Every trace is identified by unique trace_id. Every trace includes zero
or more tracing spans which are identified by a span id.

Jaeger
------

Jaeger is a distributed tracing system released as open source by
Uber Technologies. It is one of implementations of open tracing specification.
Jaeger supports Trace detail view where a single trace is represented as
a tree of tracing span with detailed timing information about every span.
In order to make this feature work all tracing spans should form a lineage
from the same root span.


Implementation
==============

Every operation has unique identifier. Example identifiers are:

- all-dbs.read
- database.delete
- replication.trigger
- view.compaction

Tracing begins with a root span that can be filtered based on
a set of configurable rules. When the root trace is created these
rules are applied to see if the trace should be generated and logged.
If a trace is disabled due to filtering then no trace data is generated.


Code instrumentation
--------------------

The span lifecycle is controled by

- `ctrace:start_span`
- `ctrace:finish_span`
- `ctrace:with_span`

The instrumentation can add tags and logs to a span.

Example of instrumentation:

```
ctrace:with_span('database.read', #{'db.name' => <<>>}, fun() ->
    ctrace:tag(#{
        peer => Peer,
        'http.method' => Method,
        nonce => Nonce,
        'http.url' => Path,
        'span.kind' => <<"server">>,
        component => <<"couchdb.chttpd">>
    }),
    ctrace:log(#{
        field0 => "value0"
    })

    handle_request(HttpReq)
end),
```

As you can see the `ctrace:with_span/3` function receives a function which
wraps the operation we wanted to trace:

- `ctrace:tag/1` to add new tags to the span
- `ctrace:log/1` add log event to the span

There are some informative functions as well:

- `ctrace:refs/0` - returns all other spans we have references from the current
- `ctrace:operation_name/0` - returns operation name for the current span
- `ctrace:trace_id/0` - returns trace id for the current span
- `ctrace:span_id/0` - returns span id for the current span

Instrumentation guide
---------------------

- Start root span at system boundaries
  - httpd
  - internal trigger (replication or compaction jobs)
- Start new child span when you cross layer boundaries
- Start new child span when you cross node bounadary
- Extend `<app>_httpd_handlers:handler_info/1` as needed to
  have operation ids. (We as community might need to work on
  naming conventions)
- Use [span conventions](https://github.com/apache/couchdb-documentation/blob/master/rfcs/011-opentracing.md#conventions) https://github.com/opentracing/specification/blob/master/semantic_conventions.md
- When in doubt consult open tracing spec
  - [spec overview](https://github.com/opentracing/specification/blob/master/specification.md)
  - [conventions](https://github.com/opentracing/specification/blob/master/semantic_conventions.md#standard-span-tags-and-log-fields)

Configuration
-------------

Traces are configured using standard CouchDB ini file based configuration.
There is a global toggle `[tracing] enabled = true | false` that switches
tracing on or off completely. The `[tracing]` section also includes
configuration for where to send trace data. There are two reporters which we
support.

The thrift over udp reporter (this is the default) has following configuration
options:

- protocol = udp
- thrift_format = compact | binary
- agent_host = 127.0.0.1
- agent_port = 6831

The thrift over http has following options

- protocol = http
- endpoint = http://127.0.0.1:14268

An example of `[tracing]` section

```ini
[tracing]

enabled = true
thrift_format = compact ; compact | binary
agent_host = 127.0.0.1
agent_port = 6831
app_name = couchdb ; Value to use for the `location.application` tag
```

In the `[tracing.filters]` section we can define a set of rules for
whether to include a trace. Keys are the operation name of the root
span and values are a simple DSL for whether to include the given
span based on its tags. See below for a more thorough description
of the DSL. The `all` key is special and is used when no other
filter matches a given operation. If the `all` key is not present
then ctrace behaves as if it were defined as `(#{}) -> false`. I.e.,
any trace that doesn't have a configuration entry is not generated
and logged.

```ini
[tracing.filters]
; trace all events
; all = (#{}) -> true
; trace all events with X-B3-... headers
; all = (#{external := External}) when External == true -> true
; database-info.read = (#{'http.method' := Method}) when Method == 'GET' -> true
; view.build = (#{'view.name' := Name}) when Name == "foo" -> 0.25
```

Filter DSL Description
---

```
<operation_name> = ( #{<[arguments]>} ) when <[conditions]> -> <[actions]>
```

Where:
 - operation_name is the name of the root span
 - arguments is comma separated pairs of
   `<tag_or_field_name> := <variable_name>`
 - actions is a list which contains
   - `report`
 - conditions
   - `<[condition]>`
   - `| <[condition]> <[operator]> <[condition]>`
 - condition:
   - `<variable_name> <[operator]> <value>`
     `| <[guard_function]>(<[variable_name]>)`
 - `variable_name` - lowercase name without special characters
 - guard_function: one of
   - `is_atom`
   - `is_float`
   - `is_integer`
   - `is_list`
   - `is_number`
   - `is_pid`
   - `is_port`
   - `is_reference`
   - `is_tuple`
   - `is_map`
   - `is_binary`
   - `is_function`
   - `element` - `element(n, tuple)`
   - `abs`
   - `hd` - return head of the list
   - `length`
   - `map_get`
   - `map_size`
   - `round`
   - `node`
   - `size` - returns size of the tuple
   - `bit_size` - returns number of bits in binary
   - `byte_size` - returns number of bytes in binary
   - `tl` - return tail of a list
   - `trunc`
   - `self`
 - operator: one of
   - `not`
   - `and` - evaluates both expressions
   - `andalso` - evaluates second only when first is true
   - `or` - evaluates both expressions
   - `orelse` - evaluates second only when first is false
   - `xor`
   - `+`
   - `-`
   - `*`
   - `div`
   - `rem`
   - `band` - bitwise AND
   - `bor` - bitwise OR
   - `bxor` - bitwise XOR
   - `bnot` - bitwise NOT
   - `bsl` - arithmetic bitshift left
   - `bsr` - bitshift right
   - `>`
   - `>=`
   - `<`
   - `=<`
   - `=:=`
   - `==`
   - `=/=`
   - `/=` - not equal


b3 propagation
--------------

In order to correlate spans across multiple systems the information
about parent span can be passed via headers. Currently the chttpd
application is responsible for extracting and parsing the header.
The ctrace application provides following facilities to enable this
use case:

- `{root, RootSpan}` option for `ctrace:start_span/2`
- `ctrace:external_span/3` to convert references to a root span

The span references could be set either via `b3` header of via
individual headers. In case when individual headers are used the
following set of headers is supported:

- X-B3-TraceId (32 lower-hex characters)
- X-B3-SpanId (16 lower-hex characters)
  (has no effect if X-B3-TraceId is not set)
- X-B3-ParentSpanId (16 lower-hex characters)
  (has no effect if X-B3-TraceId is not set)

Alternatively a single `b3` header could be used. It has to be
in the following format:

b3={TraceId}-{SpanId}-{SamplingState}-{ParentSpanId}

Where SamplingState is either `0` or `1`. However we ignore the value.

Note: We only support 128 bit TraceId's.

Developing
==========

Here we provide a list frequently used commands
useful while working on this application.


1. Run all tests
```
make setup-eunit
make && ERL_LIBS=`pwd`/src BUILDDIR=`pwd` mix test --trace src/chttpd/test/exunit/ src/ctrace/test/exunit/
```

2. Run tests selectively
```
make && ERL_LIBS=`pwd`/src BUILDDIR=`pwd` mix test --trace src/chttpd/test/exunit/ctrace_context_test.exs:59
```

3. Re-run only failed tests
```
make && ERL_LIBS=`pwd`/src BUILDDIR=`pwd` mix test --failed --trace src/chttpd/test/exunit/ src/ctrace/test/exunit/
```

4. Running jaeger in docker
```
docker run -d --net fdb-core --name jaeger.local   -p 6831:6831/udp   -p 16686:16686   jaegertracing/all-in-one:1.14
```

If Docker isn't your cup of tea, the Jaeger project also provides
prebuilt binaries that can be downloaded. On macOS we can easily
setup a development Jaeger instance by running the prebuilt
`jaeger-all-in-one` binary without any arguments.