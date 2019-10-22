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

Every trace is identified by unique trace_id.
Every trace includes zero or more tracing spans.
Which are identified by span id.

Jaeger
------

Jaeger is a distributed tracing system released as open source by Uber Technologies.
It is one of implementations of open tracing specification.
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

The tracing begins with sampling. We don't do anything if
sampler is not configured for a given operation.

For every operation we start a separate tracing filter process.
This process receieve spans when `ctrace:finish_span` is called.
The filtering processes are named after operation name.

Every filtering process has a number of filtering rules.
The first rule matching the conditions of a given span would be selected.
Every rule has a list of actions to execute on given span.

Currently we only support `report`. In the future we might support following:

- report
- count
- sample = probability (float)

When the rule is selected and it has `report` action we forward the span to
`reporter` process. Reporter process encodes the span and sends it to jaeger.

Span pipeline
-------------

+-------+       +------+       +--------+       +------+
|sampler| ----> |filter| ----> |reporter| ----> |jaeger|
+-------+       +------+       +--------+       +------+

- sampler - adds filtering rules into span and does prefiltering
- filter - uses filtering rules to decide if it needs to forward span
- reporter - sends span to jaeger

Code instrumentation
--------------------

The span lifecycle is controled by

- `ctrace:start_span`
- `ctrace:finish_span`

The instrumentation can add tags and logs to a span. In some cases we
embed span in other structures. Therefore to avoid confussion we don't 
use term `span` and use `subject` instead. Currently we support `#httpd{}`
record and `db` as subjects.

Example of instrumentation:
```
HttpReq2 = ctrace:trace(HttpReq1, fun(S0) ->
    S1 = ctrace:tag(S0, #{
        peer => Peer,
        'http.method' => Method,
        nonce => Nonce,
        'http.url' => Path,
        'span.kind' => <<"server">>,
        component => <<"couchdb.chttpd">>
    }),
    ctrace:log(S1, #{
        field0 => "value0"
    })
end``1),
```

As you can see the `ctrace:trace/2` function receives a function which
operates on the span. The functions that can be on span are:

- `ctrace:tag/2` to add new tags to the span
- `ctrace:set_operation_name/2` sometimes operation name is
  not available when span is started. This function let
  us set the name latter.
- `ctrace:log/2` add log event to the span

There are some informative functions as well:

- `ctrace:refs/1` - returns all other spans we have references from the current
- `ctrace:operation_name/1` - returns operation name for the current span
- `ctrace:trace_id/1` - returns trace id for the current span
- `ctrace:span_id/1` - returns span id for the current span

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
- Use `ctrace:new_request_ctx` to pass additional information
  about request.
- Update layers to pass `request_ctx` as needed (not done for jobs).
- Use [span conventions](https://github.com/apache/couchdb-documentation/blob/master/rfcs/011-opentracing.md#conventions) https://github.com/opentracing/specification/blob/master/semantic_conventions.md
- When in doubt consult open tracing spec
  - [spec overview](https://github.com/opentracing/specification/blob/master/specification.md)
  - [conventions](https://github.com/opentracing/specification/blob/master/semantic_conventions.md#standard-span-tags-and-log-fields)

Configuration
-------------

The tracers are configured using standard CouchDB ini files
based configuration. There is a global toggle
`[tracing]->'enabled' = false` which enables the tracing.
The samplers are configured in a `[tracing.samplers]` section, which
specifies the sampler to use for given tracer. If sampler is not
configured the spans for a given operation are droped. Every sampler
must have a corespondent filter section. The naming convention is:
`[tracing.OperationId]`. For security reasons `[tracing.OperationId]`
is not available via HTTP endpoint. Administrator can toggle tracing
with predefined rules for specific operation by setting a correspondent
sampler to either `none` or `all`.
The keys in filter section are irrelevant and used only for ordering
purpose. The rules are processed in the alphabetical order. We use a
DSL for defining rules. The DSL has following structure:
```
( #{<[arguments]>} ) when <[conditions]> -> <[actions]>
```

Where:
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
 - guarg_function: one of
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

### Open tracing agent configuration

```
[tracing]

thrift_format = compact ; compact | binary
agent_host = 127.0.0.1
agent_port = 6831
; app_name is the value whicj would be used for
; `location.application` tag
app_name = couchdb
```


Bellow is an example of a configuration:

```ini
[tracing]
enabled = true
thrift_format = compact ; compact | binary
agent_host = jaeger.local
agent_port = 6831
; app_name is the value whicj would be used for
; `location.application` tag
app_name = couchdb

[tracing.samplers]

view.build = all
database-info.read = all

[tracing.view.build]

a_select = (#{'view.name' := Name}) when Name == "blablabla" -> [report]
details = (#{parent := Parent}) when Parent == <<"view.build">> -> [report]

[tracing.database-info.read]

select = (#{'http.method' := Method}) when Method == 'GET' -> [report]
details = (#{parent := Parent}) when Parent == <<"database-info.read">> -> [report]
```

Note: It is important to add `details = (#{parent := Parent}) when Parent == <<"database-info.read">> -> [report]`
rule if you wanted to report children spans.


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