# Base64 encoder with URL-safe scheme

[![CI](https://github.com/apache/couchdb-b64url/actions/workflows/ci.yml/badge.svg)](https://github.com/apache/couchdb-b64url/actions/workflows/ci.yml)

This is a simple NIF that is responsible for quickly encoding and
decoding Base64 URL values:

```erlang
1> Thing = b64url:encode("Hello, CouchDB!").
<<"SGVsbG8sIENvdWNoREIh">>
2> b64url:decode(Thing).
<<"Hello, CouchDB!">>
```

## Performance

This implementation is significantly faster than the Erlang version it replaced
in CouchDB. The `benchmark.escript` file contains the original implementation
(using regular expressions to replace unsafe characters in the output of the
`base64` module) and can be used to compare the two for strings of various
lengths. For example:

```
ERL_LIBS=_build/default/lib/b64url/ ./test/benchmark.escript 4 10 100 30
erl :       75491270 bytes /  30 seconds =     2516375.67 bps
nif :      672299342 bytes /  30 seconds =    22409978.07 bps
```

This test invocation spawns four workers that generate random strings between 10
and 100 bytes in length and then perform an encode/decode on them in a tight
loop for 30 seconds, and then reports the aggregate encoded data volume. Note
that the generator overhead (`crypto:strong_rand_bytes/1`) is included in these
results, so the relative difference in encoder throughput is rather larger than
what's reported here.

## Timeslice Consumption

NIF implementations must take care to avoid doing [lengthy
work](https://www.erlang.org/doc/man/erl_nif.html#lengthy_work) in a single
invocation. This library will yield back to the Erlang VM as needed when
operating on a large string, maintaining a partial result until it can resume
operation. The current implementation uses a conservative heuristic that
estimates 64 bytes of encoding / decoding to consume 1% of a timeslice, so input
strings shorter than ~6k should be encoded / decoded within a single invocation,
and the library should not adversely affect the responsiveness of the VM in any
way.
