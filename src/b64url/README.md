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

This implementation is faster than the Erlang version in OTP 26-28,
especially for larger binaries (1000+ bytes). To benchmark clone
erlperf repo and run `./benchmark.sh` script. In the future, it's
plausible Erlang OTP's base64 module may become faster than the NIF,
due to improvements in the JIT capabilities but it's not there yet.

```
./benchmark.sh

[...]

--- bytes: 100 -----
Code                   ||        QPS       Time   Rel
encode_otp_100          1    1613 Ki     620 ns  100%
encode_nif_100          1    1391 Ki     719 ns   86%
Code                   ||        QPS       Time   Rel
decode_nif_100          1    1453 Ki     688 ns  100%
decode_otp_100          1    1395 Ki     716 ns   96%

[...]

--- bytes: 1000 -----
Code                    ||        QPS       Time   Rel
encode_nif_1000          1     369 Ki    2711 ns  100%
encode_otp_1000          1     204 Ki    4904 ns   55%
Code                    ||        QPS       Time   Rel
decode_nif_1000          1     455 Ki    2196 ns  100%
decode_otp_1000          1     178 Ki    5612 ns   39%

[...]

--- bytes: 10000000 -----
Code                        ||        QPS       Time   Rel
encode_nif_10000000          1         45   22388 us  100%
encode_otp_10000000          1         19   51724 us   43%
Code                        ||        QPS       Time   Rel
decode_nif_10000000          1         55   18078 us  100%
decode_otp_10000000          1         17   60020 us   30%
```

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
