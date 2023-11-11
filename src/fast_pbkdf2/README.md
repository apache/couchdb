# Fast PBKDF2

[![Actions Status](https://github.com/esl/fast_pbkdf2/workflows/ci/badge.svg)](https://github.com/esl/fast_pbkdf2/actions)
[![codecov](https://codecov.io/gh/esl/fast_pbkdf2/branch/main/graph/badge.svg)](https://codecov.io/gh/esl/fast_pbkdf2)
[![Hex](http://img.shields.io/hexpm/v/fast_pbkdf2.svg)](https://hex.pm/packages/fast_pbkdf2)

`fast_pbkdf2` is an Erlang implementation of [PBKDF2][PBKDF2], where the algorithm is a carefully-optimised NIF that uses timeslicing and nif scheduling to respect the latency properties of the BEAM.
All OTP versions from OTP18 have been tested manually and should work correctly, but on CI we support only from 21.3

## Building
`fast_pbkdf2` is a rebar3-compatible OTP application, that uses the [port_compiler](https://github.com/blt/port_compiler) for the C part of the code.

Building is as easy as `rebar3 compile`, and using it in your projects as
```erlang
{deps,
 [{fast_pbkdf2, "1.0.0"}]}.
{plugins, [pc, rebar3_hex]}.
{provider_hooks,
 [{pre,
   [{compile, {pc, compile}},
    {clean, {pc, clean}}]}]}.
```

## Using

```erlang
DerivedPassword = fast_pbkdf2:pbkdf2(Hash, Password, Salt, IterationCount)
```
where `Hash` is the underlying hash function chosen as described by
```erlang
-type sha_type() :: crypto:sha1() | crypto:sha2().
```

### Custom `dkLen`
If what you desire is PBKDF2 with custom `dkLen`(I assume that if that is what you want, then you know your RFC), in a way that allows you to request longer derived keys, you may use `fast_pbkdf2:pbkdf2_block/5` with a given block index and do the indexing and chunking yourself, or use `fast_pbkdf2:pbkdf2/5` for the full algorithm. However, it doesn't really add much more entropy to the derived key to use outputs larger than the output of the underlying hash, so you might as well, use `pbkdf2` where dkLen is that of the hash's output, which is the same than `pbkdf2_block` with index `1`, which is simply the `pbkdf2/4` function.

## Performance

### The problem
PBKDF2 is a challenge derivation method, that is, it forces the client to compute a challenge in order to derive the desired password. But when the server implementation is slower than that of an attacker, it makes the server vulnerable to DoS by hogging itself with computations. We could see that on the CI and load-testing pipelines of [MongooseIM][MIM] for example.

### The solution
Is partial. We don't expect to have the fastest implementation, as that would be purely C code on GPUs, so unfortunately an attacker will pretty much always have better chances there. _But_ we can make the computation cheap enough for us that other computations —like the load of a session establishment— will be more relevant than that of the challenge; and also that other defence mechanisms like IP blacklisting or traffic shaping, will fire in good time.

### The outcome
On average it's 10x faster on the machines I've tested it (you can compare using the provided module in `./benchmarks/bench.ex`), but while the erlang implementation consumes memory linearly to the iteration count (1M it count with 120 clients quickly allocated 7GB of RAM, and 1M is common for password managers for example), the NIF implementation does not allocate any more memory. Also, the NIFS spend all of their time in user level alone, while the erlang one jumps to system calls in around ~2% of the time (I'd guess due to some heavy allocation and garbage collection patterns).

## Credit where credit is due
The initial algorithm and optimisations were taken from Joseph Birr-Pixton's
[fastpbkdf2](https://github.com/ctz/fastpbkdf2)'s repository.

## Read more:
* Password-Based Cryptography Specification (PBKDF2): [RFC8018](https://tools.ietf.org/html/rfc8018#section-5.2)
* HMAC: [RFC2104]( https://tools.ietf.org/html/rfc2104)
* SHAs and HMAC-SHA: [RFC6234](https://tools.ietf.org/html/rfc6234)

[MIM]: https://github.com/esl/MongooseIM
[PBKDF2]: https://tools.ietf.org/html/rfc8018#section-5.2
