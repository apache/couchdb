# snappy

Erlang NIF for Google's [snappy](https://github.com/google/snappy)

This is used for `file_compression = snappy`, the default, in
`couch_compress`.

## Vendoring

`c_src/snappy` has the sources from upstream with two headers to replace the
upstream CMake generated ones, we don't want to add CMake as another build
dependency just for this:

 * `config.h`, from `cmake/config.h.in`. Seeting come from compiler
   macros: byte order, SSSE3 or NEON support and the GCC/clang builtins
 * `snappy-stubs-public.h`, from `snappy-stubs-public.h.in`, with the version
   filled in and `<sys/uio.h>` used everywhere except Windows.

### Updating

 1. Check out https://github.com/google/snappy.
 2. Copy `COPYING`, `NEWS`, `snappy.cc`, `snappy.h`, `snappy-internal.h`,
    `snappy-sinksource.cc`, `snappy-sinksource.h`, `snappy-stubs-internal.cc`
    and `snappy-stubs-internal.h` into `c_src/snappy` (skip other *.cc files)
 3. Update version in `snappy-stubs-public.h`. Compare with `config.h` with the
    new `snappy-stubs-public.h.in` and `cmake/config.h.in`.
 4. Run `make eunit apps=snappy`. The tests decode frames written by snappy
    1.1.3, which CouchDB bundled before. This ensure data on disk stays
    readable.
