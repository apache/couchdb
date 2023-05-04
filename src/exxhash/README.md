exxhash
=======

https://cyan4973.github.io/xxHash/

xxHash is an extremely fast non-cryptographic hash algorithm. The idea is to
use it replace MD5 throughout Apache CouchDB codebase.

This is not a complete implementation of xxHash it only implement the basic 128
bit hash API.

API
===

```
exxhash:xxhash128(Binary)
```

Updating
===

xxHash was vendored from https://cyan4973.github.io/xxHash/
with commit SHA f4bef929aa854e9f52a303c5e58fd52855a0ecfa

Only these two files are used from the original library:
  `c_src/xxhash.h`
  `c_src/xxhash.c`
