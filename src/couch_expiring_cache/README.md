# Couch Expiring Cache

This is a library for creating an FDB backed key value cache, where
each entry has a `stale` and `expires` time associated with it. Once
the current time exceeds the `expires` time, the entry is
automatically removed. The `stale` time can be used to indicate that a
refresh is necessary, while still returning a non-expired value. It is
potentially useful for implementing e.g. caches to external systems of
record, such as OAuth 2.

The data model is based on this [FDB forum discussion](
https://forums.foundationdb.org/t/designing-key-value-expiration-in-fdb/156).

```
(?EXPIRING_CACHE, Name, ?PK, Key) := (Val, StaleTS, ExpireTS)
(?EXPIRING_CACHE, Name, ?EXP, ExpireTS, Key) := ()
```
where `Name` is a unique namespace for a particular use case. N.B.
that it's possible for cache data remain indefinitely in FDB when a
`Name` is changed or retired with unexpired entries. For such cases,
we provide `couch_expiring_cache_fdb:clear_all/1` to manually clean
up those entries.

## Example

Typical usage for this library is to create a separate behaviour
module for each `Name`, which internally starts a uniquely named
`couch_expiring_cache_server` to handle expiration and removal of
entries for that `Name`. For example, to cache authorization decisions
from an external source, one could implement a module like the
following:

```erlang
-module(auth_fdb_decision_cache).

-behaviour(couch_expiring_cache_server).

-export([
    start_link/0
]).


-define(CACHE_NAME, <<"auth-decision">>).


start_link() ->
    Opts = #{
        cache_name => ?CACHE_NAME,
        period => 1000, % clear expired entries every second
        batch_size => 500, % clear at most 500 entries each period
        max_jitter => 10
    },
    couch_expiring_cache_server:start_link(?MODULE, Opts).
```

## Modules

* `couch_expiring_cache`: The API module, it contains functions for
  inserting and looking up cache entries, which are simply
  pass-throughs to `couch_expiring_cache_fdb`.

* `couch_expiring_cache_fdb`: The module which interacts with FDB, in
  addition to insertion and lookup functions, it also contains a
  function to clear an expired range, which is called periodically
  from instances of `couch_expiring_cache_server`.

* `couch_expiring_cache_server`: An "abstract" gen_server, a specific
  behaviour of this module should be created for each `Name`, which
  can override the default expiration parameters. It periodically
  removes expired cache entries using configurable parameters for
  period, jitter, and batch size.
