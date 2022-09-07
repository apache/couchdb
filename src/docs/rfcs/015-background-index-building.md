---
name: Formal RFC
about: Submit a formal Request For Comments for consideration by the team.
title: 'Background index building'
labels: rfc, discussion
assignees: ''

---

# Introduction

This document describes the design for the background index builder in CouchDB 4.

## Abstract

Background index builder monitors databases for changes and then kicks off
asynchronous index updates. It is also responsible for removing stale indexing
data.

## Requirements Language

The key words "MUST", "MUST NOT", "REQUIRED", "SHALL", "SHALL NOT", "SHOULD",
"SHOULD NOT", "RECOMMENDED", "MAY", and "OPTIONAL" in this document are to be
interpreted as described in [RFC
2119](https://www.rfc-editor.org/rfc/rfc2119.txt).

---

# Detailed Description

The two main components of the background index builder are:
 1) The notification mechanism
 2) Index building behavior API and registration facility

The notification mechanism monitors databases for updates and the secondary
index applications register with the background indexer and provide an
implementation of the index building API.

## Database Updates Notifications

After each document update transaction finishes, the background indexer is
notified via a callback. The indexer then bumps the timestamp for that database
in a set of sharded ETS tables. Each sharded ETS table has an associated
background process which periodically removes entries from there and calls the
index building API functions for each registered indexing backend.

In addition to building indices, the background index builder also cleanups up
stale index data. This is index data left behind after design documents have
been updated or deleted and the view signatures changed.

Background index building and cleaning may be enabled or disabled with
configuration options. There is also a configurable delay during which db
updates would accumulate for each database. This is used to avoid re-scheduling
`couch_jobs` too often.

## Background Index Building Behavior

Unlike CouchDB 3 (`ken`), the background index builder in CouchDB 4 doesn't
have centralized knowledge of all the possible secondary indices. Instead, each
secondary indexing application may register with the background index builder
and provide a set of callbacks implementing background index building for their
particular index types.


Background index building behavior is a standard Erlang/OTP behavior defined
as:

```
-callback build_indices(Db :: map(), DDocs :: list(#doc{})) ->
    [{ok, JobId::binary()} | {error, any()}].

-callback cleanup_indices(Db :: map(), DDocs :: list(#doc{})) ->
    [ok | {error, any()}].
```

Each indexing application, may register with the index builder by using
`fabric2_index:register(Module)` function. When it registers, it must provide
an implementation of that behavior in that module.

 * `build_indices/2`: must inspect all the passed in design doc bodies and
trigger asynchronous index updates for the all views that module is responsible
for.

 *`cleanup_indices/2`: must clean up all the stale indexing data associated
with all the views in the design docs passed in as an argument.

# Advantages and Disadvantages

 * Main advantage is simplicity. Rely on node-local updates and the fact that
   all indexing is currently backed by `couch_jobs` jobs, which handle global
   locking and coordination.

 * Main disadvantage is also simplicity. There is no concept of priority to
   allow users to build some indices before others.

# Key Changes

Configuration format has changed. Instead of configuring background index
building in the `[ken]` section, it is now configured in the `[fabric]` config
section. Otherwise there are no external API changes.

## Applications and Modules affected

 * fabric2_index
 * fabric2_db
 * couch_views

## HTTP API additions

N/A

## HTTP API deprecations

N/A

# Security Considerations

None

# References

[fabric2_index](https://github.com/apache/couchdb/blob/prototype/fdb-layer/src/fabric/src/fabric2_index.erl)
[ken](https://github.com/apache/couchdb/tree/master/src/ken)

# Co-authors

 * @davisp

# Acknowledgements

 * @davisp
