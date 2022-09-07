---
name: Formal RFC
about: Submit a formal Request For Comments for consideration by the team.
title: Implementation of _all_docs DB info metadata in FoundationDB
labels: rfc, discussion
assignees: ''

---

# Introduction

## Abstract

This document describes how to maintain an index of all the documents in a
database backed by FoundationDB, one sufficient to power the _all_docs endpoint.
It also addresses the individual metadata fields included in the response to a
GET /dbname request.

## Requirements Language

[NOTE]: # ( Do not alter the section below. Follow its instructions. )

The key words "MUST", "MUST NOT", "REQUIRED", "SHALL", "SHALL NOT",
"SHOULD", "SHOULD NOT", "RECOMMENDED",  "MAY", and "OPTIONAL" in this
document are to be interpreted as described in
[RFC 2119](https://www.rfc-editor.org/rfc/rfc2119.txt).

## Terminology

[TIP]:  # ( Provide a list of any unique terms or acronyms, and their definitions here.)

---

# Detailed Description

## _all_docs

Normal requests to the `_all_docs` index will be powered by a dedicated subspace
containing a single key for each document in the database that has at least one
deleted=false entry in the revisions subspace. This dedicated subspace can be
populated by blind writes on each update transaction, as the revisions subspace
ensures proper coordination of concurrent writers trying to modify the same
document. The structure of the keys in this space looks like

```
(?BY_ID, DocID) = (ValueFormat, RevPosition, RevHash)
```

where the individual elements are defined as follows:

* ValueFormat: enum for the value encoding, to enable schema evolution
* DocID: the document ID
* RevPosition: positive integer encoded using standard tuple layer encoding
* RevHash: 16 bytes uniquely identifying the winning revision of this document

If a transaction deletes the last "live" edit branch of a document, it must also
clear the corresponding entry for the document from this subspace.

A request that specifies `include_docs=true` can be implemented either by
performing a range request against this subspace and then N additional range
requests explicitly specifying the full revision information in the ?DOCS
subspace, or by doing a full range scan directly against that subspace,
discarding conflict bodies and any user data associated with deleted revisions.
As the implementation choice there has no bearing on the actual data model we
leave it unspecified in this RFC.

## dbinfo

The so-called "dbinfo" JSON object contains various bits of metadata about a
database. Here's how we'll carry those forward:

`db_name`: should be trivially accessible.

`doc_count`: this will be maintained as a single key mutated using
FoundationDB's atomic operations. Transactions that create a new document or
re-create one where all previous edit branches had been deleted should increment
the counter by 1.

`doc_del_count`: as above, this is a key mutated using atomic operations.
Transactions that tombstone the last deleted=false edit branch on a document
should increment it by 1. Transactions that add a new deleted=false edit branch
to a document where all previous edit branches were deleted must decrement it by
1.

The revisions model ensures that every transaction has enough information to
know whether it needs to modify either or both of the above counters.

`update_seq`: the most efficient way to retrieve this value is to execute a
`get_key` operation using a `last_less_than` KeySelector on the end of the
?CHANGES subspace, so no additional writes are required.

`purge_seq`: TBD on a more detailed design for purge. If it ends up being
entirely transactional then this could be fixed to `update_seq` or dropped
entirely.

### Data Sizes

There are three distinct sizes that we currently track for every database:

* `sizes.external`: described as the "number of bytes that would be required to
  represent the contents outside of the database".
* `sizes.active`: a theoretical minimum number of bytes to store this database
  on disk.
* `sizes.file`: the current number of bytes on disk.

The relationship between `sizes.active` and `sizes.file` is used to guide
decisions on database compaction. FoundationDB doesn't require compaction, and
any distinction that might exist between these two quantities (e.g. from storage
engine compression) is not surfaced up to the clients, so it probably doesn't
make sense to have both.

The current implementation of `sizes.external` does *not* measure the length of
a JSON representation of the data, but rather the size of an uncompressed Erlang
term representation of the JSON. This is a somewhat awkward choice as the
internal Erlang term representation is liable to change over time (e.g. with the
introduction of Maps in newer Erlang releases, or plausibly even a JSON decoder
that directly emits the format defined in the document storage RFC).

Assuming we can agree on a set of sizes and how they should be calculated, the
implementation will require two pieces: a single key for each size, mutated by
atomic operations, and a record of the size of each revision in the ?REVISIONS
subspace so that a transaction can compute the delta for each document.

### Clustering

The `r`, `w`, `q`, and `n` values in the `cluster` object were introduced in
CouchDB 2.x to describe the topology of a database and the default quorum
settings for operations against it. If we wanted to bring these forward, here's
how they'd be defined:

* `r`: always fixed at 1

* `w`: interpreted as the number of transaction logs that record a commit, this
  is dependent on the `redundancy mode` for the underlying FoundationDB database

* `n`: interpreted as number of storage servers that host a key, this is also
  dependent on the `redundancy mode` for the underlying FoundationDB database

* `q`: the closest analogue here would be to use the `get_boundary_keys` API and
  report number of distinct ranges implied by the boundary keys

This interpretation could lead to some surprises, though. For example, "r=1,
w=4, n=3" is a popular configuration, but this is nonsensical for someone
expecting to see Dynamo-style numbers. Ignoring backwards compatibility, the
sensible thing is to point users toward the actual FoundationDB configuration
information, and to deprecate this entire `cluster` object. Open for discussion.

# Advantages and Disadvantages

[NOTE]: # ( Briefly, list the benefits and drawbacks that would be realized should )
[NOTE]: # ( the proposal be accepted for inclusion into Apache CouchDB. )

# Key Changes

The underlying transaction in FoundationDB must complete within 5 seconds, which
implicitly limits the number of results that can be returned in a single
_all_docs invocation.

## Applications and Modules affected

TBD depending on exact code layout going forward.

## HTTP API additions

None.

## HTTP API deprecations

The `total_rows` and `offset` fields are removed from the response to
`_all_docs`, which now has the simpler form

    {"rows": [
        {"id":"foo", "key":"foo", "value":{"rev":"1-deadbeef..."}},
        ...
    ]}

The following fields are removed in the dbinfo response:

* `compact_running`

* `disk_format_version`: this is a tricky one. We define "format versions" for
  every single type of key we're storing in FoundationDB, and those versions
  could vary on a key-by-key basis, so listing a single number for an entire
  database is sort of ill-posed. 


The following fields are already marked as deprecated and can be removed in the
next major release, independent of the FoundationDB work:

* `instance_start_time`
* `other`
* `data_size`
* `disk_size`


# Security Considerations

None have been identified.

# References

[TIP]:  # ( Include any references to CouchDB documentation, mailing list discussion, )
[TIP]:  # ( external standards or other links here. )

# Acknowledgements

[TIP]:  # ( Who helped you write this RFC? )