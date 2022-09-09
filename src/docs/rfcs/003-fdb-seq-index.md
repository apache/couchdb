---
name: Formal RFC
about: Submit a formal Request For Comments for consideration by the team.
title: 'Data Model and Index Management for _changes in FoundationDB'
labels: rfc, discussion
assignees: ''

---

[NOTE]: # ( ^^ Provide a general summary of the RFC in the title above. ^^ )

# Introduction

Data Model and Index Management for `_changes` in FoundationDB

## Abstract

This document describes how to implement the `by_seq` index that supports the
`_changes` endpoints in FoundationDB. It covers the data model, index
maintenance, and access patterns.

The basic data model is one where the key is a `Sequence` (as defined below) and
the value is a document ID, revision, and branch count.

## Requirements Language

[NOTE]: # ( Do not alter the section below. Follow its instructions. )

The key words "MUST", "MUST NOT", "REQUIRED", "SHALL", "SHALL NOT",
"SHOULD", "SHOULD NOT", "RECOMMENDED",  "MAY", and "OPTIONAL" in this
document are to be interpreted as described in
[RFC 2119](https://www.rfc-editor.org/rfc/rfc2119.txt).

## Terminology

`Versionstamp`: a 12 byte, unique, monotonically (but not sequentially)
increasing value for each committed transaction. The first 8 bytes are the
committed version of the database. The next 2 bytes are monotonic in the
serialization order for transactions. The final 2 bytes are user-defined and can
be used to create multiple versionstamps in a single transaction.

`Incarnation`: a monotonically increasing Integer value specified for each
CouchDB database. The `Incarnation` starts at zero (i.e. `\x14` in the tuple
layer encoding) when a database is created and is incremented by one whenever a
database is relocated to a different FoundationDB cluster. Thus the majority of
the time an Incarnation fits into a single byte, or two bytes if the database
has been moved around a small number of times.

`Sequence`: the combinination of the current `Incarnation` for the database and
the `Versionstamp` of the transaction. Sequences are monotonically increasing
even when a database is relocated across FoundationDB clusters.

`style=all_docs`: An optional query parameter to the `_changes` feed which
requests that all leaf revision ids are included in the response. The replicator
(one of the most frequent consumers of `_changes`) supplies this parameter.

---

# Detailed Description

The `_changes` feed provides a list of the documents in a given database, in the
order in which they were most recently updated. Each document shows up exactly
once in a normal response to the `_changes` feed.

In CouchDB 2.x and 3.x the database sequence is a composition of sequence
numbers from individual database shards. In the API this sequence is encoded as
a long Base64 string. The response to the `_changes` feed is not totally
ordered; the only guarantee is that a client can resume the feed from a given
sequence and be guaranteed not to miss any updates.

Future releases of CouchDB based on FoundationDB will be able to offer stronger
guarantees. The `Sequence` defined in the Terminology section above is totally
ordered across the entire cluster, and repeated calls to `_changes` on a
quiescent database will retrieve the same results in the same order. The
`Sequence` will still be encoded as a string, but as it's a more compact value
we propose to encode it in hexadecimal notation. These strings will sort
correctly, something that has not always been true in CouchDB 2.x.

## Data Model

Each database will contain a `changes` subspace with keys and values that take
the form

`("changes", Sequence) = (SeqFormat, DocID, RevPosition, RevHash, BranchCount,
NotDeleted)`

where the individual elements are defined as follows:

- `SeqFormat`: enum for the value encoding, to enable schema evolution
- `DocID`: the document ID
- `RevPosition`: positive integer encoded using standard tuple layer encoding
  (signed, variable-length, order-preserving)
- `RevHash`: 16 bytes uniquely identifying the winning revision of this document
- `Sequence`: the sequence of the last transaction that modified the document
  (NB: not necessarily the transaction that produced the `RevPosition-RevHash`
  edit).
- `BranchCount`: the number of edit branches associated with this document
- `NotDeleted`: `\x26` if the leaf of the edit branch is deleted, `\x27`
  otherwise (following tuple encoding for booleans)

A typical response to `_changes` includes all of this information in each row
except the internal `SeqFormat` and the `BranchCount`. The latter is used as an
optimization for the `style=all_docs` request; if this parameter is specified
and the `BranchCount` is 1 we can avoid making an extra request to the
"revisions" space to discover that there are no other revisions to include.

## Index Maintenance

As discussed in [RFC 001](001-fdb-revision-metadata-model.md), an update attempt
always retrieves the metadata KV for the current winning branch from the
"revisions" subspace. This metadata entry includes the sequence of the last edit
to the document, which serves as the key into the index in our "changes"
subspace. The writer will use that information to clear the existing KV from the
`_changes` subspace as part of the transaction.

The writer also knows in all cases what the `RevPosition`, `RevHash`,
`BranchCount`, and `NotDeleted` will be following the edit, and can use the
`set_versionstamped_key` API to write a new KV with the correct new sequence of
the transaction into the "changes" subspace.

In short, the operations in this subspace are
- doc insert: 0 read, 0 clear, 1 insert
- doc update: 0 read, 1 clear, 1 insert

## Handling of Unknown Commit Results

When using versionstamped keys as proposed in this RFC one needs to pay
particular care to the degraded mode when FoundationDB responds to a transaction
commit with `commit_unknown_result`. Versionstamped keys are not idempotent, and
so a naïve retry approach could result in duplicate entries in the "changes"
subspace. The index maintenance in this subspace is "blind" (i.e. no reads in
this subspace are performed), so the risk for duplicate entries is indeed a
valid concern.

We can guard against creating duplicates in the "changes" subspace by having the
transaction that updates that subspace also insert a KV into a dedicated
"transaction ID" subspace specifically corresponding to this document update. If
the CouchDB layer receives a `commit_unknown_result` it can simply check for the
presence of the transaction ID in FoundationDB to determine whether the previous
transaction succeeded or failed. If the transaction ID is not present, CouchDB
can safely retry with the same transaction ID. After a successful transaction
commit, the CouchDB layer can delete the transaction ID KV asynchronously. For
example, each process could dump the transaction ID of a successful commit into
a local ets table (shared by all databases), and a process could scan that table
once every few seconds and clear the associated entries from FDB in a single
transaction.

## Access Patterns

Let's consider first the simple case where an entire response to `_changes` fits
within a single FoundationDB transaction (specifically the 5 second limit). In
this case a normal request to `_changes` can be satisfied with a single range
read from the "changes" subspace. A `style=all_docs` request will need to check
the `BranchCount` for each row; if it's larger than 1, the client will need to
do a followup range request against the "revisions" subspace to retrieve the
additional revision identifiers to include in the response. A request with
`include_docs=true` will need to make a separate range request to the doc
storage subspace to retrieve the body of each winning document revision.

If a normal response to `_changes` cannot be delivered in a single transaction
the CouchDB layer should execute multiple transactions in series and stitch the
responses together as needed. Note that this opens up a subtle behavior change
from classic CouchDB, where a single database snapshot could be held open
~indefinitely for each shard, providing a complete snapshot of the database as
it existed at the *beginning* of the response. While future enhancements in
FoundationDB may allow us to recover that behavior, in the current version we
may end up with duplicate entries for individual documents that are updated
during the course of streaming the `_changes` response. The end result will be
that each document in the database shows up at least once, and if you take the
last entry for each document that you observe in the feed, you'll have the state
of the database as it existed at the *end* of the response.

Finally, when a user requests `_changes` with `feed=continuous` there is no
expectation of exactly-once semantics, and in fact this is implemented using
multiple database snapshots for each shard today. The extra bit of work with
this response type is to efficiently discover when a new read of the "changes"
subspace for a given database is required in FoundationDB. A few different
options have been discussed on the mailing list:

1. Writers publish `db_updated` events to `couch_event`, listeners use
   distributed Erlang to subscribe to all nodes, similar to the classic
   approach.
1. Poll the `_changes` subspace, scale by nominating a specific process per node
   to do the polling.
1. Same as above but using a watch on DB metadata that changes with every update
   instead of polling.

This RFC proposes to pursue the second approach. It preserves the goal of a
stateless CouchDB layer with no coordination between instances, and has a
well-known scalability and performance profile.

# Advantages

This design eliminates "rewinds" of the `_changes` feed due to cluster
membership changes, and enhances database sequences to enable relocation of
logical CouchDB databases across FoundationDB clusters without rewinds as well.

We anticipate improved throughput due to the more compact encoding of database
sequences.

The new sequence format always sorts correctly, which simplifies the job of
consumers tracking the sequence from which they should resume in parallel
processing environments.

# Disadvantages

It will not be possible to retrieve a complete point-in-time snapshot of a large
database in which each document appears exactly once. This may change with a
future enhancement to the storage engine underpinning FoundationDB.

# Key Changes

Nothing additional to report here.

## Applications and Modules affected

TBD depending on exact code layout going forward, but this functionality cuts
across several core modules of CouchDB.

## HTTP API additions

None.

## HTTP API deprecations

None.

# Security Considerations

None have been identified.

# References

[Original mailing list
discussion](https://lists.apache.org/thread.html/29d69efc47cb6328977fc1c66efecaa50c5d93a2f17aa7a3392211af@%3Cdev.couchdb.apache.org%3E)

[Detailed thread on isolation semantics for long
responses](https://lists.apache.org/thread.html/a4429197919e66ef0193d128872e17b3b62c1f197918df185136b35d@%3Cuser.couchdb.apache.org%3E)

# Acknowledgements

Thanks to @iilyak, @rnewson, @mikerhodes, @garrensmith and @alexmiller-apple for
comments on the mailing list discussions, and to @wohali for working through the
implications of the isolation changes on IRC.
