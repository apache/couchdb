[NOTE]: # ( ^^ Provide a general summary of the RFC in the title above. ^^ )

# Introduction

This is a proposal for the storage of document revision history metadata as a
set of KVs in FoundationDB.

## Abstract

This design stores each edit branch as its own KV, and all of the edit branches
are stored separately from the actual document data. Document reads can avoid
retrieving this information, while writes can avoid retrieving the document
body.

## Requirements Language

[NOTE]: # ( Do not alter the section below. Follow its instructions. )

The key words "MUST", "MUST NOT", "REQUIRED", "SHALL", "SHALL NOT", "SHOULD",
"SHOULD NOT", "RECOMMENDED",  "MAY", and "OPTIONAL" in this document are to be
interpreted as described in [RFC
2119](https://www.rfc-editor.org/rfc/rfc2119.txt).

## Terminology

[TIP]:  # ( Provide a list of any unique terms or acronyms, and their
definitions here.)

`Versionstamp`: a 12 byte, unique, monotonically (but not sequentially)
increasing value for each committed transaction. The first 8 bytes are the
committed version of the database. The next 2 bytes are monotonic in the
serialization order for transactions. The final 2 bytes are user-defined and can
be used to create multiple versionstamps in a single transaction.

`Incarnation`: a single byte, monotonically increasing value specified for each
CouchDB database. The `Incarnation` starts at `\x00` when a database is created
and is incremented by one whenever a database is relocated to a different
FoundationDB cluster.

`Sequence`: a 13 byte value formed by combining the current `Incarnation` of
the database and the `Versionstamp` of the transaction. Sequences are
monotonically increasing even when a database is relocated across FoundationDB
clusters.

---

# Detailed Description

The size limits in FoundationDB preclude storing the entire revision tree as a
single value; in pathological situations the tree could exceed 100KB. Rather, we
propose to store each edit *branch* as a separate KV. We have two different
value formats, one that is used for the "winning" edit branch and one used for
any additional edit branches of the document. The winning edit branch includes
the following information:

`(“revisions”, DocID, NotDeleted, RevPosition, RevHash) = (RevFormat, Sequence,
BranchCount, [ParentRev, GrandparentRev, …])`

while the other edit branches omit the `Sequence` and `BranchCount`:

`(“revisions”, DocID, NotDeleted, RevPosition, RevHash) = (RevFormat,
[ParentRev, GrandparentRev, …])`

The individual elements of the key and value are defined as follows:
- `DocID`: the document ID
- `NotDeleted`: `\x26` if the leaf of the edit branch is deleted, `\x27`
  otherwise (following tuple encoding for booleans)
- `RevPosition`: positive integer encoded using standard tuple layer encoding
  (signed, variable-length, order-preserving)
- `RevHash`: 16 bytes uniquely identifying this revision
- `RevFormat`: enum for the revision encoding being used to enable schema
  evolution
- `Sequence`: the sequence of the last transaction that modified the document
  (NB: not necessarily the last edit to *this* branch).
- `BranchCount`: the number of edit branches associated with this document.
- `[ParentRev, GrandparentRev, ...]`: 16 byte identifiers of ancestors, up to
  1000 by default

## Limits

In order to stay compatible with FoundationDB size limits we need to prevent
administrators from increasing `_revs_limit` beyond what we can fit into a
single value. Suggest **4000** as a max.

## Update Path

Each edit on a document will read and modify the so-called "winning" edit
branch, a property that is essential for FoundationDB to correctly identify
concurrent modifications to a given document as conflicting. We enforce this
specifically by storing the `Sequence` only on the winning branch. Other
branches set this to null.

If a writer comes in and tries to extend a losing edit branch, it will find the
first element of the value to be null and will do an additional edit branch read
to retrieve the winning branch. It can then compare both branches to see which
one will be the winner following that edit, and can assign the extra metadata to
that branch accordingly.

A writer attempting to delete the winning branch (i.e., setting `NotDeleted` to
`\x26`) will need to read two contiguous KVs, the one for the winner and the one
right before it. If the branch before it will be the winner following the
deletion then we move the storage of the extra metadata to it accordingly. If
the tombstoned branch remains the winner for this document then we only update
that branch.

A writer extending the winning branch with an updated document (the common case)
will proceed reading just the one branch.

A writer attempting to insert a new document without any base revision will need
to execute a `get_range_startswith` operation with `limit=1` and `reverse=true`
on the key range prefixed by ("revisions", DocID). A null result from that range
read would be the signal to go ahead with the write. If another transaction
races our writer and inserts the document first FoundationDB will detect the
intersection between the write set of that transaction and the read range here
and correctly cause our writer to fail.

New edit branches can only be created on that first edit to a document or during
`new_edits=false`, so most interactive writers will just carry over the
`BranchCount` with each edit they make. A writer with `new_edits=false` will
retrieve the full range of KV pairs and set the `BranchCount` accordingly.
Tracking the `BranchCount` here enables us to push that information into the
`_changes` feed index, where it can be used to optimize the popular
`style=all_docs` queries in the common case of a single edit branch per
document.

Summarizing the performance profile:
- Extending a losing branch: 2 KVs, 2 roundtrips
- Deleting the winning branch: 2 KVs, 1 roundtrip
- Extending the winning branch: 1 KV, 1 roundtrip
- `new_edits=false` update: `<N>` KVs, 1 roundtrip

# Advantages

We can read a document revision without retrieving the revision tree, which in
the case of frequently-edited documents may be larger than the doc itself.

We ensure that an interactive document update against the winning branch only
needs to read the edit branch KV against which the update is being applied, and
it can read that branch immediately knowing only the content of the edit that is
being attempted (i.e., it does not need to read the current version of the
document itself). The less common scenario of updating a losing branch is only
slightly less efficient, requiring two roundtrips.

Interactively updating a document with a large number of edit branches is
therefore dramatically cheaper, as no more than two edit branches are read or
modified regardless of the number of branches that exist, and no tree merge
logic is required.

Including `NotDeleted` in the key ensures that we can efficiently accept the
case where we upload a new document with the same ID where all previous edit
branches have been deleted; i.e. we can construct a key selector which
automatically tells us there are no `deleted=false` edit branches.

The `RevFormat` enum gives us the ability to evolve revision history storage
over time, and to support alternative conflict resolution policies like Last
Writer Wins.

Access to the indexed `Sequence` ensures we can clear the old entry in the
`changes` subspace during an edit. The `set_versionstamped_value` API is used to
store this value automatically.

The key structure above naturally sorts so that the "winning" revision is the
last one in the list, which we leverage when deleting the winning edit branch
(and thus promoting the one next in line), and extending a conflict branch (to
coordinate the update to the `Sequence`) This is also a small optimization for
reads with `?revs=true` or `?revs_info=true`, where we want the details of the
winning edit branch but don't actually know the `RevPosition` and `RevHash` of
that branch.

# Disadvantages

Historical revision identifiers shared by multiple edit branches are duplicated.

# Key Changes

Administrators cannot set `_revs_limit` larger than 4,000 (previously
unlimited?). Default stays the same at 1,000.

The intention with this data model is that an interactive edit that supplies a
revision identifier of a deleted leaf will always fail with a conflict. This is
a subtle departure from CouchDB 2.3 behavior, where an attempt to extend a
deleted edit branch can succeed if some other `deleted=false` edit branch
exists. This is an undocumented and seemingly unintentional behavior. If we need
to match that behavior it will require reading 3 KVs in 2 roundtrips for *every*
edit that we reject with a conflict.

## Modules affected

TBD depending on exact code layout going forward, but the `couch_key_tree`
module contains the current revision tree implementation.

## HTTP API additions

None.

## HTTP API deprecations

None.

## Security Considerations

None have been identified.

# References

[Original mailing list
discussion](https://lists.apache.org/thread.html/853b86f3a83108745af510959bb381370a99988af4528617bdbe1be4@%3Cdev.couchdb.apache.org%3E)

[apache/couchdb#1957](https://github.com/apache/couchdb/issues/1957) (originally
submitted RFC as an issue in the main project repo instead of a PR here).

# Acknowledgements

Thanks to @iilyak, @davisp, @janl, @garrensmith and @rnewson for comments on the
mailing list discussion.
