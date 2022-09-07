---
name: Formal RFC
about: Submit a formal Request For Comments for consideration by the team.
title: 'JSON document storage in FoundationDB'
labels: rfc, discussion
assignees: ''

---

[NOTE]: # ( ^^ Provide a general summary of the RFC in the title above. ^^ )

# Introduction

This document describes a data model for storing JSON documents as key-value
pairs in FoundationDB. It includes a discussion of storing multiple versions of
the document, each identified by unique revision identifiers, and discusses some
of the operations needed to query and modify these documents.

## Abstract

The data model maps each "leaf" JSON value (number, string, true, false, and
null) to a single KV in FoundationDB. Nested relationships are modeled using a
tuple structure in the keys. Different versions of a document are stored
completely independently from one another. Values are encoded using
FoundationDB's tuple encoding.

The use of a single KV pair for each leaf value implies a new 100KB limit on
those values stored in CouchDB documents. An alternative design could split
these large (string) values across multiple KV pairs.

Extremely deeply-nested data structures and the use of long names in the nesting
objects could cause a path to a leaf value to exceed FoundationDB's 10KB limit
on key sizes. String interning could reduce the likelihood of this occurring but
not eliminate it entirely. Interning could also provide some significant space
savings in the current FoundationDB storage engine, although the introduction of
key prefix elision in the Redwood engine should also help on that front.

FoundationDB imposes a hard 10MB limit on transactions. In order to reserve
space for additional metadata, user-defined indexes, and generally drive users
towards best practices in data modeling this RFC proposes a **1MB (1,000,000
byte)** limit on document sizes going forward.

## Requirements Language

[NOTE]: # ( Do not alter the section below. Follow its instructions. )

The key words "MUST", "MUST NOT", "REQUIRED", "SHALL", "SHALL NOT",
"SHOULD", "SHOULD NOT", "RECOMMENDED",  "MAY", and "OPTIONAL" in this
document are to be interpreted as described in
[RFC 2119](https://www.rfc-editor.org/rfc/rfc2119.txt).

## Terminology

---

# Detailed Description

## Value Encoding

The `true` (`\x27`), `false` (`\x26`) and `null` (`\x00`) values each have a
single-byte encoding in FoundationDB's tuple layer. Integers are represented
with arbitrary precision (technically, up to 255 bytes can be used).
Floating-point numbers use an IEEE binary representation up to double precision.
More details on these specific byte codes are available in the [FoundationDB
documentation](https://github.com/apple/foundationdb/blob/6.0.18/design/tuple.md).

Unicode strings must be encoded into UTF-8. They are prefixed with a `\x02`
bytecode and are null-terminated. Any nulls within the string must be replaced
by `\x00\xff`. Raw byte strings have their own `\x01` prefix and must follow the
same rules regarding null bytes in the string. Both are limited to 100KB.

An object is decomposed into multiple key-value pairs, where each key is a tuple
identifying the path to a final leaf value. For example, the object

```
{
    "foo": {
        "bar": {
            "baz": 123
        }
    }
}
```

would be represented by a key-value pair of

```
pack({"foo", "bar", "baz"}) = pack({123})
```

Clients SHOULD NOT submit objects containing duplicate keys, as CouchDB will
only preserve  the last occurrence of the key and will silently drop the other
occurrences. Similarly, clients MUST NOT rely on the ordering of keys within an
Object as this ordering will generally not be preserved by the database.

An array of N elements is represented by N distinct key-value pairs, where the
last element of the tuple key is an integer representing the zero-indexed
position of the value within the array. As an example:

```
{
    "states": ["MA", "OH", "TX", "NM", "PA"]
}
```

becomes

```
pack({"states", 0}) = pack({"MA"})
pack({"states", 1}) = pack({"OH"})
pack({"states", 2}) = pack({"TX"})
pack({"states", 3}) = pack({"NM"})
pack({"states", 4}) = pack({"PA"})
```

More details on the encodings in the FoundationDB Tuple Layer can be found in
the [design
documentation](https://github.com/apple/foundationdb/blob/6.0.18/design/tuple.md).

## Document Subspace and Versioning

Document bodies will be stored in their own portion of the keyspace with a fixed
single-byte prefix identifying the "subspace". Each revision of a document will
be stored separately without term sharing, and the document ID and revision ID
are baked into the key. The structure looks like this

```
{DbName, ?DOCUMENTS, DocID, NotDeleted, RevPos, RevHash} = RevisionMetadata
{DbName, ?DOCUMENTS, DocID, NotDeleted, RevPos, RevHash, "foo"} = (value for doc.foo)
et cetera
```

where `RevisionMetadata` includes at the minimum an enum to enable schema
evolution for subsequent changes to the document encoding structure, and
`NotDeleted` is `true` if this revision is a typical `deleted=false` revision,
and `false` if the revision is storing user-supplied data associated with the
tombstone. Regular document deletions without any data in the tombstone do not
show up in the `?DOCUMENTS` subspace at all. This key structure ensures that in
the case of multiple edit branches the "winning" revision's data will sort last
in the key space.

## CRUD Operations

FoundationDB transactions have a hard limit of 10 MB each. Our document
operations will need to modify some metadata alongside the user data, and we'd
also like to reserve space for updating indexes as part of the same transaction.
This document proposes to limit the maximum document size to **1 MB (1,000,000
bytes)** going forward (excluding attachments).

A document insert does not need to clear any data in the `?DOCUMENTS` subspace,
and simply inserts the new document content. The transaction will issue a read
against the `?REVISIONS` subspace to ensure that no `NotDeleted` revision
already exists.

A document update targeting a parent revision will clear the entire range of
keys associated with the parent revision in the `?DOCUMENTS` space as part of
its transaction. Again, the read in the `?REVISIONS` space ensures that this
transaction can only succeed if the parent revision is actually a leaf revision.

Document deletions are a special class of update that typically do not insert
any keys into the `?DOCUMENTS` subspace. However, if a user includes extra
fields in the deletion they will show up in this subspace.

Document reads where we already know the specific revision of interest can be
done efficiently using a single `get_range_startswith` operation. In the more
common case where we do not know the revision identifier, there are two basic
options:

1. We can retrieve the winning revision ID from the `?REVISIONS` subspace, then
   execute a `get_range_startswith` operation as above.
1. We can start streaming the entire key range from the `?DOCUMENTS` space
   prefixed by `DocID` in reverse, and break if we reach another revision of the
   document ID besides the winning one.

Document reads specifying `conflicts`, `deleted_conflicts`, `meta`, or
`revs_info` will need to retrieve the revision metadata from the `?REVISIONS`
subspace alongside the document body regardless of which option we pursue above.

If a reader is implementing Option 2 and does not find any keys associated with
the supplied `DocID` in the `?DOCUMENTS` space, it will need to do a followup
read on the `?REVISIONS` space in order to determine whether the appropriate
response is `{"not_found": "missing"}` or `{"not_found": "deleted"}`.

# Advantages and Disadvantages

A leading alternative to this design in the mailing list discussion was to
simply store each JSON document as a single key-value pair. Documents exceeding
the 100KB value threshold would be chunked up into contiguous key-value pairs.
The advantages of this "exploded" approach are

- it lends itself nicely to sub-document operations, e.g. apache/couchdb#1559
- it optimizes the creation of Mango indexes on existing databases since we only
  need to retrieve the value(s) we want to index
- it optimizes Mango queries that use field selectors

The disadvantages of this approach are that it uses a larger number of key-value
pairs and has a higher overall storage overhead from the repeated common key
prefixes. The new FoundationDB storage engine should eliminate some of the
storage overhead.
As per the FoundationDB discussion about being able to co-locate compute operations with data storage servers/nodes](https://forums.foundationdb.org/t/feature-request-predicate-pushdown/954/6), if we were to make use of this hypothetical feature, we’d not get a guarantee of entire documents being co-located on one storage node, requiring us to do extra work should we want to, say, assemble a full `doc` to send to a map function. JS views would have a harder time, while Mango indexes with their explicit field declarations might get around this particular complexity more easily. For now, this is recorded here so we don’t forget track of this later.


# Key Changes

- Individual strings within documents are limited to 100 KB each.
- The "path" to a leaf value within a document can be no longer than 10 KB.
- The entire JSON document is limited to 1 MiB.

Size limitations aside, this design preserves all of the existing API options
for working with CouchDB documents.

## Applications and Modules affected

TBD depending on exact code layout going forward.

## HTTP API additions

None.

## HTTP API deprecations

None, aside from the more restrictive size limitations discussed in the Key
Changes section above.

# Security Considerations

None have been identified.

# References

[Original mailing list discussion](https://lists.apache.org/thread.html/fb8bdd386b83d60dc50411c51c5dddff7503ece32d35f88612d228cc@%3Cdev.couchdb.apache.org%3E)

[Draft RFC for revision metadata](https://github.com/apache/couchdb-documentation/blob/rfc/001-fdb-revision-model/rfcs/001-fdb-revision-metadata-model.md)

[Current version of Tuple Layer documentation](https://github.com/apple/foundationdb/blob/6.0.18/design/tuple.md)

# Acknowledgements

We had lots of input on the mailing list in this discussion, thanks to

- @banjiewen
- @davisp
- @ermouth
- @iilyak
- @janl
- @mikerhodes
- @rnewson
- @vatamane
- @wohali
- Michael Fair.
- Reddy B.
