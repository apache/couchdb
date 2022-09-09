# Mango RFC

---

name: Formal RFC
about: Submit a formal Request For Comments for consideration by the team.
title: ‘Mango JSON indexes in FoundationDB’
labels: rfc, discussion
assignees: ‘’

---

[note]: # " ^^ Provide a general summary of the RFC in the title above. ^^ "

# Introduction

This document describes the data model, querying and indexing management for Mango JSON indexes with FoundationDB.

## Abstract

This document details the data model for storing Mango indexes. Indexes will be updated in the transaction that a document is written to FoundationDB. When an index is created on an existing database, a background task will build the index up to the Sequence that the index was created at.

## Requirements Language

[note]: # " Do not alter the section below. Follow its instructions. "

The keywords “MUST”, “MUST NOT”, “REQUIRED”, “SHALL”, “SHALL NOT”,
“SHOULD”, “SHOULD NOT”, “RECOMMENDED”, “MAY”, and “OPTIONAL” in this
document are to be interpreted as described in
[RFC 2119](https://www.rfc-editor.org/rfc/rfc2119.txt).

## Terminology

`Sequence`: a 13-byte value formed by combining the current `Incarnation` of the database and the `Versionstamp` of the transaction. Sequences are monotonically increasing even when a database is relocated across FoundationDB clusters. See (RFC002)[LINK TBD] for a full explanation.

---

# Detailed Description

Mango is a declarative JSON querying syntax that allows a user to retrieve documents based on a selector. Indexes can be defined to improve query performance. In CouchDB Mango is a query layer built on top of Map/Reduce indexes. Each Mango query follows a two-step process, first a subset of the selector is converted into a map query to be used with a predefined index or falling back to `_all_docs` if no indexes are available. Each document retrieved from the index is then matched against the full query selector.

With CouchDB on FoundationDB, all new created Mango indexes have the `interactive: true` option set. Thereby Mango indexes will be indexed in the same transaction that a document is add/updated to the database.

## Data Model

### Index Definitions

A Mango index is defined as:

```json
{
  "name": "view-name",
  "index": {
    "fields": ["fieldA", "fieldB"]
  },
  "partial_filter_selector": {}
}
```

The above index definition would be converted into a map index that looks like this:

```json
{
  "_id": "_design/ddoc",
  "language": "query",
  "views": {
    "view-name": {
      "map": {
        "fields": [{ "fieldA": "asc" }, { "fieldB": "asc" }],
        "selector": {}
      }
    }
  },
  "options": [{ "autoupdate": false }, { "interactive": true }]
}
```

- `{"autoupdate": false}` means that the index will not be auto updated in the background
- `{"interactive": true}` configures the index to be updated in the document update transaction

### Index Definition

Mango indexes are a layer on top of map indexes. So the index definition is the same as the map index definition.

### Index Limits

This design has certain defined limits for it to work correctly:

- The index definition (`name`, `fields` and `partial_filter_selector`) cannot exceed 64 KB FDB value limit
- The sorted keys for an index cannot exceed the 8 KB key limit
- To be able to update the index in the transaction that a document is updated in, there will have to be a limit on the number of Mango indexes for a database so that the transaction stays within the 10MB transaction limit. This limit is still TBD based on testing.

## Index building and management

When an index is created on an existing database, the index will be updated in a background job up to the versionstamp that the index was added to the database at. The process for building a new index would be:

1. Save index to the database, along with a creation versionstamp and set the index status to `building` so that is it not used to service any queries until it is updated. Add a job to `couch_jobs` to build the index.
2. Any write requests (document updates) after the saved index definition will update the index in the document update. Index writers can assume that previous versions of the document have already been indexed.
3. `couch_jobs` will start reading sections of the changes feed and building the index, this background process will keep processing the changes read until it reaches the creation versionstamp. Once it reaches that point, the index is up to date and `build_status` will be marked as `active` and the index can be used to service queries.
4. There is some subtle behavior around step 3 that is worth mentioning. The background process will have the 5-second transaction limit, so it will process smaller parts of the changes feed. Which means that it won’t have one consistent view of the changes feed throughout the index building process. This will lead to a conflict situation when the background process transaction is adding a document to the index while at the same time a write request has a transaction that is updating the same document. There are two possible outcomes to this, if the background process wins, the write request will get a conflict. At that point the write request will try to process the document again, read the old values for that document, remove them from the index and add the new values to the index. If the write request wins, and the background process gets a conflict, then the background process can try again, the document would have been removed from its old position in the changes feed and moved to the later position, so the background process won’t see the document and will then move on to the next one.

## Advantages

- Indexes are kept up to date when documents are changed, meaning you can read your own writes
- Makes Mango indexes first-class citizens and opens up the opportunity to create more Mango specific functionality

## Disadvantages

- FoundationDB currently does not allow CouchDB to do the document selector matching at the shard level. However, there is a discussion for this [Feature Request: Predicate pushdown](https://forums.foundationdb.org/t/feature-request-predicate-pushdown/954)

## Key Changes

- Mango indexes will be stored separately to Map/Reduce indexes.
- Mango Indexes will be updated when a document is updated
- A background process will build a new Mango index on an existing database
- There are specific index limits mentioned in the Index Limits section.

Index limitations aside, this design preserves all of the existing API options
for working with CouchDB documents.

## Applications and Modules affected

The `mango` application will be modified to work with FoundationDB

## HTTP API additions

When querying any of the `_index` endpoints an extra field, `build_status`, will be added to the index definition.
The `build_status` will either be `building` or `active`.

## HTTP API deprecations

None,

# Security Considerations

None have been identified.

# References

[Original mailing list discussion](https://lists.apache.org/thread.html/b614d41b72d98c7418aa42e5aa8e3b56f9cf1061761f912cf67b738a@%3Cdev.couchdb.apache.org%3E)

# Acknowledgements

thanks to following in participating in the design discussion

- @kocolosk
- @willholley
- @janl
- @alexmiller-apple
