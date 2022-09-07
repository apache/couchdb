# Map indexes RFC

---

name: Formal RFC
about: Submit a formal Request For Comments for consideration by the team.
title: ‘Map indexes on FoundationDB’
labels: rfc, discussion
assignees: ''

---

## Introduction

This document describes the data model and index management for building and querying map indexes.

## Abstract

Map indexes will have their data model stored in FoundationDB. Each index is grouped via its design doc's view signature. An index will store the index's key/values, size of the index and the last sequence number from the changes feed used to update the index.

Indexes will be built using the background jobs api, `couch_jobs`, and will use the changes feed. There will be new size limitations on keys (10KB) and values (100KB) that are emitted from a map function.

## Requirements Language

[note]: # " Do not alter the section below. Follow its instructions. "

The key words "MUST", "MUST NOT", "REQUIRED", "SHALL", "SHALL NOT",
"SHOULD", "SHOULD NOT", "RECOMMENDED", "MAY", and "OPTIONAL" in this
document are to be interpreted as described in
[RFC 2119](https://www.rfc-editor.org/rfc/rfc2119.txt).

## Terminology

`Sequence`: a 13-byte value formed by combining the current `Incarnation` of the database and the `Versionstamp` of the transaction. Sequences are monotonically increasing even when a database is relocated across FoundationDB clusters. See (RFC002)[LINK TBD] for a full explanation.

`View Signature`: A md5 hash of the views, options, view language defined in a design document.
`Interactive view`: A view updated in the same transaction that the document is added/updated to the database.

---

## Detailed Description

CouchDB views are used to create secondary indexes in a database. An index is defined by creating map/reduce functions in a design document. This document describes building the map indexes on top of FoundationDB (FDB).
There are two ways to build a secondary index: via a background job or via in the same transaction that the document is added to the database. Building the index via the background job is the default way that a map index will be build. An example map function to do this is shown below:

```json
{
  "_id": "_design/design-doc-id",
  "_rev": "1-8d361a23b4cb8e213f0868ea3d2742c2",
  "views": {
    "map-view": {
      "map": "function (doc) {\n  emit(doc._id, 1);\n}"
    }
  },
  "language": "javascript"
}
```

Adding `interactive: true` to the option field of an index will configure the index to be updated in the same transaction that the document is added to the database. This functionality has primarily been added to support Mango indexes but can work with map indexes. An example of a map index configured is shown below:

```json
{
  "_id": "_design/design-doc-id",
  "_rev": "1-8d361a23b4cb8e213f0868ea3d2742c2",
  "views": {
    "map-view": {
      "map": "function (doc) {\n  emit(doc._id, 1);\n}"
    }
  },
  "language": "javascript",
  "options": [{ "interactive": true }]
}
```

Interactive views have two step process to being built. When an index is added to the database, a background job is created for the index to be built up to the change sequence, creation versionstamp, that the index was added at. Any new documents added after the index was added will be indexed in the transaction that the document is added to the database. If a query for an interactive view is received before the background job is complete, CouchDB will wait until the background job is complete before serving the request.

### Data model

The data model for a map indexed is:

```
% View build sequence - The change sequence that the index has been updated to.
(<database>, ?DB_VIEWS, ?VIEW_INFO, ?VIEW_UPDATE_SEQ, <view_signature>) = Sequence

% Interactive View Creation Versionstamp
(<database>, ?DB_VIEWS, ?VIEW_INFO, ?VIEW_CREATION_VS, <signature>) = Versionstamp
% Interactive View Build Status
(<database>, ?DB_VIEWS, ?VIEW_INFO, ?VIEW_BUILD_STATUS, <signature>) = INDEX_BUILDING | INDEX_READY

% Number of rows in the index
{<database>, ?DB_VIEWS, ?VIEW_INFO, ?VIEW_ROW_COUNT, ?VIEW_ID_INFO, <view_id>, <view_signature> } = <row_count>
% Key/Value size of index
{<database>, ?DB_VIEWS, ?VIEW_INFO, ?VIEW_KV_SIZE, <view_signature>, <view_id>} = <kv_size>

% Id index, used to track record what keys are in the index for each document
(<database>, ?DB_VIEWS, ?VIEW_DATA, <view_signature>, ?VIEW_ID_RANGE, <_id>, <view_id>) = [total_keys, total_size, unique_keys]
% The key/values for the index
(<database>, ?DB_VIEWS, ?VIEW_DATA, <view_signature>, ?VIEW_MAP_RANGE, <view_id>, {<key>, <_id>}, <dupe_id>) = {<emitted_key>, <emitted_value>}
```

Each field is defined as:

- `database` is the specific database namespace
- `?DB_VIEWS` is the views namespace.
- `<view_signature>` is the design documents `View Signature`
- `?VIEW_INFO` is the view information namespace
- `?VIEW_UPDATE_SEQ` is the change sequence namespace
- `?VIEW_ID_RANGE` is the map id index namespace
- `?VIEW_MAP_RANGE` is the map namespace
- `_id` is the document id
- `view_id` id of a view defined in the design document
- `key` is the encoded emitted row key from a map function
- `count` is a value that is incremented to allow duplicate keys to be emitted for a document
- `emitted_key` is the emitted key from the map function
- `emitted_value` is the emitted value from the map function
- `row_count` number of rows in the index
- `kv_size` size of the index
- `total_keys` is the number of keys emitted by a document
- `total_size` is the size of the key/values emitted by the document
- `unique_keys` is the unique keys emitted by the document
- `dupe_id` the duplication id to allow multiple documents to emit a key/value

The process flow for a document to be indexed in the background is as follows:

1. FDB Transaction is started
1. Read the document from the changes read (The number of documents to read at one type is configurable, the default is 100)
1. The document is passed to the javascript query server and run through all the map functions defined in the design document
1. The view's sequence number is updated to the sequence the document is in the changes feed.
1. If the document was deleted and was previously in the view, the previous keys for the document are read from `?VIEW_ID_RANGE` and then cleared from the `?VIEW_MAP_RANGE`. The Row count and size count are also decreased.
1. If the document is being updated and was previously added to the index, then he previous keys for the document are read from `?VIEW_ID_RANGE` and then cleared from the `?VIEW_MAP_RANGE` and then the index is updated with the latest emitted keys and value.
1. The emitted keys are stored in the `?VIEW_ID_RANGE`
1. The emitted keys are encoded then added to the `?VIEW_MAP_RANGE` with the emitted keys and value stored
1. The `?VIEW_ROW_COUNT` is incremented
1. The `?VIEW_KV_SIZE` is increased

### Emitted Keys and Values limites

If we have a design document like the following:

```js
{
  "_id": "_design/design-doc-id",
  "_rev": "1-8d361a23b4cb8e213f0868ea3d2742c2",
  "views": {
    "map-view": {
      "map": "function (doc) {
          emit(doc._id, doc.location);
          emit([doc._id, doc.value], doc.name);
        }",
    }
  },
  "language": "javascript",
  "options": [{"interactive":  true}]
}
```

Each emit would be a new key/value row in the map index. Each key row cannot exceed 8KB and and each value row cannot exceed 64KB.
If a document is emitted as a value, that document is not allowed to exceeed 64KB.

### Key ordering

FoundationDB orders key by byte value which is not how CouchDB orders keys. To maintain CouchDB's view collation, a type value will need to be prepended to each key so that the correct sort order of null < boolean < numbers < strings < arrays < objects is maintained.

In CouchDB 2.x, strings are compared via ICU. The way to do this with FoundationDB is that for every string an ICU sort string will be generated upfront and used for index ordering instead of the original string.

### Index building

An index will be built and updated via a [background job worker](https://github.com/apache/couchdb-documentation/blob/main/rfcs/007-background-jobs.md). When a request for a view is received, the request process will add a job item onto the background queue for the index to be updated. A worker will take the item off the queue and update the index. Once the index has been built, the background job server will notify the request that the index is up to date. The request process will then read from the index and return the results. This process can also be optimised in two ways. Firstly, using a new couch_events system to listen for document changes in a database and then adding indexing jobs to the queue to keep indexes warm. The second optimisation is if the index only requires a small update, rather update the index in the HTTP request process instead of doing the work via the background queue.

Initially, the building of an index will be a single worker running through the changes feed and creating the index. In the future, we plan to parallelise that work so that multiple workers could build the index at the same time. This will reduce build times.

### View clean up

When a design document is changed, new indexes will be built and grouped under a new `View Signature`. The old map indexes will still be in FDB. To clean up will be supported via the existing [/db/\_view_cleanup](https://docs.couchdb.org/en/latest/api/database/compact.html#db-view-cleanup) endpoint.

A future optimisation would be to automate this and have CouchDB to monitor design doc changes and then look to clean up old view indexes via a background worker.

### Stale = “ok” and stable = true

With the consistency guarantee’s CouchDB will get from FDB, `stable = true` will no longer be an option that CouchDB would support and so the argument would be ignored. Similar `stale = “ok”` would now be translated to `update = false`.

### Size limits

- The sum of all keys emitted for a document cannot exceed 64 KB
- Emitted keys will not be able to exceed 8 KB
- Values cannot exceed 64 KB
- There could be rare cases where the number of key-value pairs emitted for a map function could lead to a transaction either exceeding 10 MB in size which isn’t allowed or exceeding 5 MB which impacts the performance of the cluster. In this situation, CouchDB will send an error.

These limits are the hard limits imposed by FoundationDB. We will have to set the user imposed limits to lower than that as we store more information than just the user keys and values.

## Advantages

- Map indexes will work on FoundationDB with the same behaviour as current CouchDB 1.x
- Options like stale = “ok” and ‘stable = true’ will no longer be needed

## Disadvantages

- Size limits on key and values

## Key Changes

- Indexes are stored in FoundationDB
- Indexes will be built via the background job queue
- ICU sort strings will be generated ahead of time for each key that is a string

## Applications and Modules affected

- couch_mrview will be removed and replaced with a new couch_views OTP application

## HTTP API additions

The API will remain the same.

## HTTP API deprecations

- `stable = true` is no longer supported
- `stale = "ok"` is now converted to `update = false`
- reduce functions are not supported in this RFC

## Security Considerations

None have been identified.

## Future improvements

Two future improvements we could look to do that builds upon this work:

- Better error handling for user functions. Currently, if a document fails when run through the map function, a user has to read the logs to discover that. We could look at adding an error-index and a new API endpoint.
- Parallel building of the index. In this RFC, the index is only built sequentially by one index worker. In the future, it would be nice to split that work up and parallelize the building of the index.

## References

- TBD link to background tasks RFC
- [Original mailing list discussion](https://lists.apache.org/thread.html/5cb6e1dbe9d179869576b6b2b67bca8d86b30583bced9924d0bbe122@%3Cdev.couchdb.apache.org%3E)

## Acknowledgements

Thanks to everyone that participated in the mailing list discussion

- @janl
- @kocolosk
- @willholley
- @mikerhodes
