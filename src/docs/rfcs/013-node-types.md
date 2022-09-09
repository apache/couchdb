---
name: Node Types
about: Introduce heterogeneous node types to CouchDB 4
title: 'Node Types'
labels: rfc, discussion
assignees: ''

---

# Introduction

This RFC proposes the ability to have different node types in CouchDB 4+. This
would improve performance and allow for a more efficient use of resources.

## Abstract

Previously, in CouchDB 2 and 3, cluster functionality was uniformly distributed
amongst the nodes. Any node could accept HTTP requests, run replication jobs
and build secondary indices. With the FDB-based topology, CRUD operations have
lower resource needs and so it could be useful to have a heterogeneous
topology, where for example, CRUD operations run on lower capacity nodes, and a
few higher capacity nodes handle replication or indexing jobs.


## Requirements Language

The key words "MUST", "MUST NOT", "REQUIRED", "SHALL", "SHALL NOT",
"SHOULD", "SHOULD NOT", "RECOMMENDED",  "MAY", and "OPTIONAL" in this
document are to be interpreted as described in
[RFC 2119](https://www.rfc-editor.org/rfc/rfc2119.txt).

## Terminology

*node type* : A label used to designate a subset of CouchDB functionality.

---

# Detailed Description

## Node Types

A node type is a description of a some internal CouchDB functionality. These
are the initially defined node types:

 * `api_frontend` : Indicates this node can accept HTTP API requests.
 * `view_indexing` : Indicates this node can build map/reduce view indices.
 * `search_indexing` : Indicates this node can build search indices.
 * `replication` : Indicates this node can run replication jobs.

Users can configure CouchDB nodes with any combination of those types.

## Configuration

Configuration MAY be specified in the Erlang application or OS environment
variables. OS environment variables have a higher precedence. By default, if
the type is not configured in either one of those places, it defaults to
`true`.

### Erlang Application Environment Configuration

Configuration MUST be specified for the `fabric` application, under the
`node_types` key. The value MUST be proplist which looks like `[{$type, true |
false}, ...]`. For example, the `va.args` file MAY be used like such:

```
-fabric node_types '[{api_frontend, false}, {replication, true}]'

```

### OS Environment Configuration

Node types MAY be set via environment variables using the `COUCHDB_NODE_TYPE_`
prefix. The prefix SHOULD be followed by the type label. If the value of the
variable is `false` the functionality indicated will be disabled on that
node. Any other value, indicates `true`.

Example:

`COUCHDB_NODE_TYPE_API_FRONTEND=false COUCHDB_NODE_TYPE_VIEW_INDEXING=true ...`

## Implementation

Implementation should be minimally invasive, at least for the node types listed
above.

 * `api_frontend` would enable the `chttpd` application, or its top level
   supervisor.

 * All background tasks in FDB are executed via the `couch_jobs` framework. The
top level application supervisors typically have a separate `gen_server` in
charge of accepting jobs and executing them. The implementation then would be
as simple as having a `case` statement around the worker's `start_link()`
function.

# Advantages and Disadvantages

## Disadvantages

 - Increased configuration-state complexity

## Advantages

 - Ability to utilize hardware resources better
 - Possibly better security by running indexing and replication jobs in an
   isolated environment inaccessible from the outside

# Key Changes

 - Heterogeneous node types
 - New configuration section
 - New configuration environment variables

## Applications and Modules Affected

 - chttpd
 - fabric
 - couch_views
 - couch_jobs
 - couch_replicator
 - mango

## HTTP API additions

N/A

## HTTP API deprecations

N/A

# Security Considerations

N/A

# References

[1] https://github.com/apache/couchdb/issues/1338

[2] https://github.com/apache/couchdb-documentation/blob/main/rfcs/007-background-jobs.md

# Acknowledgments

@kocolosk
@mikerhodes
