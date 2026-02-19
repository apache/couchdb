---
name: Formal RFC
about: Submit a formal Request For Comments for consideration by the team.
title: 'Support covering indexes when using Mango JSON (view) indexes'
labels: rfc, discussion
assignees: ''

---

[NOTE]: # ( ^^ Provide a general summary of the RFC in the title above. ^^ )

# Introduction

## Abstract

[NOTE]: # ( Provide a 1-to-3 paragraph overview of the requested change. )
[NOTE]: # ( Describe what problem you are solving, and the general approach. )

Covering indexes are used to reduce the time the database takes to respond to
queries. An index "covers" a query when the query only requires fields that are
in the index (in this way, "covering" is a property of index and query
combined). When this is the case, the database doesn't need to consult primary
data and can return results for the query from only the index. In more familiar
CouchDB terminology, this is equivalent to querying a view with
`include_docs=false`.

When evaluating a query, Mango currently doesn't use the concept of covering
indexes; even if a query could be answered without reading each result's full
JSON document, Mango will still read it. This makes it impossible for Mango to
return data as quickly as the underlying view.

My benchmarking shows that Mango can answer at the same rate as the underlying
view index. It currently runs at the same pace as calling the view with
`include_docs=true`. Preliminary modifications to Mango showed that, with
covering index support and a query that can use it, Mango can stream results
as quickly as the underlying view. Adding covering indexes therefore increases
the production use-cases Mango can support substantially.

There are likely two phases to this:

- Enable covering indexing processing for current indexes (ie, over view keys).
- Allow Mango view indexes to include extra data from documents, storing it in
  the `value` of the view. Support use of this extra data within the covering
  indexes feature.

### Out of scope

This proposal only covers adding covering indexes to JSON indexes and not text
indexes. The aim is to reduce the need for CouchDB users to run separate
processes, such as Lucene, to get improved querying performance and capability.

We do not aim to replicate `reduce` functionality from views, only to bring
parity to non-reduced view execution speed (ie, when views are used to search
the document space) to Mango.

## Requirements Language

[NOTE]: # ( Do not alter the section below. Follow its instructions. )

The key words "MUST", "MUST NOT", "REQUIRED", "SHALL", "SHALL NOT",
"SHOULD", "SHOULD NOT", "RECOMMENDED",  "MAY", and "OPTIONAL" in this
document are to be interpreted as described in
[RFC 2119](https://www.rfc-editor.org/rfc/rfc2119.txt).

## Terminology

[TIP]:  # ( Provide a list of any unique terms or acronyms, and their definitions here.)

- Mango: CouchDB's Mongo-inspired querying system.
- View / JSON index: Mango index that uses the same index as Cloudant views.
- Coordinator: the Erlang process that handles doing a distributed query across
    a CouchDB cluster.

---

# Detailed Description

[NOTE]: # ( Describe the solution being proposed in greater detail. )
[NOTE]: # ( Assume your audience has knowledge of, but not necessarily familiarity )
[NOTE]: # ( with, the CouchDB internals. Provide enough context so that the reader )
[NOTE]: # ( can make an informed decision about the proposal. )

[TIP]:  # ( Artwork may be attached to the submission and linked as necessary. )
[TIP]:  # ( ASCII artwork can also be included in code blocks, if desired. )

This would take place within `mango_view_cursor.erl`. The key functions
involved are the shard-level `view_cb/2`, the streaming result handler at the
coordinator end (`handle_message/2`) and the `execute/3` function.

## Mango JSON index selection

A Mango JSON index is implemented as a view with a complex key. The first field
in the index is the first entry in the complex key, the second field is the
second key and so on. Even indexes with one field use a complex key with length
`1`.

When choosing a JSON index to use for a query, there are a couple of things that
are important to covering indexes.

Firstly, note there are certain predicate operators that can be used with an
index, currently: `$lt`, `$lte`, `$eq`, `$gte` and `$gt`. These can easily be
converted to key operations within a key ordered index. For an index to be
chosen for a query, the first key within the indexes complex key MUST be used
with a predicate operator that can be converted into an operation on the index.

Secondly, a quirk of Mango indexes is that for a document to be included in an
index it must contain all of the index's indexed fields. Documents without all
the fields will not be included. This means that when we are choosing an index
for a query, we must further choose an index where the predicates within the
`selector` imply `$exists=true` for all fields in the index's key. Without that,
we will have incomplete results.

Why is this? Let's look at an index with these fields:

```json
["age", "name"]
```

Now we index two documents. The first document is included in the index while the second is not (because it doesn't include `name`):


```json
{"_id": "foo", "age": 39, "name": "mike"}

{"_id": "bar", "age": 39, "pet": "cat"}
```

The `selector` `{"age": {"$gt": 30}}` should return both documents. However, if
we use the index above, we'd miss out `bar` because it's not in the index.
Therefore we can't use the index.

On the other hand, the `selector` `{"age": {"$gt": 30}, "name": {"$exists":
true}}` requires that the `name` field exist so the index can be used because
the query predicates can only match documents containing both `age` and `name`,
just like the index. In both cases, note the predicate `"age": {"$gt": 30}`
implies `"age": {"$exists": true}`.

## Phase 1: handle keys only covering indexes

Within `execute/3` we will need to decide whether the view should be requested
to include documents. If the index is covering, this will not be required and
so the `include_docs` argument to the view fabric call will be `false`. We'll
need to add a helper method to return whether the index is covering.

When selecting an index, we'll need to ensure that only fields in the `selector`
and not `fields` are used when choosing an index. This is because we need all
fields in the `selector` to be present per [Mango JSON index
selection](#mango-json-index-selection). This is because `fields` is only used
after we generate the result set, and none of the field names in `fields` need
to exist in result documents.

As an example, an index `["age", "name"]` would still require the `selector` to
imply `$exists=true` for both `age` and `name` even if the `fields` were just
`["age"]` in order that correct results be returned.

Of note, this means that if an index is unusable pre-covering-index support, it
will continue to be unusable after this implementation: whether an index covers
a query is only used to prefer one already usable index over another.

Within `view_cb/2`, we'll need to know whether an index is covering. Without
that, `view_cb/2` will interpret the lack of included documents as an indicator
that it should do nothing, while in fact we want it to fully process the result
as it does when `include_docs` is used -- apart from when the user passes `r>=2`
in the Mango query because then the coordinator reads and processes documents.
(Aside: it'd be good to remove this `r` option to simplify things).

In `handle_message/2` the main work is ensuring that we handle mixed cluster
version states -- ie, cluster state during upgrades.

## Phase 2: add support for included fields in indexes

I propose we add an `include` field into a Mango JSON index definition:

```json
{
    "index": {
        "fields": [ "age", "name" ],
        "include": [ "occupation", "manager_id" ]
    },
    "name": "foo-json-index",
    "type": "json"
}
```

Behaviour requirements:

- Unlike `fields`, the fields in `include` _do not have to exist_ in the source
    document in order that the document be included in the index. This is to
    allow the index to cover more queries.
- Including a deeply nested field would follow the same pattern as for other
    field references in Mango, `person.address.zip`.
- There is no notation to include the whole document, that is, no equivalent of
    `emit(doc.name, doc)`.
- `"include": []`,  `"include": null` and omitting the `include` field are all
    equivalent.
- Ordering of the fields in `include` is not important. They can be reordered
    before storing if needed (eg, sorted).
- It will be an error to include a field in both `fields` and `include`. This
    should be rejected by the `_index` call.
- The `include` field would be rejected for `text` type indexes.

Alternatives considered:

- Adding `include` outside `index`. This didn't seem right as the `index`
    object already includes `partial_filter_selector` and `include` seems a
    peer of this. ([docs](https://docs.couchdb.org/en/stable/api/database/find.html#db-index)).
- Alternative name `store`. We use this for Lucene indexes when dreyfus/clouseau
    is used. I elected to use a separate name to either `value` or `store` to
    avoid index-type specificity. I take the name from Postgres, which uses
    `INCLUDE` in its index definition to [support covering indexes][pgcover].

[pgcover]: https://www.postgresql.org/docs/current/indexes-index-only-scans.html

Adding this will require changes in `mango_idx_view` to store the definition and
in how we process documents during indexing, which looks to be in
`get_index_entries` in `mango_native_proc`.

We'll then need to update the Mango cursor methods mentioned above to take
account of the values within the covering index code.

One thing to be careful about is again index selection. We will still need all
index keys to be present in the `selector` as above so need differentiate
between the fields in index's keys and values when selecting an index to ensure
we retain the correct behaviour per [Mango JSON index
selection](#mango-json-index-selection).

### Limits on included fields

Adding "too much" data to indexes is likely to slow down index scans because
there will be more data to process. We would also like to avoid users creating
pathological cases just because they can. Therefore, limiting the data that can
be stored seems wise. Saying that, for those that are willing to profile their
workloads should have a get-out clause from limits.

As an example, [postgres](https://www.postgresql.org/docs/current/limits.html) limits indexes to 32 columns. Its max field size is 1GB; I think we'd like something a little smaller!

Therefore the feature will have the following limit enforcement settings:

- `mango_json_index_include_fields_max` is the limit on the length of the
    `include` list.
- `mango_json_index_include_depth_max` is a limit on the depth of fields we will
    pull out. Basically the maximum numbers of `.` in a path.
- If the total number of bytes for values exceeds
    `mango_json_index_include_size_bytes_max` then we will skip that document
    from the index.

I need to check whether these should be prefixed `mango_` given they would live
in a `mango` configuration section.

Defaults:

- `mango_json_index_include_fields_max=16`
- `mango_json_index_include_depth_max=8`
- `mango_json_index_include_size_bytes_max=32768` (32kb)

I have chosen power-of-two limits mostly because they feel like familiar
numbers. Not a great reason, so these may be refined during code writing if I
can work out suitable benchmarks.


## Mixed versions during cluster upgrades

The relevant scenarios here are an updated coordinator talking to outdated
shards, and the opposite of an outdated coordinator talking to upgraded shards.
A further wrinkle is that while a coordinator is either upgraded or not, the
shards that the coordinator speaks to can be a mixture of upgraded and outdated.

For the purposes of this discussion, we only need to worry about when a covering
index is in play during a query; the code path outside that use-case should not
change.

From what I can tell, we can avoid special code paths for cluster upgrades
specific to this work. Instead we accept that some queries will take longer
during cluster upgrade mixed version operation. This is described below.

### Updated coordinator, outdated shard

In this case, the coordinator will note the covering index, and set the view
query option `include_docs=false`. This means that the row passed to `view_cb/2`
will not have a document included. In the function, `case ViewRow#view_row.doc
of` will hit the `undefined` clause, meaning that the row is passed through
unchanged, without the document. When the row reaches the coordinator and is
passed to `doc_member_and_extract/2` from `handle_message/2`, the `case
couch_util:get_value(doc, RowProps) of` will also hit its `undefined` clause.
The coordinator will then perform a quorum read with `r=1` of the document and
carry out the match and extract.

This will slow down the processing of results at the coordinator for that row,
but shouldn't alter the correctness of the result. So we shouldn't need a
special code path to support this case. Which is nice.

### Outdated coordinator, updated shard

In this case, the coordinator won't be checking for covering indexes, meaning
that `include_docs=true` will be set when `r<2` as today.

I suspect we'll set an option in `viewcbargs` that contains the index field
names and whether it's a covering index. This means that an updated shard will
be checking for those fields. When it can't find them, it'll fallback to the
current behaviour in `view_cb/2`, meaning that it reads the document found via
`include_docs=true`, execute `match_and_extract_doc/3` and return the row if it
matches the query.

The coordinator will receive the final result document as today and assume it's
correct, and forward it to the client. More work than needed will be carried out
at the shard. But, again, this doesn't appear to require special code so long as
we are careful.


# Advantages and Disadvantages

[NOTE]: # ( Briefly, list the benefits and drawbacks that would be realized should )
[NOTE]: # ( the proposal be accepted for inclusion into Apache CouchDB. )

The primary advantage is allowing Mango queries to be as performant as View
queries while retaining their greater flexibility. It does not aim to replace
`reduce` functionality.

There should be no disadvantages to this change. Existing functionality remains
intact, and there should be no decreases in performance.

# Key Changes

[TIP]: # ( If the changes will affect how a user interacts with CouchDB, explain. )

Adding the extra field to Mango JSON index definitions is the key API change.

Users will generally have to alter their indexes to take best advantage of this
new functionality. As there has been no advantage to including extra items in
a JSON index's key so far, most indexes won't contain the data to cover queries.

For now, this RFC doesn't explicitly suggest exposing the covering index choice
via `_explain`, though this would be a valuable piece of extra work. We will,
however, need to ensure that the execution stats that each `_find` call spits
out are correct -- that is, for a covering index there should be no documents
read.

## Applications and Modules affected

[NOTE]: # ( List the OTP applications or functional modules in CouchDB affected by the proposal. )

- `mango`; see details above.

## HTTP API additions

[NOTE]: # ( Provide *exact* detail on each new API endpoint, including: )
[NOTE]: # (   HTTP methods [HEAD, GET, PUT, POST, DELETE, etc.] )
[NOTE]: # (   Synopsis of functionality )
[NOTE]: # (   Headers and parameters accepted )
[NOTE]: # (   JSON in [if a PUT or POST type] )
[NOTE]: # (   JSON out )
[NOTE]: # (   Valid status codes and their definitions )
[NOTE]: # (   A proposed Request and Response block )

Add field `include` to Mango JSON index definitions. Validate that it is only
accepted within JSON indexes and not text indexes.

```json
{
    "index": {
        "fields": [ "age", "name" ],
        "include": [ "occupation", "manager_id" ]
    },
    "name": "foo-json-index",
    "type": "json"
}
```

## HTTP API deprecations

[NOTE]: # ( Provide *exact* detail on the API endpoints to be deprecated. )
[NOTE]: # ( If these endpoints are replaced by new endpoints, list those as well. )
[NOTE]: # ( State the proposed version in which the deprecation and removal will occur. )

None.

# Security Considerations

[NOTE]: # ( Include any impact to the security of CouchDB here. )

This change is an optimisation to query performance. It should not have any
effects on CouchDB's security model.

# References

[TIP]:  # ( Include any references to CouchDB documentation, mailing list discussion, )
[TIP]:  # ( external standards or other links here. )

Crunchydata have a nice article on [why covering indexes are a valuable feature][crunch].

[crunch]: https://www.crunchydata.com/blog/why-covering-indexes-are-incredibly-helpful

# Acknowledgements

[TIP]:  # ( Who helped you write this RFC? )

This work builds on prior PoCs and work by Will Holley and Garren Smith, and
has benefited from discussion with them.
