# nouveau

Nouveau is a modern replacement for dreyfus/clouseau and is built on;

1) the Dropwizard framework (https://dropwizard.io)
2) Java 11+
3) Lucene 9

Nouveau transforms Apache CouchDB databases into Apache Lucene indexes at the shard level and then merges the results together.

This work is currently EXPERIMENTAL and may change in ways that invalidate any existing Nouveau index.

## What works?

* you can define a default analyzer and different analyzers by field name.
* sorting on text and numbers (and combinations of fields)
* classic lucene query syntax
* count and range facets
* bookmark support for paginating efficiently through large results sets
* indexes automatically deleted if database is deleted (as long as nouveau is running!)
* integration with ken
* integration with mango
* integration with resharding
* update=false
* `_nouveau_info`
* `_search_cleanup`
* /openapi.{json.yaml}

## What doesn't work yet?

* results grouping
* configurable stop words for analyzers
* Makefile.win or Windows generally

I don't intend to add grouping support, it seems barely used. Would accept a tidy contribution, though.

## Why is this better than dreyfus/clouseau?

* No scalang (or Scala!)
* Supports any version of Java that Lucene 9 supports
* memory-mapped I/O for performance (which works best on Java 19)
* direct I/O used for segment merging (so we don't evict useful data from disk cache)

## Getting started

Configure CouchDB with `--enable-nouveau`

Build Nouveau with;

`make`

Run Nouveau with;

`dev/run --admin=foo:bar --with-nouveau`

Make a database with some data and an index definition;

```
#!/bin/sh

URL="http://foo:bar@127.0.0.1:15984/foo"

curl -X DELETE "$URL"
curl -X PUT "$URL?n=3&q=16"

curl -X PUT "$URL/_design/foo" -d '{"nouveau":{"bar":{"default_analyzer":"standard", "field_analyzers":{"foo":"english"}, "index":"function(doc) { index(\"string\", \"foo\", \"bar\"); }"}}}'

# curl "$URL/_index" -Hcontent-type:application/json -d '{"type":"nouveau", "index": {"fields": [{"name": "bar", "type":"number"}]}}'

for I in {1..5}; do
    DOCID=$RANDOM
    DOCID=$[ $DOCID % 100000 ]
    BAR=$RANDOM
    BAR=$[ $BAR % 100000 ]
    curl -X PUT "$URL/doc$DOCID" -d "{\"bar\": $BAR}"
done

while true; do
    curl 'foo:bar@localhost:15984/foo/_design/foo/_nouveau/bar?q=*:*'
done
```

In order not to collide with `dreyfus` I've hooked Nouveau in with new paths;

`curl 'foo:bar@localhost:15984/foo/_design/foo/_nouveau/bar?q=*:*'`

This will cause Nouveau to build indexes for each copy (N) and each
shard range (Q) and then perform a search and return the results. Lots
of query syntax is working as is sorting on strings and numbers
(`sort=["fieldnamehere&lt;string&gt;"] or sort=["fieldnamehere&lt;number&gt;"`],
defaulting to number).

Facet support

Counts of string fields and Ranges for numeric fields;

```
curl 'foo:bar@localhost:15984/foo/_design/foo/_nouveau/bar?q=*:*&limit=1&ranges={"bar":[{"label":"cheap","min":0,"max":100}]}&counts=["foo"]' -g
```

## Index function

| Arguments                                                       | Effect
| :-------------------------------------------------------------- | :-----
| index("text", "foo", "bar", {"store": true});                   | analyzes value for full-text searching, optionally stores the value
| index("string", "foo", "bar", {"store": true});                 | indexes value as single token, optionally stores value
| index("double", "foo", 12.0, {"store": true});                  | indexes value, optionally stores value
| index("stored", "foo", "bar");                                  | stores a number, returned with hits
| index("stored", "foo", 12.0);                                   | stores a string, returned with hits

## Deployment options

All indexes are prefix with their erlang hostname so you can deploy a
single nouveau server per cluster if this meets your needs. You can
also configure a different nouveau server for each couchdb node too.

There is no need to co-locate the nouveau server with the couchdb
cluster, though this is a common option.
