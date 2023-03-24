# nouveau
Lucene 9 + DropWizard = Maybe a good search option for Apache CouchDB?

Nouveau is an experimental search extension for CouchDB 3.x.

## What works?

* you can define a default analyzer and different analyzers by field name.
* sorting on text and numbers
* classic lucene query syntax
* count and range facets
* cursor support for paginating efficiently through large results sets
* indexes automatically deleted if database is deleted (as long as nouveau is running!)
* integration with ken
* update=false
* support for stale=ok
* integration with mango

## What doesn't work yet?

* include_docs=true
* No support for results grouping
* No support to configure stop words for analyzers

## Why is this better than dreyfus/clouseau?

* No scalang (or Scala!)
* Supports any version of Java that Lucene 9 supports
* memory-mapped I/O for performance
* direct I/O used for segment merging (so we don't evict useful data from disk cache)
* It's new and shiny.

## Getting started

Configure CouchDB with `--enable-nouveau'

Build Nouveau with;

`make`

Run Nouvea with;

`dev/run --admin=foo:bar --with-nouveau`

Make a database with some data and an index definition;

```
#!/bin/sh

URL="http://foo:bar@127.0.0.1:15984/foo"

curl -X DELETE "$URL"
curl -X PUT "$URL?n=3&q=16"

curl -X PUT "$URL/_design/foo" -d '{"nouveau":{"bar":{"lucene_major": 9, "default_analyzer":"standard", "field_analyzers":{"foo":"english"}, "index":"function(doc) { index(\"string\", \"foo\", \"bar\"); }"}}}'

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
(`sort=["fieldnamehere&lt;string&gt;"] or sort=["fieldnamehere&lt;number&gt;"],
defaulting to number).

Facet support

Counts of string fields and Ranges for numeric fields;

```
curl 'foo:bar@localhost:15984/foo/_design/foo/_nouveau/bar?q=*:*&limit=1&ranges={"bar":[{"label":"cheap","min":0,"max":100}]}&counts=["foo"]' -g
```

## Index function

To ease migration nouveau functions can use the 'index' function exactly as it exists in dreyfus, but the function also supports a new style.

| Arguments                                          | Effect
| :------------------------------------------------- | :-----
| index("foo", "bar");                               | adds a TextField.
| index("foo", "bar", {"store":true});               | adds a TextField and a StoredField.
| index("foo", "bar", {"store":true, "facet":true}); | adds a TextField, a StoredField and a SortedSetDocValuesField.
| index("text", "foo", "bar");                       | adds a TextField.
| index("text", "foo", "bar", {"store":true});       | adds a TextField with Store.YES
| index("string", "foo", "bar");                     | adds a StringField.
