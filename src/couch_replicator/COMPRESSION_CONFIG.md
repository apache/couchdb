# CouchDB Replicator Request Compression

The replicator can optionally gzip-compress outbound request bodies for
`_bulk_docs` and `_revs_diff`. This reduces bandwidth during replication.
CouchDB already supports `Content-Encoding: gzip` on inbound requests, so no
server-side changes are needed.

Compression is disabled by default. Relevant `[replicator]` config keys:

```ini
[replicator]
compress_requests = false
compress_min_size = 1024    ; minimum body size in bytes before compressing
```

Metric: `couch_replicator.requests_compressed.gzip` — number of gzip-compressed requests sent.
