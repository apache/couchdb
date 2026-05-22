# CouchDB Replicator Compression

## Overview

The replicator now supports configurable HTTP compression to reduce bandwidth during replication.

## Configuration

```ini
[replicator]
; Enable compression (default: true)
compress_requests = true

; Minimum body size to compress in bytes (default: 1024)
compress_min_size = 1024

; Algorithm: gzip (default), deflate
compression_algorithm = gzip

; Accept these encodings in responses
accept_encodings = gzip, deflate
```

## Algorithms

- **gzip** (default): Best compatibility, built-in to Erlang
- **deflate**: Built-in to Erlang, slightly faster than gzip

## Statistics

- `couch_replicator.requests.compressed` - Total compressed requests
- `couch_replicator.requests.compressed.{algorithm}` - Per-algorithm stats