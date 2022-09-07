---
name: Pagination support
about: Add bookmark based pagination interface
title: 'Pagination API'
labels: rfc, discussion
assignees: ''

---

Implement pagination support for query like endpoints

# Introduction

The main driver for this proposal is the introduction of FoundationDB as a
storage engine. FoundationDB imposes limits on transaction duration and size.
Therefore we need to find way to restrict amount of data we return to customers.
We could simply set the maximum `limit` which would cap amount of rows client can
request. However this "solution" has a big disadvantage. Which is it would require
clients to write pagination recipe in their code. Current pagination scheme
requires complex logic on the client side. There are quite a few corner cases
to handle.

## Abstract

The main addition is to add new bookmark based pagination scheme to all quiery
like endpoints. As a first step we would keep `_all_dbs`, `_dbs_info` and
`_changes` out of scope for the following reasons:
- _all_dbs - the endpoint returns a list instead of object as the rest of the endpoints
- _dbs_info - the endpoint returns a list instead of object as the rest of the endpoints
- _changes - the endpoint contains too many different modes and it would require
  more careful consideration

The endpoints in scope are:
- {db}/_all_docs
- {db}/_all_docs/queries
- {db}/_design/{ddoc}/_view/{view}
- {db}/_design/{ddoc}/_view/{view}/queries

In a nutshell the idea is:
- add `page_size` query field to control number of rows on each page and to flag
  that client expects paginated response
- add `first`, `previous`, `next` fields which contain bookmark part of URI
- add `bookmark` query field to retrieve bookmarked page


## Requirements Language

The key words "MUST", "MUST NOT", "REQUIRED", "SHALL", "SHALL NOT",
"SHOULD", "SHOULD NOT", "RECOMMENDED",  "MAY", and "OPTIONAL" in this
document are to be interpreted as described in
[RFC 2119](https://www.rfc-editor.org/rfc/rfc2119.txt).

## Terminology

- `bookmark` - is opaque token which would contain information needed to retrieve
  bookmarked page. The format of the token value MUST NOT be relied upon in the client.

---

# Detailed Description

## Implementation proposal

1) Add new optional query field called `bookmark` to following endpoints
  - {db}/_all_docs
  - {db}/_all_docs/queries
  - {db}/_design/{ddoc}/_view/{view}
  - {db}/_design/{ddoc}/_view/{view}/queries

2) Add new optional query field called `page_size` if it is set we would use
  paginated endpoint otherwise use old code path
3) Implement per-endpoint configurable max limits to restrict page size of
  paginated responses

 ```
 [request_limits]
 _all_docs = 5000
 _all_docs/queries = 5000
 _all_dbs = 5000
 _dbs_info = 5000
 _view = 2500
 _view/queries = 2500
 _find = 2500
 ```
4) Add following additional fields into response:
  ```
    "first": "12345678945621321689",
    "previous": "983uiwfjkdsdf",
    "next": "12343tyekf3"
  ```

## Limitations

- The `first`/`next`/`last` keys in the response are represented as path which
  includes the bookmark query key. This means the bookmark token size contributes
  to total URI length and is subject to a max URL length (around 2000 characters).
  This means storing `keys` in a bookmark is not an option. For that reason
  `POST` method is not supported when pagination is enabled
- Ideally we would want to signal (return 400) when number of rows returned from
  streaming version of the endpoint goes over limit configured in `request_limit`.
  However with streaming we've already sent a return code.

## Semantics of the implementation

- Only GET method would have pagination support
- The bookmark would include information needed to ensure proper pagination
  without the need to repeat initial parameters of the request.
- Don't use delayed responses when `bookmark` field is provided
- Don't use delayed responses when `page_size` query key is specified and when
  it is below the max limit
- Return 400 when `bookmark` field is set and other query fields are present
- Return 400 when `page_size` query key is specified and it is greater than
  the max limit
- The `previous`/`next`/`first` keys are optional and we omit them for the cases
  they don't make sense
- Set default value for `page_size` to `limit` if provided `limit` is less than
  value configured in `request_limit` of `default.ini` for the given endpoint
- Set default value for `page_size` to the value configured in `request_limit`
  of `default.ini` for the given endpoint
- Once the `limit` is reached the final response will not have a "next" bookmark
- The maximum value for `skip` query parameter is limited to the same `page_size` or
  value configured in `request_limit` whatever is less
- Once the underlying call to FoundationDB returns less than `page_size`
  the response will not have a "next" bookmark
- When `page_size` is used with `_all_docs/queries` or `{db}/_design/{ddoc}/_view/{view}/queries`
  the specified limit applies to number of queries provided in the request.
- For `_all_docs/queries` and `{db}/_design/{ddoc}/_view/{view}/queries` the total
  number of rows returned shouldn't exceed provided `page_size` or configured
  max limit (whatever is less)
- Paginated requests are subject to FDB transaction timeout. This is implemented
  via lack of `{restart_tx, true}` option for FDB calls.
- The request to `_all_docs/queries` and `{db}/_design/{ddoc}/_view/{view}/queries`
  can include bookmarks:
  ```
  {"queries": [
    {"bookmark": "bookmarkForQuery1PageL"},
    {"bookmark": "bookmarkForQuery2PageM"},
    {"bookmark": "bookmarkForQuery3PageN"}
    ]
  }
  ```
- Every bookmark returned by `_all_docs/queries` and `{db}/_design/{ddoc}/_view/{view}/queries`
  can be submitted via separate request to `_all_docs` and `{db}/_design/{ddoc}/_view/{view}`
  correspondly.


## Configuration

The page size limits are configured in `default.ini` (or another `ini` file) in
`request_limit` section as follows:

```
 [request_limits]
 _all_docs = 5000
 _all_docs/queries = 5000
 _all_dbs = 5000
 _dbs_info = 5000
 _view = 2500
 _view/queries = 2500
 _find = 2500
```

## Roadmap

- initial implementation as described in this document
- create API versioning proposal and implement the feature
- create separate proposal for `_changes` endpoint
- implement pagination enabled version of `_all_dbs` and `_dbs_info` which would
  change response type to be object (using versioned API feature)

# Key Changes

- New configuration section
- New query fields
- New fields in response body
- Enforcing strict limits on number of rows requested by client

## Applications and Modules affected

- chttpd

## HTTP API additions

N/A

## HTTP API deprecations

N/A

# Security Considerations

No changes to the security model

# References

- [Streaming API in CouchDB 4.0 discussion](https://lists.apache.org/thread.html/r02cee7045cac4722e1682bb69ba0ec791f5cce025597d0099fb34033%40%3Cdev.couchdb.apache.org%3E)
- [Streaming API in CouchDB 4.0 discussion part 2](https://lists.apache.org/thread.html/ra8d16937cca332207d772844d2789f932fbc4572443a354391663b9c%40%3Cdev.couchdb.apache.org%3E)
- [API versioning discussion](https://lists.apache.org/thread.html/rcc742c0fdca0363bb338b54526045720868597ea35ee6842aef174e0%40%3Cdev.couchdb.apache.org%3E)

# Acknowledgements

[TIP]:  # ( Who helped you write this RFC? )

Thank you to all partitipants in the discussion threads. You all helped to shape
and refine this proposal in one form or another.

- Adam Kocoloski
- Garren Smith
- Glynn Bird
- Joan Touzet
- Mike Rhodes
- Nick Vatamaniuc
- Paul Davis
- Richard Ellis
- Robert Samuel Newson
