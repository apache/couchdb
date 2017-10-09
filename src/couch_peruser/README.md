# couch_peruser [![Build Status](https://travis-ci.org/apache/couchdb-peruser.svg?branch=master)](https://travis-ci.org/apache/couchdb-peruser)

couch_peruser is a CouchDB application that ensures that a private per-user
database exists for each document in _users. These databases are
writable only by the corresponding user. Databases are in the form:

  userdb-{hex encoded username}

For example, the user `bob` will have a database named `userdb-626f62`.

The reason for hex encoding is that CouchDB usernames have no restrictions,
but CouchDB databases have restrictions. Hex encoding the UTF-8
representation of the username is a transformation that's easy to
correctly implement in just about any language, especially JavaScript
and Erlang. Other encodings would be possible, but would require
additional client and server-side code to support that encoding. This
is the simplest scheme that is obviously correct.

## Implementation Notes

The module itself is a `gen_server` and it implements the `mem3_cluster`
behaviour.

In a CouchDB cluster, the module runs on each node in the cluster. On startup,
it launches a changes listener for each shard of the `authentication_db`
(`_users`).

In a cluster, when a change notification comes in (after a user doc has been
created/updated/deleted), each node independently calculates if it should
handle the notification based on the current list of active nodes in the
cluster. This ensures that we avoid trying to update the internal `_dbs`
concurrently and causing conflicts. It also ensures that at least one node
does handle a notification. The mechanism that handles this does survive
cluster reconfigurations transparently.
