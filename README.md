couch_peruser
===============

couch_peruser is a CouchDB daemon that ensures that a private per-user
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
