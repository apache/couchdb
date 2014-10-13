couchperuser
============

couchperuser is a CouchDB daemon that ensures that a private per-user
database exists for each document in _users. These databases are
writable only by the corresponding user.

Currently this is very much purpose-built for CodeCosmos. Databases are
in the form:

  userdb-{hex encoded username}

For example, the user `bob` will have a database named `userdb-626f62`.

The reason for hex encoding is that CouchDB usernames have no restrictions,
but CouchDB databases have restrictions. Hex encoding the UTF-8
representation of the username is a transformation that's easy to
correctly implement in just about any language, especially JavaScript
and Erlang. Other encodings would be possible, but would require
additional client and server-side code to support that encoding. This
is the simplest scheme that is obviously correct.

Installation
----

Install any dependencies. `couchperuser` requires `rebar`, e.g.

    $ brew install rebar

Ensure the `plugins` directory exists, e.g.

    $ mkdir /usr/local/Cellar/couchdb/1.6.0_1/lib/couchdb/plugins

Clone (download) the repo:

    $ git clone https://github.com/etrepum/couchperuser.git

Move the plugin files:

    $ mv couchperuser /usr/local/Cellar/couchdb/1.6.0_1/lib/couchdb/plugins

Build the plugin files:

    $ cd /usr/local/Cellar/couchdb/1.6.0_1/lib/couchdb/plugins
    $ make

Restart couchdb
