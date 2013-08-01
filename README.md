couchperuser
============

couchperuser is a CouchDB daemon that ensures that a private per-user
database exists for each document _users.

Currently this is very much purpose-built for CodeCosmos. Databases are
in the form:

  userdb-{hex encoded username}

For example, the user `bob` will have a database named `userdb-626f62`.
