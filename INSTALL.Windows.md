Apache CouchDB INSTALL.Windows
==============================

Due to the complexity of building CouchDB on the Windows platform,
full build documentation and all necessary support files are in
the couchdb-glazier repository.

Be sure to find the branch that matches the release you are building, for
example `couchdb_2.0`.

Build & Test
------------
Once all dependencies are built and installed per the documentation in
couchdb-glazier, these commands will configure and build CouchDB:

    powershell -ExecutionPolicy Bypass .\configure.ps1 -WithCurl
    make -f Makefile.win check

This will build couchdb, as well as run the eunit and javascript tests.

As of CouchDB 2.0 RC1, all eunit and javascript tests should pass.
