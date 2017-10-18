Mango Tests
===========

CouchDB should be started with `./dev/run -a testuser:testpass`.

To run these, do this in the Mango top level directory:

    $ virtualenv venv
    $ source venv/bin/activate
    $ make pip-install
    $ make test

To run an individual test suite:
    nosetests --nocapture test/12-use-correct-index.py 

To run the tests with text index support:
    MANGO_TEXT_INDEXES=1 nosetests --nocapture test


Test configuration
==================

The following environment variables can be used to configure the test fixtures:

COUCH_HOST - root url (including port) of the CouchDB instance to run the tests against. Default is "http://127.0.0.1:15984".
COUCH_USER - CouchDB username (with admin premissions). Default is "testuser"
COUCH_PASSWORD -  CouchDB password. Default is "testpass"
COUCH_AUTH_HEADER - Optional Authorization header value. If specified, this is used instead of basic authentication with the username/password variables above.
MANGO_TEXT_INDEXES - Set to "1" to run the tests only applicable to text indexes
