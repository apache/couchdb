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