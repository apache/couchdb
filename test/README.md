Mango Tests
===========

CouchDB should be started with ./dev/run -a testuser:testpass.

To run these, do this in the top level directory:

    $ virtualenv venv
    $ source venv/bin/activate
    $ pip install nose requests
    $ pip install hypothesis
    $ nosetests
