Fauxton
=======

This is the initial implementation of Fauxton, focused on fleshing out
the various pieces of functionality and as a test bed for new ideas.
Full functionality and design considerations will be added later.

Current items of interest:

  * Live JSON editor with dynamic JS Hinting and error popups
  * Initial plugin system
  * Minimal externally loadable plugin example
  * Data popups for additional db info on \_all_dbs page
  * CouchDB API compliant urls

## Setup ##

Some recentish version of node.js and npm is required for the local
development and build tools.

### CouchDB Setup ###

    git clone https://github.com/cloudant-labs/couchdb/ couchdb-fauxton
    cd couchdb-fauxton
    git checkout fauxton
    ./bootstrap && ./configure && make dev

### Fauxton Setup ###

    cd src/fauxton

    # Install all dependencies
    npm install

    # Add node_modules/.bin to your path
    # export PATH=./node_modules/.bin:$PATH
		# Or just use the wrappers in ./bin/

    # Development mode, non minified files
    ./bin/bbb couchdebug

    # Or fully compiled install
    # ./bin/bbb couchdb

### Start CouchDB with Fauxton Installed ###

    ../../utils/run &
    open http://localhost:5984/_utils/fauxton/index.html

### Install as a Couchapp

Complete the setup process as above and run:

    ./bin/bbb couchapp_setup couchapp_install

Install [phantomjs](http://phantomjs.org/) for tests.
