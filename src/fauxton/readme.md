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
development and build tools. You will will also need backbone boiler
plate which will install the other dependencies needed.

    git clone https://github.com/cloudant-labs/couchdb/ couchdb-fauxton
    cd couchdb-fauxton
    git checkout fauxton
    ./bootstrap && ./configure && make dev
    cd src/fauxton
    npm install -g bbb
    npm install grunt-contrib-copy grunt-couchapp underscore
    bbb couchdb **or** bbb couchdebug
    ../../utils/run &
    open http://localhost:5984/_utils/fauxton/index.html

### Install as a Couchapp
    git clone https://github.com/cloudant-labs/couchdb/ couchdb-fauxton
    cd couchdb-fauxton
    git checkout fauxton
    cd src/fauxton
    npm install -g bbb # install bbb globally
    npm install grunt-contrib-copy grunt-couchapp underscore couchapp
    bbb couchapp_setup couchapp_install

If you don't want bbb installed globally you can install it as a node module in
the CWD:

    npm install bbb grunt-contrib-copy grunt-couchapp underscore couchapp
    node_modules/bbb/bin/bbb couchapp_setup couchapp_install

Install [phantomjs](http://phantomjs.org/) for tests.
