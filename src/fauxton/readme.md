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

    npm install -g bbb grunt-contrib-copy grunt-couchapp grunt-templater underscore
    bbb server

Now visit [http://localhost:8000](http://localhost:8000)
