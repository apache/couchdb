# CouchJS

## Drop-in replacement JavaScript engine for Apache CouchDB

CouchJS is a command-line Node.js program. It is 100% compatible with Apache CouchDB's built-in JavaScript system.

By using CouchJS, you will get 100% CouchDB compatibility (the test suite completely passes) but your JavaScript environment is V8, or Node.js.

CouchJS is available as an npm module.

    $ npm install -g couchjs

## Usage

Install CouchDB. Install this package with npm. Confirm your `couchjs` install location.

    $ which couchjs # Note, your path will be different from mine.
    /home/jhs/node/bin/couchjs

Look at the CouchDB config for the JavaScript query server.

    $ curl http://localhost:5984/_config/query_servers/javascript
    "/home/jhs/couchdb/bin/couchjs /home/jhs/couchdb/share/couchdb/server/main.js"

Change that to this `couchjs`. **Leave the second argument the same.**

    $ curl -X PUT http://localhost:5984/_config/query_servers/javascript \
      -H content-type:application/json \
      -d "\"`which couchjs` /home/jhs/couchdb/share/couchdb/server/main.js\""

Done!

## Idea

JavaScript is decoupled from the CouchDB core. To do JavaScript stuff, CouchDB runs a normal Unix subprocess, `couchjs`. This subprocess is just a read-eval-print loop on standard i/o. CouchDB passes `couchjs` a file name, and *that file* contains the view server  implementation.

This tool duplicates the "REPL" look and feel of `couchjs` and supports the exact same view server implementation.

## Security

I have no idea. I would not trust it for production use.

## Log

If you create a file, `/tmp/couchjs.log` then *couchjs* will output debugging messages there.

## License

Apache 2.0

See the [Apache 2.0 license](named/blob/master/LICENSE).

[tap]: https://github.com/isaacs/node-tap
[def]: https://github.com/iriscouch/defaultable
