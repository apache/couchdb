# CouchJS

## Drop-in replacement JavaScript engine for Apache CouchDB

CouchJS is a command-line Node.js program. It is 100% compatible with Apache CouchDB's built-in JavaScript system.

By using CouchJS, you will get 100% CouchDB compatibility (the test suite completely passes) but your JavaScript environment is V8, or Node.js.

See share/doc/src/experimental.rst for installation instructions.

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
