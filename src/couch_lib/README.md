# Description

`couch_lib` application is a collection of "pure" miscellaneous functions.
They are "pure" in a sense that these functions should not call any other CouchDB
applications. Think of this application as an extension for Erlang/OTP standard library.

The two main reasons for this application to exist are:

- to share non CouchDB specific helper functions between applications
- avoid or break cyclic dependencies between applications

Please DO NOT put CouchDB specific functionality in here. This means you shouldn't:

- call couch_log:
- call config:
- rely on process dictionary values set by processes within CouchDB using `erlang:put/2`
- send messages to specific `gen_server` processes using `gen_server:call`

# Provided functionality


