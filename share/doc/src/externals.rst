.. Licensed under the Apache License, Version 2.0 (the "License"); you may not
.. use this file except in compliance with the License. You may obtain a copy of
.. the License at
..
..   http://www.apache.org/licenses/LICENSE-2.0
..
.. Unless required by applicable law or agreed to in writing, software
.. distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
.. WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
.. License for the specific language governing permissions and limitations under
.. the License.


.. _externals:

=====================
CouchDB Externals API
=====================

:Author: Paul Joseph Davis
:Date: 2010-09-26
:Source: http://davispj.com/2010/09/26/new-couchdb-externals-api.html

For a bit of background, CouchDB has had an API for managing `external OS
processes`_ that are capable of handling HTTP requests for a given
URL prefix. These OS processes communicate with CouchDB using JSON over
stdio. They're dead simple to write and provide CouchDB users an easy way to
extend CouchDB functionality.

Even though they're dead simple to write, there are a few issues. The
implementation in CouchDB does not provide fancy pooling semantics. The
current API is explicitly synchronous which prevents people from writing
event driven code in an external handler. In the end, they may be simple,
but their simplicity is also quite limiting.

During CouchCamp a few weeks ago I had multiple discussions with various people
that wanted to see the _externals API modified in slight ways that weren't
mutually compatible. After having multiple discussions with multiple people
we formed a general consensus on what a new API could look like.

The New Hotness
---------------

So the first idea for improving the _external API was to make CouchDB act as
a reverse proxy. This would allow people to write an HTTP server that was as
simple or as complicated as they wanted. It will allow people to change their
networking configuration more easily and also allow for external processes to
be hosted on nodes other than the one running CouchDB. Bottom line, it not
only allows us to have similar semantics as _externals, it provides a lot more
fringe benefits as well. I'm always a fan of extra awesomeness.

After hitting on the idea of adding a reverse proxy, people quickly pointed
out that it would require users to start manually managing their external
processes using something like `Runit`_ or `Supervisord`_. After some
more discussions I ran into people that wanted something like _externals that
didn't handle HTTP requests. After that it was easy to see that adding a second
feature that managed OS processes was the way to go.

I spent this weekend implementing both of these features. Both are at the stage
of working but requiring more testing. In the case of the HTTP proxy I have no
tests because I can't decide how to test the thing. If you have ideas, I'd
sure like to hear them.

**[Update]**: I woke up the other morning realizing that I was being an idiot
and that Erlang is awesome. There's no reason that I can't have an HTTP client,
proxy, and server all hosted in the same process. So that's what I did. It
turns out to be a fairly nice way of configuring matching assertions between
the client and the server to test the proxy transmissions.

How does it work? - HTTP Proxying
---------------------------------

To configure a :ref:`proxy handler <config/proxy>`, edit your `local.ini` and
add a section like such::

    [httpd_global_handlers]
    _fti = {couch_httpd_proxy, handle_proxy_req, <<"http://127.0.0.1:5985">>}

This would be approximately what you'd need to do to get `CouchDB-Lucene`_
handled through this interface. The URL you use to access a query would be:

    http://127.0.0.1:5984/_fti/db_name/_design/foo/by_content?q=hello

A couple things to note here. Anything in the path after the configured proxy
name ("_fti" in this case) will be appended to the configured destination URL
("http://127.0.0.1:5985" in this case). The query string and any associated
body will also be proxied transparently.

Also, of note is that there's nothing that limits on what resources can be
proxied. You're free to choose any destination that the CouchDB node is capable
of communicating with.

How does it work? - OS Daemons
------------------------------

The second part of the new API gives CouchDB simple OS process management. When
CouchDB boots it will start each configured OS daemon. If one of these daemons
fails at some point, it will be restarted. If one of these daemons fails too
often, CouchDB will stop attempting to start it.

OS daemons are one-to-one. For each daemon, CouchDB will make sure that exactly
one instance of it is alive. If you have something where you want multiple
processes, you need to either tell CouchDB about each one, or have a main
process that forks off the required sub-processes.

To configure an :config:section:`OS daemon <os_daemons>`, add this to your
`local.ini`::

    [os_daemons]
    my_daemon = /path/to/command -with args

Configuration API
+++++++++++++++++

As an added benefit, because stdio is now free, I implemented a simple API
that OS daemons can use to read the configuration of their CouchDB host. This
way you can have them store their configuration inside CouchDB's config system
if you desire. Or they can peek at things like the
:config:option:`httpd/bind_address` and :config:option:`httpd/port` that CouchDB
is using.

A request for a config section looks like this::

    ["get", "os_daemons"]\n

And the response::

    {"my_daemon": "/path/to/command -with args"}\n

Or to get a specific key::

    ["get", "os_daemons", "my_daemon"]\n

And the response::

    "/path/to/command -with args"\n

All requests and responses are terminated with a newline (indicated by ``\n``).

Logging API
+++++++++++

There's also an API for adding messages to CouchDB's logs. Its simply::

    ["log", $MESG]\n

Where ``$MESG`` is any arbitrary JSON. There is no response from this command. As
with the config API, the trailing ``\n`` represents a newline byte.

Dynamic Daemons
+++++++++++++++

The OS daemons react in real time to changes to the configuration system. If
you set or delete keys in the :config:section:`os_daemons` section,
the corresponding daemons will be started or killed as appropriate.

Neat. But So What?
------------------

It was suggested that a good first demo would be a `Node.js`_ handler. So, I
present to you a "Hello, World" Node.js handler. Also, remember that this
currently relies on code in my fork on `GitHub`_.

File `node-hello-world.js`:

.. code-block:: javascript

    var http = require('http');
    var sys = require('sys');

    // Send a log message to be included in CouchDB's
    // log files.

    var log = function(mesg) {
      console.log(JSON.stringify(["log", mesg]));
    }

    // The Node.js example HTTP server

    var server = http.createServer(function (req, resp) {
      resp.writeHead(200, {'Content-Type': 'text/plain'});
      resp.end('Hello World\n');
      log(req.method + " " + req.url);
    })

    // We use stdin in a couple ways. First, we
    // listen for data that will be the requested
    // port information. We also listen for it
    // to close which indicates that CouchDB has
    // exited and that means its time for us to
    // exit as well.

    var stdin = process.openStdin();

    stdin.on('data', function(d) {
      server.listen(parseInt(JSON.parse(d)));
    });

    stdin.on('end', function () {
      process.exit(0);
    });

    // Send the request for the port to listen on.

    console.log(JSON.stringify(["get", "node_hello", "port"]));

File `local.ini` (Just add these to what you have):

.. code-block:: ini

    [log]
    level = info

    [os_daemons]
    node_hello = /path/to/node-hello-world.js

    [node_hello]
    port = 8000

    [httpd_global_handlers]
    _hello = {couch_httpd_proxy, handle_proxy_req, <<"http://127.0.0.1:8000">>}

And then start CouchDB and try:

.. code-block:: bash

    $ curl -v http://127.0.0.1:5984/_hello
    * About to connect() to 127.0.0.1 port 5984 (#0)
    *   Trying 127.0.0.1... connected
    * Connected to 127.0.0.1 (127.0.0.1) port 5984 (#0)
    > GET /_hello HTTP/1.1
    > User-Agent: curl/7.19.7 (universal-apple-darwin10.0) libcurl/7.19.7 OpenSSL/0.9.8l zlib/1.2.3
    > Host: 127.0.0.1:5984
    > Accept: */*
    >
    < HTTP/1.1 200
    < Transfer-Encoding: chunked
    < Server: CouchDB (Erlang/OTP)
    < Date: Mon, 27 Sep 2010 01:13:37 GMT
    < Content-Type: text/plain
    < Connection: keep-alive
    <
    Hello World
    * Connection #0 to host 127.0.0.1 left intact
    * Closing connection #0

The corresponding CouchDB logs look like::

    Apache CouchDB 1.5.0 (LogLevel=info) is starting.
    Apache CouchDB has started. Time to relax.
    [info] [<0.31.0>] Apache CouchDB has started on http://127.0.0.1:5984/
    [info] [<0.105.0>] 127.0.0.1 - - 'GET' /_hello 200
    [info] [<0.95.0>] Daemon "node-hello" :: GET /


.. _external OS processes: http://wiki.apache.org/couchdb/ExternalProcesses
.. _Runit: http://smarden.org/runit/
.. _Supervisord: http://supervisord.org/
.. _Node.js: http://nodejs.org/
.. _GitHub: http://github.com/davisp/couchdb/tree/new_externals
.. _CouchDB-Lucene: https://github.com/rnewson/couchdb-lucene
