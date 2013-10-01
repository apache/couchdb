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

.. highlight:: ini

=============
Query Servers
=============

.. _config/query_servers:

Query Servers Definition
========================

.. config:section:: query_servers :: Query Servers Definition

  .. versionchanged:: 1.2: Added CoffeeScript query server

  CouchDB delegates computation of :ref:`design documents <ddocs>` functions to
  external query servers. The external query server is a special OS process which
  communicates with CouchDB over standard input/output using a very simple
  line-based protocol with JSON messages.

  The external query server may be defined in configuration file following next
  pattern::

    [query_servers]
    LANGUAGE = PATH ARGS

  Where:

  - ``LANGUAGE``: is a programming language which code this query server may
    execute. For instance, there are `python`, `ruby`, `clojure` and other query
    servers in wild. This value is also used for `ddoc` field ``language``
    to determine query server that will process his functions.

    Note, that you may setup multiple query servers for the same programming
    language, but you have to name them different (like `python-dev` etc.).

  - ``PATH``: is a system path to the executable binary program that runs the
    query server.

  - ``ARGS``: optionally, you may specify additional command line arguments for
    the executable ``PATH``.

  The default query server is written in :ref:`JavaScript <query-server/js>`,
  running via `Mozilla SpiderMonkey`_::

    [query_servers]
    javascript = /usr/bin/couchjs /usr/share/couchdb/server/main.js
    coffeescript = /usr/bin/couchjs /usr/share/couchdb/server/main-coffee.js


  .. _Mozilla SpiderMonkey: https://developer.mozilla.org/en/docs/SpiderMonkey

  .. seealso::
     :ref:`Native Erlang Query Server <config/native_query_servers>` that allows
     to process Erlang `ddocs` and runs within CouchDB bypassing stdio
     communication and JSON serialization/deserialization round trip overhead.


.. _config/query_server_config:

Query Servers Configuration
===========================

.. config:section:: query_server_config :: Query Servers Configuration


  .. config:option:: commit_freq :: View index commit delay

    Specifies the delay in seconds before view index changes are committed to disk.
    The default value is ``5``::

      [query_server_config]
      commit_freq = 5


  .. config:option:: os_process_limit :: Query Server operation timeout

    Amount of time in seconds that the Query Server may process CouchDB command::

      [query_server_config]
      os_process_limit = 10

    CouchDB will raise `os_process_timeout` error and kill the process in case the
    Query Server doesn't return any result within this limit.


  .. config:option:: reduce_limit :: Reduce limit control

    Controls `Reduce overflow` error that raises when output of
    :ref:`reduce functions <reducefun>` is too big::

      [query_server_config]
      reduce_limit = true

    Normally, you don't have to disable (by setting ``false`` value) this option
    since main propose of `reduce` functions is to *reduce* the input.


.. _config/native_query_servers:

Native Erlang Query Server
==========================

.. config:section:: native_query_servers :: Native Erlang Query Server

  .. warning::

     Due to security restrictions, the Erlang query server is disabled by
     default.

     Unlike the JavaScript query server, the Erlang one does not runs in a sandbox
     mode. This means that Erlang code has full access to your OS,
     filesystem and network, which may lead to security issues. While Erlang
     functions are faster than JavaScript ones, you need to be careful
     about running them, especially if they were written by someone else.

  CouchDB has a native Erlang query server, allowing you to write your map/reduce
  functions in Erlang.

  First, you'll need to edit your `local.ini` to include a
  ``[native_query_servers]`` section::

    [native_query_servers]
    erlang = {couch_native_process, start_link, []}

  To see these changes you will also need to restart the server.
  To test out using :ref:`Erlang views <query-server/erlang>`, visit the
  `Futon` admin interface, create a new database and open a temporary view.
  You should now be able to select ``erlang`` from the language drop-down.

  Let's try an example of map/reduce functions which count the total documents at
  each number of revisions (there are x many documents at version "1", and y
  documents at "2"... etc). Add a few documents to the database, then enter the
  following functions as a temporary view:

  .. code-block:: erlang

    %% Map Function
    fun({Doc}) ->
      <<K,_/binary>> = proplists:get_value(<<"_rev">>, Doc, null),
      V = proplists:get_value(<<"_id">>, Doc, null),
      Emit(<<K>>, V)
    end.

    %% Reduce Function
    fun(Keys, Values, ReReduce) -> length(Values) end.

  If all has gone well, after running the view you should see a list of the total
  number of documents at each revision number.
