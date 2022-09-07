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

.. versionchanged:: 2.3 Changed configuration method for Query Servers
  and Native Query Servers.

CouchDB delegates computation of :ref:`design documents <ddocs>` functions
to external query servers. The external query server is a special OS
process which communicates with CouchDB over standard input/output using a
very simple line-based protocol with JSON messages.

An external query server may be defined with environment variables following
this pattern::

    COUCHDB_QUERY_SERVER_LANGUAGE="PATH ARGS"

Where:

- ``LANGUAGE``: is a programming language which code this query server may
  execute. For instance, there are `PYTHON`, `RUBY`, `CLOJURE` and other
  query servers in the wild. This value in *lowercase* is also used for ``ddoc``
  field ``language`` to determine which query server processes the functions.

  Note, that you may set up multiple query servers for the same programming
  language, but you have to name them differently (like `PYTHONDEV` etc.).

- ``PATH``: is a system path to the executable binary program that runs the
  query server.

- ``ARGS``: optionally, you may specify additional command line arguments
  for the executable ``PATH``.

The default query server is written in :ref:`JavaScript <query-server/js>`,
running via `Mozilla SpiderMonkey`_. It requires no special environment
settings to enable, but is the equivalent of these two variables::

    COUCHDB_QUERY_SERVER_JAVASCRIPT="/opt/couchdb/bin/couchjs /opt/couchdb/share/server/main.js"
    COUCHDB_QUERY_SERVER_COFFEESCRIPT="/opt/couchdb/bin/couchjs /opt/couchdb/share/server/main-coffee.js"

By default, ``couchjs`` limits the max runtime allocation to 64MiB.
If you run into out of memory issue in your ddoc functions,
you can adjust the memory limitation (here, increasing to 512 MiB)::

    COUCHDB_QUERY_SERVER_JAVASCRIPT="/usr/bin/couchjs -S 536870912 /usr/share/server/main.js"

For more info about the available options, please consult ``couchjs -h``.

.. _Mozilla SpiderMonkey: https://spidermonkey.dev/

.. seealso::
    The :ref:`Mango Query Server <api/db/_find>` is a declarative language
    that requires *no programming*, allowing for easier indexing and finding
    of data in documents.

    The :ref:`Native Erlang Query Server <config/native_query_servers>`
    allows running `ddocs` written in Erlang natively, bypassing
    stdio communication and JSON serialization/deserialization round trip
    overhead.

.. _config/query_server_config:

Query Servers Configuration
===========================

.. config:section:: query_server_config :: Query Servers Configuration

    .. config:option:: commit_freq :: View index commit delay

        Specifies the delay in seconds before view index changes are committed
        to disk. The default value is ``5``::

            [query_server_config]
            commit_freq = 5

    .. config:option:: os_process_limit :: Query Server process hard limit

        Hard limit on the number of OS processes usable by Query
        Servers. The default value is ``100``::

            [query_server_config]
            os_process_limit = 100

        Setting ``os_process_limit`` too low can result in starvation of
        Query Servers, and manifest in ``os_process_timeout`` errors,
        while setting it too high can potentially use too many system
        resources. Production settings are typically 10-20 times the
        default value.

    .. config:option:: os_process_soft_limit :: Query Server process soft limit

        Soft limit on the number of OS processes usable by Query
        Servers. The default value is ``100``::

            [query_server_config]
            os_process_soft_limit = 100

        Idle OS processes are closed until the total reaches the soft
        limit.

        For example, if the hard limit is 200 and the soft limit is
        100, the total number of OS processes will never exceed 200,
        and CouchDB will close all idle OS processes until it reaches
        100, at which point it will leave the rest intact, even if
        some are idle.

    .. config:option:: reduce_limit :: Reduce limit control

        Controls `Reduce overflow` error that raises when output of
        :ref:`reduce functions <reducefun>` is too big::

            [query_server_config]
            reduce_limit = true

        Normally, you don't have to disable (by setting ``false`` value) this
        option since main propose of `reduce` functions is to *reduce* the
        input.

.. _config/native_query_servers:

Native Erlang Query Server
==========================

.. config:section:: native_query_servers :: Native Erlang Query Server

    .. warning::
        Due to security restrictions, the Erlang query server is disabled by
        default.

        Unlike the JavaScript query server, the Erlang one does not run in a
        sandbox mode. This means that Erlang code has full access to your OS,
        file system and network, which may lead to security issues. While Erlang
        functions are faster than JavaScript ones, you need to be careful
        about running them, especially if they were written by someone else.

    CouchDB has a native Erlang query server, allowing you to write your
    map/reduce functions in Erlang.

    First, you'll need to edit your `local.ini` to include a
    ``[native_query_servers]`` section::

        [native_query_servers]
        enable_erlang_query_server = true

    To see these changes you will also need to restart the server.

    Let's try an example of map/reduce functions which count the total
    documents at each number of revisions (there are x many documents at
    version "1", and y documents at "2"... etc). Add a few documents to the
    database, then enter the following functions as a view:

    .. code-block:: erlang

        %% Map Function
        fun({Doc}) ->
            <<K,_/binary>> = proplists:get_value(<<"_rev">>, Doc, null),
            V = proplists:get_value(<<"_id">>, Doc, null),
            Emit(<<K>>, V)
        end.

        %% Reduce Function
        fun(Keys, Values, ReReduce) -> length(Values) end.

    If all has gone well, after running the view you should see a list of the
    total number of documents at each revision number.

    Additional examples are on the `users@couchdb.apache.org mailing list
    <https://lists.apache.org/thread.html/9b5f2837bd32189385bb82eee44aec243f2ecacc6e907ffe0e1e03d3@1360091211@%3Cuser.couchdb.apache.org%3E>`_.

.. _config/search:

Search
======

CouchDB's search subsystem can be configured via the ``dreyfus`` configuration section.

.. config:section:: dreyfus :: Search Subsystem Configuration

    .. config:option:: name :: Clouseau JVM node name and location

        The name and location of the Clouseau Java service required to enable Search
        functionality. Defaults to ``clouseau@127.0.0.1``.

    .. config:option:: retry_limit :: Maximum number of connection retries

        CouchDB will try to reconnect to Clouseau using a bounded exponential backoff with
        the following number of iterations. Defaults to ``5``.

    .. config:option:: limit :: Default result set limit for global search

        The number of results returned from a global search query if no limit is
        specified. Defaults to ``25``.

    .. config:option:: limit_partitions :: Default result set limit for partitioned DBs

        The number of results returned from a search on a partition of a database if no
        limit is specified. Defaults to ``2000``.

    .. config:option:: max_limit :: Maximum result set for global search

        The maximum number of results that can be returned from a global search query (or
        any search query on a database without user-defined partitions). Attempts to set
        ``?limit=N higher`` than this value will be rejected. Defaults to ``200``.

    .. config:option:: max_limit_partitions :: Maximum result set for partitioned search

        The maximum number of results that can be returned when searching a partition of a
        database. Attempts to set ``?limit=N`` higher than this value will be rejected. If
        this config setting is not defined, CouchDB will use the value of ``max_limit``
        instead. If neither is defined, the default is ``2000``.

.. _config/mango:

Mango
=====

Mango is the Query Engine that services the :ref:`_find <api/db/_find>`, endpoint.

.. config:section:: mango :: Mango Configuration

    .. config:option:: index_all_disabled :: Disable "index all fields" behaviour

        Set to ``true`` to disable the "index all fields" text index. This can lead
        to out of memory issues when there are documents with nested array fields.
        Defaults to ``false``.::

            [mango]
            index_all_disabled = false

    .. config:option:: default_limit :: Default limit value for Mango queries.

        Sets the default number of results that will be returned in a
        :ref:`_find <api/db/_find>` response. Individual requests can override this
        by setting ``limit`` directly in the query parameters.
        Defaults to ``25``.::

            [mango]
            default_limit = 25

    .. config:option:: index_scan_warning_threshold :: Ratio threshold that generates \
        an index scan warning

        This sets the ratio between documents scanned and results matched that
        will generate a warning in the _find response. For example, if a query
        requires reading 100 documents to return 10 rows, a warning will be
        generated if this value is ``10``.

        Defaults to ``10``. Setting the value to ``0`` disables the warning.::

            [mango]
            index_scan_warning_threshold = 10
