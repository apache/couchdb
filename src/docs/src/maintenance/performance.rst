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

.. _performance:

===========
Performance
===========

With up to tens of thousands of documents you will generally find CouchDB to
perform well no matter how you write your code. Once you start getting into
the millions of documents you need to be a lot more careful.

Disk I/O
========

File Size
---------

The smaller your file size, the less `I/O` operations there will be,
the more of the file can be cached by CouchDB and the operating system,
the quicker it is to replicate, backup etc. Consequently you should carefully
examine the data you are storing. For example it would be silly to use keys
that are hundreds of characters long, but your program would be hard to
maintain if you only used single character keys. Carefully consider data
that is duplicated by putting it in views.

Disk and File System Performance
--------------------------------

Using faster disks, striped RAID arrays and modern file systems can all speed
up your CouchDB deployment. However, there is one option that can increase
the responsiveness of your CouchDB server when disk performance is a
bottleneck. From the Erlang documentation for the file module:

    On operating systems with thread support, it is possible to let file
    operations be performed in threads of their own, allowing other Erlang
    processes to continue executing in parallel with the file operations.
    See the `command line flag +A in erl(1)`_.

Setting this argument to a number greater than zero can keep your CouchDB
installation responsive even during periods of heavy disk utilization. The
easiest way to set this option is through the ``ERL_FLAGS`` environment
variable. For example, to give Erlang four threads with which to perform I/O
operations add the following to ``(prefix)/etc/defaults/couchdb``
(or equivalent)::

    export ERL_FLAGS="+A 4"

.. _command line flag +A in erl(1): http://erlang.org/doc/man/erl.html

System Resource Limits
======================

One of the problems that administrators run into as their deployments become
large are resource limits imposed by the system and by the application
configuration. Raising these limits can allow your deployment to grow beyond
what the default configuration will support.

CouchDB Configuration Options
-----------------------------

max_dbs_open
^^^^^^^^^^^^

In your :ref:`configuration <config>` (local.ini or similar) familiarize
yourself with the :config:option:`couchdb/max_dbs_open`:

.. code-block:: ini

    [couchdb]
    max_dbs_open = 100

This option places an upper bound on the number of databases that can be
open at one time. CouchDB reference counts database accesses internally and
will close idle databases when it must. Sometimes it is necessary to keep
more than the default open at once, such as in deployments where many databases
will be continuously replicating.

Erlang
------

Even if you've increased the maximum connections CouchDB will allow,
the Erlang runtime system will not allow more than 65536 connections by
default. Adding the following directive to ``(prefix)/etc/vm.args`` (or
equivalent) will increase this limit (in this case to 102400)::

    +Q 102400

Note that on Windows, Erlang will not actually increase the file descriptor
limit past 8192 (i.e. the system header–defined value of ``FD_SETSIZE``). On
macOS, the limit may be as low as 1024. See `this tip for a possible
workaround`_ and `this thread for a deeper explanation`_.

.. _this tip for a possible workaround: http://erlang.org/pipermail/erlang-questions/2011-December/063119.html
.. _this thread for a deeper explanation: http://erlang.org/pipermail/erlang-questions/2011-October/061971.html

Maximum open file descriptors (ulimit)
--------------------------------------

In general, modern UNIX-like systems can handle very large numbers of file
handles per process (e.g. 100000) without problem. Don't be afraid to increase
this limit on your system.

The method of increasing these limits varies, depending on your init system and
particular OS release. The default value for many OSes is 1024 or 4096. On a
system with many databases or many views, CouchDB can very rapidly hit this
limit.

For systemd-based Linuxes (such as CentOS/RHEL 7, Ubuntu 16.04+, Debian 8
or newer), assuming you are launching CouchDB from systemd, you must
override the upper limit via editing the override file. The best practice
for this is via the ``systemctl edit couchdb`` command. Add these lines to
the file in the editor::

    [Service]
    LimitNOFILE=65536

...or whatever value you like. To increase this value higher than 65536, you
must also add the Erlang ``+Q`` parameter to your ``etc/vm.args`` file by
adding the line::

    +Q 102400

The old ``ERL_MAX_PORTS`` environment variable is ignored by the version of
Erlang supplied with CouchDB.

If your system is set up to use the Pluggable Authentication Modules (`PAM`_),
and you are **not** launching CouchDB from systemd, increasing this limit
is straightforward. For example, creating a file named
``/etc/security/limits.d/100-couchdb.conf`` with the following contents will
ensure that CouchDB can open up to 65536 file descriptors at once::

    #<domain>    <type>    <item>    <value>
    couchdb      hard      nofile    65536
    couchdb      soft      nofile    65536

If you are using our Debian/Ubuntu sysvinit script (``/etc/init.d/couchdb``),
you also need to raise the limits for the root user::

    #<domain>    <type>    <item>    <value>
    root         hard      nofile    65536
    root         soft      nofile    65536

You may also have to edit the ``/etc/pam.d/common-session`` and
``/etc/pam.d/common-session-noninteractive`` files to add the line::

    session required pam_limits.so

if it is not already present.

If your system does not use PAM, a `ulimit` command is usually available for
use in a custom script to launch CouchDB with increased resource limits.
Typical syntax would be something like `ulimit -n 65536`.

.. _PAM: http://www.linux-pam.org/

Network
=======

There is latency overhead making and receiving each request/response.
In general you should do your requests in batches. Most APIs have some
mechanism to do batches, usually by supplying lists of documents or keys in
the request body. Be careful what size you pick for the batches. The larger
batch requires more time your client has to spend encoding the items into JSON
and more time is spent decoding that number of responses. Do some benchmarking
with your own configuration and typical data to find the sweet spot.
It is likely to be between one and ten thousand documents.

If you have a fast I/O system then you can also use concurrency - have
multiple requests/responses at the same time. This mitigates the latency
involved in assembling JSON, doing the networking and decoding JSON.

As of CouchDB 1.1.0, users often report lower write performance of documents
compared to older releases. The main reason is that this release ships with
the more recent version of the HTTP server library MochiWeb, which by default
sets the TCP socket option `SO_NODELAY`_ to false. This means that small data
sent to the TCP socket, like the reply to a document write request (or reading
a very small document), will not be sent immediately to the network - TCP will
buffer it for a while hoping that it will be asked to send more data through
the same socket and then send all the data at once for increased performance.
This TCP buffering behaviour can be disabled via
:config:option:`httpd/socket_options`:

.. code-block:: ini

    [httpd]
    socket_options = [{nodelay, true}]

.. _SO_NODELAY: http://en.wikipedia.org/wiki/Nagle%27s_algorithm

.. seealso::
    Bulk :ref:`load <api/db/all_docs>` and :ref:`store <api/db/bulk_docs>` API.

Connection limit
----------------

`MochiWeb`_ handles CouchDB requests.
The default maximum number of connections is 2048. To change this limit, use the
``server_options`` configuration variable. ``max`` indicates maximum number of
connections.

.. code-block:: ini

    [chttpd]
    server_options = [{backlog, 128}, {acceptor_pool_size, 16}, {max, 4096}]

.. _MochiWeb: https://github.com/mochi/mochiweb

CouchDB
=======

DELETE operation
----------------

When you :method:`DELETE` a document the database will create a new
revision which contains the ``_id`` and ``_rev`` fields as well as
the `_deleted` flag. This revision will remain even after a `database
compaction` so that the deletion can be replicated. Deleted documents, like
non-deleted documents, can affect view build times, :method:`PUT` and
:method:`DELETE` request times, and the size of the database since they
increase the size of the B+Tree. You can see the number of deleted documents
in :get:`database information </{db}>`. If your use case creates lots of
deleted documents (for example, if you are storing short-term data like log
entries, message queues, etc), you might want to periodically switch to a new
database and delete the old one (once the entries in it have all expired).

Document's ID
-------------

The db file size is derived from your document and view sizes but also on a
multiple of your ``_id`` sizes. Not only is the ``_id`` present in the document,
but it and parts of it are duplicated in the binary tree structure CouchDB uses
to navigate the file to find the document in the first place. As a real world
example for one user switching from 16 byte ids to 4 byte ids made a database
go from 21GB to 4GB with 10 million documents (the raw JSON text when from
2.5GB to 2GB).

Inserting with sequential (and at least sorted) ids is faster than random ids.
Consequently you should consider generating ids yourself, allocating them
sequentially and using an encoding scheme that consumes fewer bytes.
For example, something that takes 16 hex digits to represent can be done in
4 base 62 digits (10 numerals, 26 lower case, 26 upper case).

Views
=====

.. _views/generation:

Views Generation
----------------

Views with the JavaScript query server are extremely slow to generate when
there are a non-trivial number of documents to process. The generation process
won't even saturate a single CPU let alone your I/O. The cause is the latency
involved in the CouchDB server and separate `couchjs` query server, dramatically
indicating how important it is to take latency out of your implementation.

You can let view access be "stale" but it isn't practical to determine when
that will occur giving you a quick response and when views will be updated
which will take a long time. (A 10 million document database took about 10
minutes to load into CouchDB but about 4 hours to do view generation).

In a cluster, "stale" requests are serviced by a fixed set of shards in order
to present users with consistent results between requests. This comes with an
availability trade-off - the fixed set of shards might not be the most
responsive / available within the cluster. If you don't need this kind of
consistency (e.g. your indexes are relatively static), you can tell CouchDB to
use any available replica by specifying ``stable=false&update=false`` instead of
``stale=ok``, or ``stable=false&update=lazy`` instead of ``stale=update_after``.

View information isn't replicated - it is rebuilt on each database so you
can't do the view generation on a separate sever.

Built-In Reduce Functions
-------------------------

If you’re using a very simple view function that only performs a sum or count
reduction, you can call native Erlang implementations of them by simply
writing ``_sum`` or ``_count`` in place of your function declaration.
This will speed up things dramatically, as it cuts down on IO between CouchDB
and the :ref:`JavaScript query server <query-server/js>`. For example, as
`mentioned on the mailing list`_, the time for outputting an (already indexed
and cached) view with about 78,000 items went down from 60 seconds to 4 seconds.

Before:

.. code-block:: javascript

    {
        "_id": "_design/foo",
        "views": {
            "bar": {
                "map": "function (doc) { emit(doc.author, 1); }",
                "reduce": "function (keys, values, rereduce) { return sum(values); }"
            }
        }
    }

After:

.. code-block:: javascript

    {
        "_id": "_design/foo",
        "views": {
            "bar": {
                "map": "function (doc) { emit(doc.author, 1); }",
                "reduce": "_sum"
            }
        }
    }

.. _mentioned on the mailing list: http://mail-archives.apache.org/mod_mbox/couchdb-user/201003.mbox/%3c5E07E00E-3D69-4A8C-ADA3-1B20CF0BA4C8@julianstahnke.com%3e

.. seealso::
    :ref:`reducefun/builtin`
