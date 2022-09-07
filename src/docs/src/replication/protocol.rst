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

.. _replication/protocol:

============================
CouchDB Replication Protocol
============================

:Version: 3

The `CouchDB Replication Protocol` is a protocol for synchronising JSON
documents between 2 peers over HTTP/1.1 by using the public :ref:`CouchDB REST
API <api>` and is based on the Apache CouchDB MVCC_ Data model.

Preface
=======

Language
--------

The key words "MUST", "MUST NOT", "REQUIRED", "SHALL", "SHALL NOT",
"SHOULD", "SHOULD NOT", "RECOMMENDED", "MAY", and "OPTIONAL" in this
document are to be interpreted as described in :rfc:`2119`.

Goals
-----

The primary goal of this specification is to describe the `CouchDB Replication
Protocol` under the hood.

The secondary goal is to provide enough detailed information about the protocol
to make it easy to build tools on any language and platform that can synchronize
data with CouchDB.

Definitions
-----------

JSON:
    :abbr:`JSON (JavaScript Object Notation)` is a text format for the
    serialization of structured data. It is described in `ECMA-262`_ and
    :rfc:`4627`.

URI:
    A URI is defined by :rfc:`3986`. It can be a URL as defined
    in :rfc:`1738`.

ID:
    An identifier (could be a UUID) as described in :rfc:`4122`.

Revision:
    A `MVCC`_ token value of following pattern: ``N-sig`` where ``N`` is ALWAYS
    a positive integer and ``sig`` is the Document signature (custom).
    Don't mix it up with the revision in version control systems!

Leaf Revision:
    The last Document Revision in a series of changes. Documents may have
    multiple Leaf Revisions (aka Conflict Revisions) due to concurrent updates.

Document:
    A document is a JSON object with an ID and Revision defined in ``_id`` and
    ``_rev`` fields respectively. A Document's ID MUST be unique within
    the Database where it is stored.

Database:
    A collection of Documents with a unique URI.

Changes Feed:
    A stream of Document-changing events (create, update, delete) for
    the specified Database.

Sequence ID:
    An ID provided by the Changes Feed. It MUST be incremental,
    but MAY NOT always be an integer.

Source:
    Database from where the Documents are replicated.

Target:
    Database where the Documents are replicated to.

Replication:
    The one-way directed synchronization process of Source and Target endpoints.

Checkpoint:
    Intermediate Recorded Sequence ID used for Replication recovery.

Replicator:
    A service or an application which initiates and runs Replication.

Filter Function:
    A special function of any programming language that is used to filter
    Documents during Replication (see :ref:`filterfun`)

Filter Function Name:
    An ID of a Filter Function that may be used as a symbolic reference (aka
    callback function) to apply the related Filter Function to Replication.

Filtered Replication:
    Replication of Documents from Source to Target using a Filter Function.

Full Replication:
    Replication of all Documents from Source to Target.

Push Replication:
    Replication process where Source is a local endpoint and Target is remote.

Pull Replication:
    Replication process where Source is a remote endpoint and Target is local.

Continuous Replication:
    Replication that "never stops": after processing all events from the
    Changes Feed, the Replicator doesn't close the connection, but awaits new
    change events from the Source. The connection is kept alive by periodic
    heartbeats.

Replication Log:
    A special Document that holds Replication history (recorded Checkpoints
    and a few more statistics) between Source and Target.

Replication ID:
    A unique value that unambiguously identifies the Replication Log.

Replication Protocol Algorithm
==============================

The `CouchDB Replication Protocol` is not *magical*, but
an agreement on usage of the public :ref:`CouchDB HTTP REST API <api>` to
enable Documents to be replicated from Source to Target.

The reference implementation, written in Erlang_, is provided by the
couch_replicator_ module in Apache CouchDB.

It is RECOMMENDED that one follow this algorithm specification, use the same
HTTP endpoints, and run requests with the same parameters to provide a
completely compatible implementation. Custom Replicator implementations MAY use
different HTTP API endpoints and request parameters depending on their local
specifics and they MAY implement only part of the Replication Protocol to run
only Push or Pull Replication. However, while such solutions could also run the
Replication process, they loose compatibility with the CouchDB Replicator.

Verify Peers
------------

.. code-block:: text

    + - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - +
    ' Verify Peers:                                                             '
    '                                                                           '
    '                404 Not Found   +--------------------------------+         '
    '       +----------------------- |     Check Source Existence     |         '
    '       |                        +--------------------------------+         '
    '       |                        |          HEAD /source          |         '
    '       |                        +--------------------------------+         '
    '       |                          |                                        '
    '       |                          | 200 OK                                 '
    '       |                          v                                        '
    '       |                        +--------------------------------+         '
    '       |                        |     Check Target Existence     | ----+   '
    '       |                        +--------------------------------+     |   '
    '       |                        |         HEAD /target           |     |   '
    '       |                        +--------------------------------+     |   '
    '       |                          |                                    |   '
    '       |                          | 404 Not Found                      |   '
    '       v                          v                                    |   '
    '   +-------+    No              +--------------------------------+     |   '
    '   | Abort | <----------------- |         Create Target?         |     |   '
    '   +-------+                    +--------------------------------+     |   '
    '       ^                          |                                    |   '
    '       |                          | Yes                                |   '
    '       |                          v                                    |   '
    '       |        Failure         +--------------------------------+     |   '
    '       +----------------------- |          Create Target         |     |   '
    '                                +--------------------------------+     |   '
    '                                |           PUT /target          |     |   '
    '                                +--------------------------------+     |   '
    '                                  |                                    |   '
    '                                  | 201 Created                 200 OK |   '
    '                                  |                                    |   '
    + - - - - - - - - - - - - - - - -  | - - - - - - - - - - - - - - - - -  | - +
                                       |                                    |
    + - - - - - - - - - - - - - - - -  | - - - - - - - - - - - - - - - - -  | - +
    ' Get Peers Information:           |                                    |   '
    '                                  +------------------------------------+   '
    '                                  |                                        '
    '                                  v                                        '
    '                                +--------------------------------+         '
    '                                |     Get Source Information     |         '
    '                                +--------------------------------+         '
    '                                                                           '
    + - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - +

The Replicator MUST ensure that both Source and Target exist
by using :head:`/{db}` requests.

Check Source Existence
^^^^^^^^^^^^^^^^^^^^^^

    **Request**:

    .. code-block:: http

        HEAD /source HTTP/1.1
        Host: localhost:5984
        User-Agent: CouchDB

    **Response**:

    .. code-block:: http

        HTTP/1.1 200 OK
        Cache-Control: must-revalidate
        Content-Type: application/json
        Date: Sat, 05 Oct 2013 08:50:39 GMT
        Server: CouchDB (Erlang/OTP)

Check Target Existence
^^^^^^^^^^^^^^^^^^^^^^

    **Request**:

    .. code-block:: http

        HEAD /target HTTP/1.1
        Host: localhost:5984
        User-Agent: CouchDB

    **Response**:

    .. code-block:: http

        HTTP/1.1 200 OK
        Cache-Control: must-revalidate
        Content-Type: application/json
        Date: Sat, 05 Oct 2013 08:51:11 GMT
        Server: CouchDB (Erlang/OTP)

Create Target?
^^^^^^^^^^^^^^

In case of a non-existent Target, the Replicator MAY make a :put:`/{db}`
request to create the Target:

    **Request**:

    .. code-block:: http

        PUT /target HTTP/1.1
        Accept: application/json
        Host: localhost:5984
        User-Agent: CouchDB

    **Response**:

    .. code-block:: http

        HTTP/1.1 201 Created
        Content-Length: 12
        Content-Type: application/json
        Date: Sat, 05 Oct 2013 08:58:41 GMT
        Server: CouchDB (Erlang/OTP)

        {
            "ok": true
        }

However, the Replicator's PUT request MAY NOT succeeded due to insufficient
privileges (which are granted by the provided credential) and so receive a
:statuscode:`401` or a :statuscode:`403` error. Such errors SHOULD be expected
and well handled:

    .. code-block:: http

        HTTP/1.1 500 Internal Server Error
        Cache-Control: must-revalidate
        Content-Length: 108
        Content-Type: application/json
        Date: Fri, 09 May 2014 13:50:32 GMT
        Server: CouchDB (Erlang OTP)

        {
            "error": "unauthorized",
            "reason": "unauthorized to access or create database http://localhost:5984/target"
        }

Abort
^^^^^

In case of a non-existent Source or Target, Replication SHOULD be aborted with
an HTTP error response:

    .. code-block:: http

        HTTP/1.1 500 Internal Server Error
        Cache-Control: must-revalidate
        Content-Length: 56
        Content-Type: application/json
        Date: Sat, 05 Oct 2013 08:55:29 GMT
        Server: CouchDB (Erlang OTP)

        {
            "error": "db_not_found",
            "reason": "could not open source"
        }

Get Peers Information
---------------------

.. code-block:: text

    + - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -+
    ' Verify Peers:                                                    '
    '                         +------------------------+               '
    '                         | Check Target Existence |               '
    '                         +------------------------+               '
    '                                     |                            '
    '                                     | 200 OK                     '
    '                                     |                            '
    + - - - - - - - - - - - - - - - - - - | - - - - - - - - - - - - - -+
                                          |
    + - - - - - - - - - - - - - - - - - - | - - - - - - - - - - - - - -+
    ' Get Peers Information:              |                            '
    '                                     v                            '
    '                         +------------------------+               '
    '                         | Get Source Information |               '
    '                         +------------------------+               '
    '                         |      GET /source       |               '
    '                         +------------------------+               '
    '                                     |                            '
    '                                     | 200 OK                     '
    '                                     v                            '
    '                         +------------------------+               '
    '                         | Get Target Information |               '
    '                         +------------------------+               '
    '                         |      GET /target       |               '
    '                         +------------------------+               '
    '                                     |                            '
    '                                     | 200 OK                     '
    '                                     |                            '
    + - - - - - - - - - - - - - - - - - - | - - - - - - - - - - - - - -+
                                          |
    + - - - - - - - - - - - - - - - - - - | - - - - - - - - - - - - - -+
    ' Find Common Ancestry:               |                            '
    '                                     |                            '
    '                                     v                            '
    '                         +-------------------------+              '
    '                         | Generate Replication ID |              '
    '                         +-------------------------+              '
    '                                                                  '
    + - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -+

The Replicator retrieves basic information both from Source and Target using
:get:`/{db}` requests. The GET response MUST contain JSON objects with
the following mandatory fields:

- **instance_start_time** (*string*): Always ``"0"``. (Returned for legacy
  reasons.)
- **update_seq** (*number* / *string*): The current database Sequence ID.

Any other fields are optional. The information that the Replicator needs
is the ``update_seq`` field: this value will be used to define a *temporary*
(because Database data is subject to change) upper bound for changes feed
listening and statistic calculating to show proper Replication progress.

Get Source Information
^^^^^^^^^^^^^^^^^^^^^^

    **Request**:

    .. code-block:: http

        GET /source HTTP/1.1
        Accept: application/json
        Host: localhost:5984
        User-Agent: CouchDB

    **Response**:

    .. code-block:: http

        HTTP/1.1 200 OK
        Cache-Control: must-revalidate
        Content-Length: 256
        Content-Type: application/json
        Date: Tue, 08 Oct 2013 07:53:08 GMT
        Server: CouchDB (Erlang OTP)

        {
            "committed_update_seq": 61772,
            "compact_running": false,
            "db_name": "source",
            "disk_format_version": 6,
            "doc_count": 41961,
            "doc_del_count": 3807,
            "instance_start_time": "0",
            "purge_seq": 0,
            "sizes": {
              "active": 70781613961,
              "disk": 79132913799,
              "external": 72345632950
            },
            "update_seq": 61772
        }

Get Target Information
^^^^^^^^^^^^^^^^^^^^^^

    **Request**:

    .. code-block:: http

        GET /target/ HTTP/1.1
        Accept: application/json
        Host: localhost:5984
        User-Agent: CouchDB

    **Response**:

    .. code-block:: http

        HTTP/1.1 200 OK
        Content-Length: 363
        Content-Type: application/json
        Date: Tue, 08 Oct 2013 12:37:01 GMT
        Server: CouchDB (Erlang/OTP)

        {
            "compact_running": false,
            "db_name": "target",
            "disk_format_version": 5,
            "doc_count": 1832,
            "doc_del_count": 1,
            "instance_start_time": "0",
            "purge_seq": 0,
            "sizes": {
              "active": 50829452,
              "disk": 77001455,
              "external": 60326450
            },
            "update_seq": "1841-g1AAAADveJzLYWBgYMlgTmGQT0lKzi9KdUhJMtbLSs1LLUst0k"
        }

Find Common Ancestry
------------------------

.. code-block:: text

    + - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - +
    ' Get Peers Information:                                                    '
    '                                                                           '
    '                             +-------------------------------------------+ '
    '                             |           Get Target Information          | '
    '                             +-------------------------------------------+ '
    '                               |                                           '
    + - - - - - - - - - - - - - - - | - - - - - - - - - - - - - - - - - - - - - +
                                    |
    + - - - - - - - - - - - - - - - | - - - - - - - - - - - - - - - - - - - - - +
    ' Find Common Ancestry:         v                                           '
    '                             +-------------------------------------------+ '
    '                             |          Generate Replication ID          | '
    '                             +-------------------------------------------+ '
    '                               |                                           '
    '                               |                                           '
    '                               v                                           '
    '                             +-------------------------------------------+ '
    '                             |      Get Replication Log from Source      | '
    '                             +-------------------------------------------+ '
    '                             |     GET /source/_local/replication-id     | '
    '                             +-------------------------------------------+ '
    '                               |                                           '
    '                               | 200 OK                                    '
    '                               | 404 Not Found                             '
    '                               v                                           '
    '                             +-------------------------------------------+ '
    '                             |      Get Replication Log from Target      | '
    '                             +-------------------------------------------+ '
    '                             |     GET /target/_local/replication-id     | '
    '                             +-------------------------------------------+ '
    '                               |                                           '
    '                               | 200 OK                                    '
    '                               | 404 Not Found                             '
    '                               v                                           '
    '                             +-------------------------------------------+ '
    '                             |          Compare Replication Logs         | '
    '                             +-------------------------------------------+ '
    '                               |                                           '
    '                               | Use latest common sequence as start point '
    '                               |                                           '
    + - - - - - - - - - - - - - - - | - - - - - - - - - - - - - - - - - - - - - +
                                    |
                                    |
    + - - - - - - - - - - - - - - - | - - - - - - - - - - - - - - - - - - - - - +
    ' Locate Changed Documents:     |                                           '
    '                               |                                           '
    '                               v                                           '
    '                             +-------------------------------------------+ '
    '                             |        Listen Source Changes Feed         | '
    '                             +-------------------------------------------+ '
    '                                                                           '
    + - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - +

Generate Replication ID
^^^^^^^^^^^^^^^^^^^^^^^

Before Replication is started, the Replicator MUST generate a Replication ID.
This value is used to track Replication History, resume and continue previously
interrupted Replication process.

The Replication ID generation algorithm is implementation specific. Whatever
algorithm is used it MUST uniquely identify the Replication process. CouchDB's
Replicator, for example, uses the following factors in generating a Replication
ID:

- Persistent Peer UUID value. For CouchDB, the local
  :config:option:`Server UUID <couchdb/uuid>` is used
- Source and Target URI and if Source or Target are local or remote Databases
- If Target needed to be created
- If Replication is Continuous
- Any custom headers
- :ref:`Filter function <filterfun>` code if used
- Changes Feed query parameters, if any

.. note::
    See `couch_replicator_ids.erl`_ for an example of a Replication ID generation
    implementation.

    .. _couch_replicator_ids.erl: https://github.com/apache/couchdb/blob/main/src/couch_replicator/src/couch_replicator_ids.erl

Retrieve Replication Logs from Source and Target
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Once the Replication ID has been generated, the Replicator SHOULD retrieve
the Replication Log from both Source and Target using
:get:`/{db}/_local/{docid}`:

    **Request**:

    .. code-block:: http

        GET /source/_local/b3e44b920ee2951cb2e123b63044427a HTTP/1.1
        Accept: application/json
        Host: localhost:5984
        User-Agent: CouchDB

    **Response**:

    .. code-block:: http

        HTTP/1.1 200 OK
        Cache-Control: must-revalidate
        Content-Length: 1019
        Content-Type: application/json
        Date: Thu, 10 Oct 2013 06:18:56 GMT
        ETag: "0-8"
        Server: CouchDB (Erlang OTP)

        {
            "_id": "_local/b3e44b920ee2951cb2e123b63044427a",
            "_rev": "0-8",
            "history": [
                {
                    "doc_write_failures": 0,
                    "docs_read": 2,
                    "docs_written": 2,
                    "end_last_seq": 5,
                    "end_time": "Thu, 10 Oct 2013 05:56:38 GMT",
                    "missing_checked": 2,
                    "missing_found": 2,
                    "recorded_seq": 5,
                    "session_id": "d5a34cbbdafa70e0db5cb57d02a6b955",
                    "start_last_seq": 3,
                    "start_time": "Thu, 10 Oct 2013 05:56:38 GMT"
                },
                {
                    "doc_write_failures": 0,
                    "docs_read": 1,
                    "docs_written": 1,
                    "end_last_seq": 3,
                    "end_time": "Thu, 10 Oct 2013 05:56:12 GMT",
                    "missing_checked": 1,
                    "missing_found": 1,
                    "recorded_seq": 3,
                    "session_id": "11a79cdae1719c362e9857cd1ddff09d",
                    "start_last_seq": 2,
                    "start_time": "Thu, 10 Oct 2013 05:56:12 GMT"
                },
                {
                    "doc_write_failures": 0,
                    "docs_read": 2,
                    "docs_written": 2,
                    "end_last_seq": 2,
                    "end_time": "Thu, 10 Oct 2013 05:56:04 GMT",
                    "missing_checked": 2,
                    "missing_found": 2,
                    "recorded_seq": 2,
                    "session_id": "77cdf93cde05f15fcb710f320c37c155",
                    "start_last_seq": 0,
                    "start_time": "Thu, 10 Oct 2013 05:56:04 GMT"
                }
            ],
            "replication_id_version": 3,
            "session_id": "d5a34cbbdafa70e0db5cb57d02a6b955",
            "source_last_seq": 5
        }

The Replication Log SHOULD contain the following fields:

- **history** (*array* of *object*): Replication history. **Required**

  - **doc_write_failures** (*number*): Number of failed writes
  - **docs_read** (*number*): Number of read documents
  - **docs_written** (*number*): Number of written documents
  - **end_last_seq** (*number*): Last processed Update Sequence ID
  - **end_time** (*string*): Replication completion timestamp in :rfc:`5322`
    format
  - **missing_checked** (*number*): Number of checked revisions on Source
  - **missing_found** (*number*): Number of missing revisions found on Target
  - **recorded_seq** (*number*): Recorded intermediate Checkpoint. **Required**
  - **session_id** (*string*): Unique session ID. Commonly, a random UUID value
    is used. **Required**
  - **start_last_seq** (*number*): Start update Sequence ID
  - **start_time** (*string*): Replication start timestamp in :rfc:`5322` format

- **replication_id_version** (*number*): Replication protocol version. Defines
  Replication ID calculation algorithm, HTTP API calls and the others
  routines. **Required**
- **session_id** (*string*): Unique ID of the last session. Shortcut to
  the ``session_id`` field of the latest ``history`` object. **Required**
- **source_last_seq** (*number*): Last processed Checkpoint. Shortcut to
  the ``recorded_seq`` field of the latest ``history`` object. **Required**

This request MAY fall with a :statuscode:`404` response:

    **Request**:

    .. code-block:: http

        GET /source/_local/b6cef528f67aa1a8a014dd1144b10e09 HTTP/1.1
        Accept: application/json
        Host: localhost:5984
        User-Agent: CouchDB

    **Response**:

    .. code-block:: http

        HTTP/1.1 404 Object Not Found
        Cache-Control: must-revalidate
        Content-Length: 41
        Content-Type: application/json
        Date: Tue, 08 Oct 2013 13:31:10 GMT
        Server: CouchDB (Erlang OTP)

        {
            "error": "not_found",
            "reason": "missing"
        }

That's OK. This means that there is no information about the current Replication
so it must not have been run previously and as such the Replicator MUST run
a Full Replication.

Compare Replication Logs
^^^^^^^^^^^^^^^^^^^^^^^^

If the Replication Logs are successfully retrieved from both Source and Target
then the Replicator MUST determine their common ancestry by following the next
algorithm:

- Compare ``session_id`` values for the chronological last session - if they
  match both Source and Target have a common Replication history and it seems
  to be valid. Use ``source_last_seq`` value for the startup Checkpoint

- In case of mismatch, iterate over the ``history`` collection to search for
  the latest (chronologically) common ``session_id`` for Source and Target.
  Use value of ``recorded_seq`` field as startup Checkpoint

If Source and Target has no common ancestry, the Replicator MUST run
Full Replication.

Locate Changed Documents
------------------------

.. code-block:: text

    + - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - +
    ' Find Common Ancestry:                                                     '
    '                                                                           '
    '             +------------------------------+                              '
    '             |   Compare Replication Logs   |                              '
    '             +------------------------------+                              '
    '                                          |                                '
    '                                          |                                '
    + - - - - - - - - - - - - - - - - - - - -  |  - - - - - - - - - - - - - - - +
                                               |
    + - - - - - - - - - - - - - - - - - - - -  |  - - - - - - - - - - - - - - - +
    ' Locate Changed Documents:                |                                '
    '                                          |                                '
    '                                          |                                '
    '                                          v                                '
    '            +-------------------------------+                              '
    '   +------> |     Listen to Changes Feed    | -----+                       '
    '   |        +-------------------------------+      |                       '
    '   |        |     GET  /source/_changes     |      |                       '
    '   |        |     POST /source/_changes     |      |                       '
    '   |        +-------------------------------+      |                       '
    '   |                                      |        |                       '
    '   |                                      |        |                       '
    '   |                There are new changes |        | No more changes       '
    '   |                                      |        |                       '
    '   |                                      v        v                       '
    '   |        +-------------------------------+    +-----------------------+ '
    '   |        |     Read Batch of Changes     |    | Replication Completed | '
    '   |        +-------------------------------+    +-----------------------+ '
    '   |                                      |                                '
    '   | No                                   |                                '
    '   |                                      v                                '
    '   |        +-------------------------------+                              '
    '   |        |  Compare Documents Revisions  |                              '
    '   |        +-------------------------------+                              '
    '   |        |    POST /target/_revs_diff    |                              '
    '   |        +-------------------------------+                              '
    '   |                                      |                                '
    '   |                               200 OK |                                '
    '   |                                      v                                '
    '   |        +-------------------------------+                              '
    '   +------- |     Any Differences Found?    |                              '
    '            +-------------------------------+                              '
    '                                          |                                '
    '                                      Yes |                                '
    '                                          |                                '
    + - - - - - - - - - - - - - - - - - - - -  |  - - - - - - - - - - - - - - - +
                                               |
    + - - - - - - - - - - - - - - - - - - - -  |  - - - - - - - - - - - - - - - +
    ' Replicate Changes:                       |                                '
    '                                          v                                '
    '            +-------------------------------+                              '
    '            |  Fetch Next Changed Document  |                              '
    '            +-------------------------------+                              '
    '                                                                           '
    + - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - +

Listen to Changes Feed
^^^^^^^^^^^^^^^^^^^^^^

When the start up Checkpoint has been defined, the Replicator SHOULD read
the Source's :ref:`Changes Feed <changes>` by using a :get:`/{db}/_changes`
request. This request MUST be made with the following query parameters:

- ``feed`` parameter defines the Changes Feed response style: for Continuous
  Replication the ``continuous`` value SHOULD be used, otherwise - ``normal``.

- ``style=all_docs`` query parameter tells the Source that it MUST include
  all Revision leaves for each document's event in output.

- For Continuous Replication the ``heartbeat`` parameter defines the heartbeat
  period in *milliseconds*. The RECOMMENDED value by default is ``10000``
  (10 seconds).

- If a startup Checkpoint was found during the Replication Logs comparison,
  the ``since`` query parameter MUST be passed with this value.
  In case of Full Replication it MAY be ``0`` (number zero) or
  be omitted.

Additionally, the ``filter`` query parameter MAY be specified to enable a
:ref:`filter function <changes/filter>` on Source side. Other
custom parameters MAY also be provided.

Read Batch of Changes
^^^^^^^^^^^^^^^^^^^^^

Reading the whole feed in a single shot may not be an optimal use of resources.
It is RECOMMENDED to process the feed in small chunks. However, there is
no specific recommendation on chunk size since it is heavily dependent on
available resources: large chunks requires more memory while they reduce
I/O operations and vice versa.

Note, that Changes Feed output format is different for a request with
:ref:`feed=normal <changes/normal>` and with
:ref:`feed=continuous <changes/continuous>` query parameter.

Normal Feed:

    **Request**:

    .. code-block:: http

        GET /source/_changes?feed=normal&style=all_docs&heartbeat=10000 HTTP/1.1
        Accept: application/json
        Host: localhost:5984
        User-Agent: CouchDB

    **Response**:

    .. code-block:: http

        HTTP/1.1 200 OK
        Cache-Control: must-revalidate
        Content-Type: application/json
        Date: Fri, 09 May 2014 16:20:41 GMT
        Server: CouchDB (Erlang OTP)
        Transfer-Encoding: chunked

        {"results":[
        {"seq":14,"id":"f957f41e","changes":[{"rev":"3-46a3"}],"deleted":true}
        {"seq":29,"id":"ddf339dd","changes":[{"rev":"10-304b"}]}
        {"seq":37,"id":"d3cc62f5","changes":[{"rev":"2-eec2"}],"deleted":true}
        {"seq":39,"id":"f13bd08b","changes":[{"rev":"1-b35d"}]}
        {"seq":41,"id":"e0a99867","changes":[{"rev":"2-c1c6"}]}
        {"seq":42,"id":"a75bdfc5","changes":[{"rev":"1-967a"}]}
        {"seq":43,"id":"a5f467a0","changes":[{"rev":"1-5575"}]}
        {"seq":45,"id":"470c3004","changes":[{"rev":"11-c292"}]}
        {"seq":46,"id":"b1cb8508","changes":[{"rev":"10-ABC"}]}
        {"seq":47,"id":"49ec0489","changes":[{"rev":"157-b01f"},{"rev":"123-6f7c"}]}
        {"seq":49,"id":"dad10379","changes":[{"rev":"1-9346"},{"rev":"6-5b8a"}]}
        {"seq":50,"id":"73464877","changes":[{"rev":"1-9f08"}]}
        {"seq":51,"id":"7ae19302","changes":[{"rev":"1-57bf"}]}
        {"seq":63,"id":"6a7a6c86","changes":[{"rev":"5-acf6"}],"deleted":true}
        {"seq":64,"id":"dfb9850a","changes":[{"rev":"1-102f"}]}
        {"seq":65,"id":"c532afa7","changes":[{"rev":"1-6491"}]}
        {"seq":66,"id":"af8a9508","changes":[{"rev":"1-3db2"}]}
        {"seq":67,"id":"caa3dded","changes":[{"rev":"1-6491"}]}
        {"seq":68,"id":"79f3b4e9","changes":[{"rev":"1-102f"}]}
        {"seq":69,"id":"1d89d16f","changes":[{"rev":"1-3db2"}]}
        {"seq":71,"id":"abae7348","changes":[{"rev":"2-7051"}]}
        {"seq":77,"id":"6c25534f","changes":[{"rev":"9-CDE"},{"rev":"3-00e7"},{"rev":"1-ABC"}]}
        {"seq":78,"id":"SpaghettiWithMeatballs","changes":[{"rev":"22-5f95"}]}
        ],
        "last_seq":78}

Continuous Feed:

    **Request**:

    .. code-block:: http

        GET /source/_changes?feed=continuous&style=all_docs&heartbeat=10000 HTTP/1.1
        Accept: application/json
        Host: localhost:5984
        User-Agent: CouchDB

    **Response**:

    .. code-block:: http

        HTTP/1.1 200 OK
        Cache-Control: must-revalidate
        Content-Type: application/json
        Date: Fri, 09 May 2014 16:22:22 GMT
        Server: CouchDB (Erlang OTP)
        Transfer-Encoding: chunked

        {"seq":14,"id":"f957f41e","changes":[{"rev":"3-46a3"}],"deleted":true}
        {"seq":29,"id":"ddf339dd","changes":[{"rev":"10-304b"}]}
        {"seq":37,"id":"d3cc62f5","changes":[{"rev":"2-eec2"}],"deleted":true}
        {"seq":39,"id":"f13bd08b","changes":[{"rev":"1-b35d"}]}
        {"seq":41,"id":"e0a99867","changes":[{"rev":"2-c1c6"}]}
        {"seq":42,"id":"a75bdfc5","changes":[{"rev":"1-967a"}]}
        {"seq":43,"id":"a5f467a0","changes":[{"rev":"1-5575"}]}
        {"seq":45,"id":"470c3004","changes":[{"rev":"11-c292"}]}
        {"seq":46,"id":"b1cb8508","changes":[{"rev":"10-ABC"}]}
        {"seq":47,"id":"49ec0489","changes":[{"rev":"157-b01f"},{"rev":"123-6f7c"}]}
        {"seq":49,"id":"dad10379","changes":[{"rev":"1-9346"},{"rev":"6-5b8a"}]}
        {"seq":50,"id":"73464877","changes":[{"rev":"1-9f08"}]}
        {"seq":51,"id":"7ae19302","changes":[{"rev":"1-57bf"}]}
        {"seq":63,"id":"6a7a6c86","changes":[{"rev":"5-acf6"}],"deleted":true}
        {"seq":64,"id":"dfb9850a","changes":[{"rev":"1-102f"}]}
        {"seq":65,"id":"c532afa7","changes":[{"rev":"1-6491"}]}
        {"seq":66,"id":"af8a9508","changes":[{"rev":"1-3db2"}]}
        {"seq":67,"id":"caa3dded","changes":[{"rev":"1-6491"}]}
        {"seq":68,"id":"79f3b4e9","changes":[{"rev":"1-102f"}]}
        {"seq":69,"id":"1d89d16f","changes":[{"rev":"1-3db2"}]}
        {"seq":71,"id":"abae7348","changes":[{"rev":"2-7051"}]}
        {"seq":75,"id":"SpaghettiWithMeatballs","changes":[{"rev":"21-5949"}]}
        {"seq":77,"id":"6c255","changes":[{"rev":"9-CDE"},{"rev":"3-00e7"},{"rev":"1-ABC"}]}
        {"seq":78,"id":"SpaghettiWithMeatballs","changes":[{"rev":"22-5f95"}]}

For both Changes Feed formats record-per-line style is preserved to simplify
iterative fetching and decoding JSON objects with less memory footprint.

Calculate Revision Difference
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

After reading the batch of changes from the Changes Feed, the Replicator forms a
JSON mapping object for Document ID and related leaf Revisions and sends
the result to Target via a :post:`/{db}/_revs_diff` request:

    **Request**:

    .. code-block:: http

        POST /target/_revs_diff HTTP/1.1
        Accept: application/json
        Content-Length: 287
        Content-Type: application/json
        Host: localhost:5984
        User-Agent: CouchDB

        {
            "baz": [
                "2-7051cbe5c8faecd085a3fa619e6e6337"
            ],
            "foo": [
                "3-6a540f3d701ac518d3b9733d673c5484"
            ],
            "bar": [
                "1-d4e501ab47de6b2000fc8a02f84a0c77",
                "1-967a00dff5e02add41819138abb3284d"
            ]
        }

    **Response**:

    .. code-block:: http

        HTTP/1.1 200 OK
        Cache-Control: must-revalidate
        Content-Length: 88
        Content-Type: application/json
        Date: Fri, 25 Oct 2013 14:44:41 GMT
        Server: CouchDB (Erlang/OTP)

        {
            "baz": {
                "missing": [
                    "2-7051cbe5c8faecd085a3fa619e6e6337"
                ]
            },
            "bar": {
                "missing": [
                    "1-d4e501ab47de6b2000fc8a02f84a0c77"
                ]
            }
        }

In the response the Replicator receives a Document ID -- Revisions mapping,
but only for Revisions that do not exist in Target and are REQUIRED to be
transferred from Source.

If all Revisions in the request match the current state of the Documents then
the response will contain an empty JSON object:

    **Request**

    .. code-block:: http

        POST /target/_revs_diff HTTP/1.1
        Accept: application/json
        Content-Length: 160
        Content-Type: application/json
        Host: localhost:5984
        User-Agent: CouchDB

        {
            "foo": [
                "3-6a540f3d701ac518d3b9733d673c5484"
            ],
            "bar": [
                "1-967a00dff5e02add41819138abb3284d"
            ]
        }

    **Response**:

    .. code-block:: http

        HTTP/1.1 200 OK
        Cache-Control: must-revalidate
        Content-Length: 2
        Content-Type: application/json
        Date: Fri, 25 Oct 2013 14:45:00 GMT
        Server: CouchDB (Erlang/OTP)

        {}

Replication Completed
^^^^^^^^^^^^^^^^^^^^^

When there are no more changes left to process and no more Documents left to
replicate, the Replicator finishes the Replication process. If Replication
wasn't Continuous, the Replicator MAY return a response to client with
statistics about the process.

    .. code-block:: http

        HTTP/1.1 200 OK
        Cache-Control: must-revalidate
        Content-Length: 414
        Content-Type: application/json
        Date: Fri, 09 May 2014 15:14:19 GMT
        Server: CouchDB (Erlang OTP)

        {
            "history": [
                {
                    "doc_write_failures": 2,
                    "docs_read": 2,
                    "docs_written": 0,
                    "end_last_seq": 2939,
                    "end_time": "Fri, 09 May 2014 15:14:19 GMT",
                    "missing_checked": 1835,
                    "missing_found": 2,
                    "recorded_seq": 2939,
                    "session_id": "05918159f64842f1fe73e9e2157b2112",
                    "start_last_seq": 0,
                    "start_time": "Fri, 09 May 2014 15:14:18 GMT"
                }
            ],
            "ok": true,
            "replication_id_version": 3,
            "session_id": "05918159f64842f1fe73e9e2157b2112",
            "source_last_seq": 2939
        }

Replicate Changes
-----------------

.. code-block:: text

    + - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - +
    ' Locate Changed Documents:                                                       '
    '                                                                                 '
    '               +-------------------------------------+                           '
    '               |      Any Differences Found?         |                           '
    '               +-------------------------------------+                           '
    '                                                   |                             '
    '                                                   |                             '
    '                                                   |                             '
    + - - - - - - - - - - - - - - - - - - - - - - - - - | - - - - - - - - - - - - - - +
                                                        |
    + - - - - - - - - - - - - - - - - - - - - - - - - - | - - - - - - - - - - - - - - +
    ' Replicate Changes:                                |                             '
    '                                                   v                             '
    '               +-------------------------------------+                           '
    '   +---------> |     Fetch Next Changed Document     | <---------------------+   '
    '   |           +-------------------------------------+                       |   '
    '   |           |          GET /source/docid          |                       |   '
    '   |           +-------------------------------------+                       |   '
    '   |             |                                                           |   '
    '   |             |                                                           |   '
    '   |             |                                          201 Created      |   '
    '   |             | 200 OK                                   401 Unauthorized |   '
    '   |             |                                          403 Forbidden    |   '
    '   |             |                                                           |   '
    '   |             v                                                           |   '
    '   |           +-------------------------------------+                       |   '
    '   |   +------ |  Document Has Changed Attachments?  |                       |   '
    '   |   |       +-------------------------------------+                       |   '
    '   |   |         |                                                           |   '
    '   |   |         |                                                           |   '
    '   |   |         | Yes                                                       |   '
    '   |   |         |                                                           |   '
    '   |   |         v                                                           |   '
    '   |   |       +------------------------+   Yes    +---------------------------+ '
    '   |   | No    |  Are They Big Enough?  | -------> | Update Document on Target | '
    '   |   |       +------------------------+          +---------------------------+ '
    '   |   |         |                                 |     PUT /target/docid     | '
    '   |   |         |                                 +---------------------------+ '
    '   |   |         |                                                               '
    '   |   |         | No                                                            '
    '   |   |         |                                                               '
    '   |   |         v                                                               '
    '   |   |       +-------------------------------------+                           '
    '   |   +-----> |     Put Document Into the Stack     |                           '
    '   |           +-------------------------------------+                           '
    '   |             |                                                               '
    '   |             |                                                               '
    '   |             v                                                               '
    '   |     No    +-------------------------------------+                           '
    '   +---------- |           Stack is Full?            |                           '
    '   |           +-------------------------------------+                           '
    '   |             |                                                               '
    '   |             | Yes                                                           '
    '   |             |                                                               '
    '   |             v                                                               '
    '   |           +-------------------------------------+                           '
    '   |           | Upload Stack of Documents to Target |                           '
    '   |           +-------------------------------------+                           '
    '   |           |       POST /target/_bulk_docs       |                           '
    '   |           +-------------------------------------+                           '
    '   |             |                                                               '
    '   |             | 201 Created                                                   '
    '   |             v                                                               '
    '   |           +-------------------------------------+                           '
    '   |           |          Ensure in Commit           |                           '
    '   |           +-------------------------------------+                           '
    '   |           |  POST /target/_ensure_full_commit   |                           '
    '   |           +-------------------------------------+                           '
    '   |             |                                                               '
    '   |             | 201 Created                                                   '
    '   |             v                                                               '
    '   |           +-------------------------------------+                           '
    '   |           |    Record Replication Checkpoint    |                           '
    '   |           +-------------------------------------+                           '
    '   |           |  PUT /source/_local/replication-id  |                           '
    '   |           |  PUT /target/_local/replication-id  |                           '
    '   |           +-------------------------------------+                           '
    '   |             |                                                               '
    '   |             | 201 Created                                                   '
    '   |             v                                                               '
    '   |     No    +-------------------------------------+                           '
    '   +---------- | All Documents from Batch Processed? |                           '
    '               +-------------------------------------+                           '
    '                                                   |                             '
    '                                               Yes |                             '
    '                                                   |                             '
    + - - - - - - - - - - - - - - - - - - - - - - - - - | - - - - - - - - - - - - - - +
                                                        |
    + - - - - - - - - - - - - - - - - - - - - - - - - - | - - - - - - - - - - - - - - +
    ' Locate Changed Documents:                         |                             '
    '                                                   v                             '
    '               +-------------------------------------+                           '
    '               |       Listen to Changes Feed        |                           '
    '               +-------------------------------------+                           '
    '                                                                                 '
    + - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - +

Fetch Changed Documents
^^^^^^^^^^^^^^^^^^^^^^^

At this step the Replicator MUST fetch all Document Leaf Revisions from Source
that are missed at Target. This operation is effective if Replication WILL
use previously calculated Revision differences since they define
missing Documents and their Revisions.

To fetch the Document the Replicator will make a :get:`/{db}/{docid}` request
with the following query parameters:

- ``revs=true``: Instructs the Source to include the list of all known revisions
  into the Document in the ``_revisions`` field. This information is needed to
  synchronize the Document's ancestors history between Source and Target

- The ``open_revs`` query parameter contains a JSON array with a list of
  Leaf Revisions that are needed to be fetched. If the specified Revision
  exists then the Document MUST be returned for this Revision. Otherwise,
  Source MUST return an object with the single field ``missing`` with the
  missed Revision as the value. In case the Document contains attachments,
  Source MUST return information only for those ones that had been changed
  (added or updated) since the specified Revision values. If an attachment
  was deleted, the Document MUST NOT have stub information for it

- ``latest=true``: Ensures, that Source will return the latest Document Revision
  regardless of which one was specified in the ``open_revs`` query parameter.
  This parameter solves a race condition problem where the requested Document
  may be changed in between this step and handling related events on the
  Changes Feed

In the response Source SHOULD return :mimetype:`multipart/mixed` or respond
instead with :mimetype:`application/json` unless the :header:`Accept` header
specifies a different mime type. The :mimetype:`multipart/mixed` content type
allows handling the response data as a stream, since there could be multiple
documents (one per each Leaf Revision) plus several attachments. These
attachments are mostly binary and JSON has no way to handle such data except as
base64 encoded strings which are very ineffective for transfer and processing
operations.

With a :mimetype:`multipart/mixed` response the Replicator handles multiple
Document Leaf Revisions and their attachments one by one as raw data without
any additional encoding applied. There is also one agreement to make data
processing more effective: the Document ALWAYS goes before its attachments, so
the Replicator has no need to process all the data to map related
Documents-Attachments and may handle it as stream with lesser memory footprint.

    **Request**:

    .. code-block:: http

        GET /source/SpaghettiWithMeatballs?revs=true&open_revs=[%225-00ecbbc%22,%221-917fa23%22,%223-6bcedf1%22]&latest=true HTTP/1.1
        Accept: multipart/mixed
        Host: localhost:5984
        User-Agent: CouchDB

    **Response**:

    .. code-block:: http

        HTTP/1.1 200 OK
        Content-Type: multipart/mixed; boundary="7b1596fc4940bc1be725ad67f11ec1c4"
        Date: Thu, 07 Nov 2013 15:10:16 GMT
        Server: CouchDB (Erlang OTP)
        Transfer-Encoding: chunked

        --7b1596fc4940bc1be725ad67f11ec1c4
        Content-Type: application/json

        {
            "_id": "SpaghettiWithMeatballs",
            "_rev": "1-917fa23",
            "_revisions": {
                "ids": [
                    "917fa23"
                ],
                "start": 1
            },
            "description": "An Italian-American delicious dish",
            "ingredients": [
                "spaghetti",
                "tomato sauce",
                "meatballs"
            ],
            "name": "Spaghetti with meatballs"
        }
        --7b1596fc4940bc1be725ad67f11ec1c4
        Content-Type: multipart/related; boundary="a81a77b0ca68389dda3243a43ca946f2"

        --a81a77b0ca68389dda3243a43ca946f2
        Content-Type: application/json

        {
            "_attachments": {
              "recipe.txt": {
                  "content_type": "text/plain",
                  "digest": "md5-R5CrCb6fX10Y46AqtNn0oQ==",
                  "follows": true,
                  "length": 87,
                  "revpos": 7
              }
            },
            "_id": "SpaghettiWithMeatballs",
            "_rev": "7-474f12e",
            "_revisions": {
                "ids": [
                    "474f12e",
                    "5949cfc",
                    "00ecbbc",
                    "fc997b6",
                    "3552c87",
                    "404838b",
                    "5defd9d",
                    "dc1e4be"
                ],
                "start": 7
            },
            "description": "An Italian-American delicious dish",
            "ingredients": [
                "spaghetti",
                "tomato sauce",
                "meatballs",
                "love"
            ],
            "name": "Spaghetti with meatballs"
        }
        --a81a77b0ca68389dda3243a43ca946f2
        Content-Disposition: attachment; filename="recipe.txt"
        Content-Type: text/plain
        Content-Length: 87

        1. Cook spaghetti
        2. Cook meetballs
        3. Mix them
        4. Add tomato sauce
        5. ...
        6. PROFIT!

        --a81a77b0ca68389dda3243a43ca946f2--
        --7b1596fc4940bc1be725ad67f11ec1c4
        Content-Type: application/json; error="true"

        {"missing":"3-6bcedf1"}
        --7b1596fc4940bc1be725ad67f11ec1c4--

After receiving the response, the Replicator puts all the received data into a
local stack for further bulk upload to utilize network bandwidth effectively.
The local stack size could be limited by number of Documents or bytes of
handled JSON data. When the stack is full the Replicator uploads all the
handled Document in bulk mode to the Target. While bulk operations are highly
RECOMMENDED to be used, in certain cases the Replicator MAY upload Documents to
Target one by one.

.. note::
    Alternative Replicator implementations MAY use alternative ways to retrieve
    Documents from Source. For instance, `PouchDB`_ doesn't use the Multipart
    API
    and fetches only the latest Document Revision with inline attachments as a
    single
    JSON object. While this is still valid CouchDB HTTP API usage, such
    solutions MAY require a different API implementation for non-CouchDB
    Peers.

.. _PouchDB: https://github.com/pouchdb/pouchdb/blob/master/packages/node_modules/pouchdb-replication/src/replicate.js

Upload Batch of Changed Documents
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

To upload multiple Documents in a single shot the Replicator sends a
:post:`/{db}/_bulk_docs` request to Target with payload containing a JSON object
with the following mandatory fields:

- **docs** (*array* of *objects*): List of Document objects to update on Target.
  These Documents MUST contain the ``_revisions`` field that holds a list of the
  full Revision history to let Target create Leaf Revisions that correctly
  preserve ancestry
- **new_edits** (*boolean*): Special flag that instructs Target to store
  Documents with the specified Revision (field ``_rev``) value as-is without
  generating a new revision. Always ``false``

The request also MAY contain :header:`X-Couch-Full-Commit` that used to control
CouchDB <3.0 behavior when delayed commits were enabled. Other Peers MAY ignore
this header or use it to control similar local feature.

    **Request**:

    .. code-block:: http

        POST /target/_bulk_docs HTTP/1.1
        Accept: application/json
        Content-Length: 826
        Content-Type:application/json
        Host: localhost:5984
        User-Agent: CouchDB
        X-Couch-Full-Commit: false

        {
            "docs": [
                {
                    "_id": "SpaghettiWithMeatballs",
                    "_rev": "1-917fa2381192822767f010b95b45325b",
                    "_revisions": {
                        "ids": [
                            "917fa2381192822767f010b95b45325b"
                        ],
                        "start": 1
                    },
                    "description": "An Italian-American delicious dish",
                    "ingredients": [
                        "spaghetti",
                        "tomato sauce",
                        "meatballs"
                    ],
                    "name": "Spaghetti with meatballs"
                },
                {
                    "_id": "LambStew",
                    "_rev": "1-34c318924a8f327223eed702ddfdc66d",
                    "_revisions": {
                        "ids": [
                            "34c318924a8f327223eed702ddfdc66d"
                        ],
                        "start": 1
                    },
                    "servings": 6,
                    "subtitle": "Delicious with scone topping",
                    "title": "Lamb Stew"
                },
                {
                    "_id": "FishStew",
                    "_rev": "1-9c65296036141e575d32ba9c034dd3ee",
                    "_revisions": {
                        "ids": [
                            "9c65296036141e575d32ba9c034dd3ee"
                        ],
                        "start": 1
                    },
                    "servings": 4,
                    "subtitle": "Delicious with fresh bread",
                    "title": "Fish Stew"
                }
            ],
            "new_edits": false
        }

In its response Target MUST return a JSON array with a list of Document update
statuses. If the Document has been stored successfully, the list item MUST
contain the field ``ok`` with ``true`` value. Otherwise it MUST contain
``error`` and ``reason`` fields with error type and a human-friendly reason
description.

Document updating failure isn't fatal as Target MAY reject the update for its
own reasons. It's RECOMMENDED to use error type ``forbidden`` for rejections,
but other error types can also be used (like invalid field name etc.). The
Replicator SHOULD NOT retry uploading rejected documents unless there are
good reasons for doing so (e.g. there is special error type for that).

Note that while a update may fail for one Document in the response,
Target can still return a :statuscode:`201` response. Same will be true if all
updates fail for all uploaded Documents.

    **Response**:

    .. code-block:: http

        HTTP/1.1 201 Created
        Cache-Control: must-revalidate
        Content-Length: 246
        Content-Type: application/json
        Date: Sun, 10 Nov 2013 19:02:26 GMT
        Server: CouchDB (Erlang/OTP)

        [
            {
                "ok": true,
                "id": "SpaghettiWithMeatballs",
                "rev":" 1-917fa2381192822767f010b95b45325b"
            },
            {
                "ok": true,
                "id": "FishStew",
                "rev": "1-9c65296036141e575d32ba9c034dd3ee"
            },
            {
                "error": "forbidden",
                "id": "LambStew",
                "reason": "sorry",
                "rev": "1-34c318924a8f327223eed702ddfdc66d"
            }
        ]

Upload Document with Attachments
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

There is a special optimization case when then Replicator WILL NOT use bulk
upload of changed Documents. This case is applied when Documents contain a
lot of attached files or the files are too big to be efficiently encoded with
Base64.

For this case the Replicator issues a :put:`/{db}/{docid}?new_edits=false
</{db}/{docid}>` request with :mimetype:`multipart/related` content type. Such
a request allows one to easily stream the Document and all its attachments
one by one without any serialization overhead.

    **Request**:

    .. code-block:: http

        PUT /target/SpaghettiWithMeatballs?new_edits=false HTTP/1.1
        Accept: application/json
        Content-Length: 1030
        Content-Type: multipart/related; boundary="864d690aeb91f25d469dec6851fb57f2"
        Host: localhost:5984
        User-Agent: CouchDB

        --2fa48cba80d0cdba7829931fe8acce9d
        Content-Type: application/json

        {
            "_attachments": {
                "recipe.txt": {
                    "content_type": "text/plain",
                    "digest": "md5-R5CrCb6fX10Y46AqtNn0oQ==",
                    "follows": true,
                    "length": 87,
                    "revpos": 7
                }
            },
            "_id": "SpaghettiWithMeatballs",
            "_rev": "7-474f12eb068c717243487a9505f6123b",
            "_revisions": {
                "ids": [
                    "474f12eb068c717243487a9505f6123b",
                    "5949cfcd437e3ee22d2d98a26d1a83bf",
                    "00ecbbc54e2a171156ec345b77dfdf59",
                    "fc997b62794a6268f2636a4a176efcd6",
                    "3552c87351aadc1e4bea2461a1e8113a",
                    "404838bc2862ce76c6ebed046f9eb542",
                    "5defd9d813628cea6e98196eb0ee8594"
                ],
                "start": 7
            },
            "description": "An Italian-American delicious dish",
            "ingredients": [
                "spaghetti",
                "tomato sauce",
                "meatballs",
                "love"
            ],
            "name": "Spaghetti with meatballs"
        }
        --2fa48cba80d0cdba7829931fe8acce9d
        Content-Disposition: attachment; filename="recipe.txt"
        Content-Type: text/plain
        Content-Length: 87

        1. Cook spaghetti
        2. Cook meetballs
        3. Mix them
        4. Add tomato sauce
        5. ...
        6. PROFIT!

        --2fa48cba80d0cdba7829931fe8acce9d--

    **Response**:

    .. code-block:: http

        HTTP/1.1 201 Created
        Cache-Control: must-revalidate
        Content-Length: 105
        Content-Type: application/json
        Date: Fri, 08 Nov 2013 16:35:27 GMT
        Server: CouchDB (Erlang/OTP)

        {
            "ok": true,
            "id": "SpaghettiWithMeatballs",
            "rev": "7-474f12eb068c717243487a9505f6123b"
        }

Unlike bulk updating via :post:`/{db}/_bulk_docs` endpoint, the response MAY
come with a different status code. For instance, in the case when the Document
is rejected, Target SHOULD respond with a :statuscode:`403`:

    **Response**:

    .. code-block:: http

        HTTP/1.1 403 Forbidden
        Cache-Control: must-revalidate
        Content-Length: 39
        Content-Type: application/json
        Date: Fri, 08 Nov 2013 16:35:27 GMT
        Server: CouchDB (Erlang/OTP)

        {
            "error": "forbidden",
            "reason": "sorry"
        }

Replicator SHOULD NOT retry requests in case of a :statuscode:`401`,
:statuscode:`403`, :statuscode:`409` or :statuscode:`412` since repeating
the request couldn't solve the issue with user credentials or uploaded data.

Ensure In Commit
^^^^^^^^^^^^^^^^

Once a batch of changes has been successfully uploaded to Target, the
Replicator issues a :post:`/{db}/_ensure_full_commit` request to ensure that
every transferred bit is laid down on disk or other *persistent* storage place.
Target MUST return :statuscode:`201` response with a JSON object containing the
following mandatory fields:

- **instance_start_time** (*string*): Timestamp of when the database was
  opened, expressed in *microseconds* since the epoch
- **ok** (*boolean*): Operation status. Constantly ``true``

  **Request**:

  .. code-block:: http

      POST /target/_ensure_full_commit HTTP/1.1
      Accept: application/json
      Content-Type: application/json
      Host: localhost:5984

  **Response**:

  .. code-block:: http

      HTTP/1.1 201 Created
      Cache-Control: must-revalidate
      Content-Length: 53
      Content-Type: application/json
      Date: Web, 06 Nov 2013 18:20:43 GMT
      Server: CouchDB (Erlang/OTP)

      {
          "instance_start_time": "0",
          "ok": true
      }

Record Replication Checkpoint
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Since batches of changes were uploaded and committed successfully, the
Replicator updates the Replication Log both on Source and Target recording
the current Replication state. This operation is REQUIRED so that in the case
of Replication failure the replication can resume from last point of success,
not from the very beginning.

Replicator updates Replication Log on Source:

    **Request**:

    .. code-block:: http

        PUT /source/_local/afa899a9e59589c3d4ce5668e3218aef HTTP/1.1
        Accept: application/json
        Content-Length: 591
        Content-Type: application/json
        Host: localhost:5984
        User-Agent: CouchDB

        {
            "_id": "_local/afa899a9e59589c3d4ce5668e3218aef",
            "_rev": "0-1",
            "_revisions": {
                "ids": [
                    "31f36e40158e717fbe9842e227b389df"
                ],
                "start": 1
            },
            "history": [
                {
                    "doc_write_failures": 0,
                    "docs_read": 6,
                    "docs_written": 6,
                    "end_last_seq": 26,
                    "end_time": "Thu, 07 Nov 2013 09:42:17 GMT",
                    "missing_checked": 6,
                    "missing_found": 6,
                    "recorded_seq": 26,
                    "session_id": "04bf15bf1d9fa8ac1abc67d0c3e04f07",
                    "start_last_seq": 0,
                    "start_time": "Thu, 07 Nov 2013 09:41:43 GMT"
                }
            ],
            "replication_id_version": 3,
            "session_id": "04bf15bf1d9fa8ac1abc67d0c3e04f07",
            "source_last_seq": 26
        }

    **Response**:

    .. code-block:: http

        HTTP/1.1 201 Created
        Cache-Control: must-revalidate
        Content-Length: 75
        Content-Type: application/json
        Date: Thu, 07 Nov 2013 09:42:17 GMT
        Server: CouchDB (Erlang/OTP)

        {
            "id": "_local/afa899a9e59589c3d4ce5668e3218aef",
            "ok": true,
            "rev": "0-2"
        }

...and on Target too:

    **Request**:

    .. code-block:: http

        PUT /target/_local/afa899a9e59589c3d4ce5668e3218aef HTTP/1.1
        Accept: application/json
        Content-Length: 591
        Content-Type: application/json
        Host: localhost:5984
        User-Agent: CouchDB

        {
            "_id": "_local/afa899a9e59589c3d4ce5668e3218aef",
            "_rev": "1-31f36e40158e717fbe9842e227b389df",
            "_revisions": {
                "ids": [
                    "31f36e40158e717fbe9842e227b389df"
                ],
                "start": 1
            },
            "history": [
                {
                    "doc_write_failures": 0,
                    "docs_read": 6,
                    "docs_written": 6,
                    "end_last_seq": 26,
                    "end_time": "Thu, 07 Nov 2013 09:42:17 GMT",
                    "missing_checked": 6,
                    "missing_found": 6,
                    "recorded_seq": 26,
                    "session_id": "04bf15bf1d9fa8ac1abc67d0c3e04f07",
                    "start_last_seq": 0,
                    "start_time": "Thu, 07 Nov 2013 09:41:43 GMT"
                }
            ],
            "replication_id_version": 3,
            "session_id": "04bf15bf1d9fa8ac1abc67d0c3e04f07",
            "source_last_seq": 26
        }

    **Response**:

    .. code-block:: http

        HTTP/1.1 201 Created
        Cache-Control: must-revalidate
        Content-Length: 106
        Content-Type: application/json
        Date: Thu, 07 Nov 2013 09:42:17 GMT
        Server: CouchDB (Erlang/OTP)

        {
            "id": "_local/afa899a9e59589c3d4ce5668e3218aef",
            "ok": true,
            "rev": "2-9b5d1e36bed6ae08611466e30af1259a"
        }

Continue Reading Changes
------------------------

Once a batch of changes had been processed and transferred to Target
successfully, the Replicator can continue to listen to the Changes Feed for new
changes. If there are no new changes to process the Replication is considered
to be done.

For Continuous Replication, the Replicator MUST continue to wait for new changes
from Source.

Protocol Robustness
===================

Since the `CouchDB Replication Protocol` works on top of HTTP, which is based on
TCP/IP, the Replicator SHOULD expect to be working within an unstable
environment with delays, losses and other bad surprises that might eventually
occur. The Replicator SHOULD NOT count every HTTP request failure as a *fatal
error*. It SHOULD be smart enough to detect timeouts, repeat failed requests,
be ready to process incomplete or malformed data and so on. *Data must flow*
- that's the rule.

Error Responses
===============

In case something goes wrong the Peer MUST respond with a JSON object with
the following REQUIRED fields:

- **error** (*string*): Error type for programs and developers
- **reason** (*string*): Error description for humans

Bad Request
-----------

If a request contains malformed data (like invalid JSON) the Peer MUST respond
with a HTTP :statuscode:`400` and ``bad_request`` as error type:

.. code-block:: javascript

    {
        "error": "bad_request",
        "reason": "invalid json"
    }

Unauthorized
------------

If a Peer REQUIRES credentials be included with the request and the request
does not contain acceptable credentials then the Peer MUST respond with the
HTTP :statuscode:`401` and ``unauthorized`` as error type:

.. code-block:: javascript

    {
        "error": "unauthorized",
        "reason": "Name or password is incorrect"
    }

Forbidden
---------

If a Peer receives valid user credentials, but the requester does not have
sufficient permissions to perform the operation then the Peer
MUST respond with a HTTP :statuscode:`403` and ``forbidden`` as error type:

.. code-block:: javascript

    {
        "error": "forbidden",
        "reason": "You may only update your own user document."
    }

Resource Not Found
------------------

If the requested resource, Database or Document wasn't found on a Peer, the Peer
MUST respond with a HTTP :statuscode:`404` and ``not_found`` as error type:

.. code-block:: javascript

    {
        "error": "not_found",
        "reason": "database \"target\" does not exists"
    }

Method Not Allowed
------------------

If an unsupported method was used then the Peer MUST respond with a
HTTP :statuscode:`405` and ``method_not_allowed`` as error type:

.. code-block:: javascript

    {
        "error": "method_not_allowed",
        "reason": "Only GET, PUT, DELETE allowed"
    }

Resource Conflict
-----------------

A resource conflict error occurs when there are concurrent updates of the same
resource by multiple clients. In this case the Peer MUST respond with a HTTP
:statuscode:`409` and ``conflict`` as error type:

.. code-block:: javascript

    {
        "error": "conflict",
        "reason": "document update conflict"
    }

Precondition Failed
-------------------

The HTTP :statuscode:`412` response may be sent in case of an attempt to
create a Database (error type ``db_exists``) that already exists
or some attachment information is missing (error type ``missing_stub``).
There is no explicit error type restrictions, but it is RECOMMEND to use error
types that are previously mentioned:

.. code-block:: javascript

    {
        "error": "db_exists",
        "reason": "database \"target\" exists"
    }

Server Error
------------

Raised in case an error is *fatal* and the Replicator cannot do anything to
continue Replication. In this case the Replicator MUST return a HTTP
:statuscode:`500` response with an error description (no restrictions on error
type applied):

.. code-block:: javascript

    {
        "error": "worker_died",
        "reason": "kaboom!"
    }

Optimisations
=============

There are RECOMMENDED approaches to optimize the Replication process:

- Keep the number of HTTP requests at a reasonable minimum
- Try to work with a connection pool and make parallel/multiple requests
  whenever possible
- Don't close sockets after each request: respect the keep-alive option
- Use continuous sessions (cookies, etc.) to reduce authentication overhead
- Try to use bulk requests for every operations with Documents
- Find out optimal batch size for Changes feed processing
- Preserve Replication Logs and resume Replication from the last Checkpoint
  whenever possible
- Optimize filter functions: let them run as fast as possible
- Get ready for surprises: networks are very unstable environments

API Reference
=============

Common Methods
--------------

- :head:`/{db}` -- Check Database existence
- :get:`/{db}` -- Retrieve Database information
- :get:`/{db}/_local/{docid}` -- Read the last Checkpoint
- :put:`/{db}/_local/{docid}` -- Save a new Checkpoint

For Target
----------

- :put:`/{db}` -- Create Target if it not exists and the option was provided
- :post:`/{db}/_revs_diff` -- Locate Revisions that are not known to Target
- :post:`/{db}/_bulk_docs` -- Upload Revisions to Target
- :put:`/{db}/{docid}` -- Upload a single Document with attachments to Target
- :post:`/{db}/_ensure_full_commit` -- Ensure that all changes are stored
  on disk

For Source
----------

- :get:`/{db}/_changes` -- Fetch changes since the last pull of Source
- :post:`/{db}/_changes` -- Fetch changes for specified Document IDs since
  the last pull of Source
- :get:`/{db}/{docid}` -- Retrieve a single Document from Source
  with attachments

Reference
=========

* `Refuge RCouch wiki <https://github.com/refuge/rcouch/wiki/Replication-Algorithm>`_
* `CouchBase Lite IOS wiki <https://github.com/couchbase/couchbase-lite-ios/wiki/Replication-Algorithm>`_

.. _ECMA-262: http://www.ecma-international.org/publications/files/ecma-st/ECMA-262.pdf
.. _MVCC: http://en.wikipedia.org/wiki/Multiversion_concurrency_control
.. _CouchDB: http://couchdb.apache.org
.. _Erlang: http://erlang.org
.. _couch_replicator: https://github.com/apache/couchdb/tree/main/src/couch_replicator
.. _change notifications: http://guide.couchdb.org/draft/notifications.html
