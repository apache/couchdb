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

.. _api/server/root:

=====
``/``
=====

.. http:get:: /
    :synopsis: Returns the welcome message and version information

    Accessing the root of a CouchDB instance returns meta information about the
    instance. The response is a JSON structure containing information about the
    server, including a welcome message and the version of the server.

    :<header Accept: - :mimetype:`application/json`
                     - :mimetype:`text/plain`
    :>header Content-Type: - :mimetype:`application/json`
                           - :mimetype:`text/plain; charset=utf-8`
    :code 200: Request completed successfully

    **Request**:

    .. code-block:: http

        GET / HTTP/1.1
        Accept: application/json
        Host: localhost:5984

    **Response**:

    .. code-block:: http

        HTTP/1.1 200 OK
        Cache-Control: must-revalidate
        Content-Length: 179
        Content-Type: application/json
        Date: Sat, 10 Aug 2013 06:33:33 GMT
        Server: CouchDB (Erlang/OTP)

        {
            "couchdb": "Welcome",
            "uuid": "85fb71bf700c17267fef77535820e371",
            "vendor": {
                "name": "The Apache Software Foundation",
                "version": "1.3.1"
            },
            "version": "1.3.1"
        }

.. _api/server/active_tasks:

==================
``/_active_tasks``
==================

.. versionchanged:: 2.1.0 Because of how the scheduling replicator works, continuous replication jobs could be periodically stopped and then started later. When they are not running they will not appear in the ``_active_tasks`` endpoint

.. http:get:: /_active_tasks
    :synopsis: Obtains a list of the tasks running in the server

    List of running tasks, including the task type, name, status
    and process ID. The result is a JSON array of the currently running tasks,
    with each task being described with a single object. Depending on operation
    type set of response object fields might be different.

    :<header Accept: - :mimetype:`application/json`
                     - :mimetype:`text/plain`
    :>header Content-Type: - :mimetype:`application/json`
                           - :mimetype:`text/plain; charset=utf-8`
    :>json number changes_done: Processed changes
    :>json string database: Source database
    :>json string pid: Process ID
    :>json number progress: Current percentage progress
    :>json number started_on: Task start time as unix timestamp
    :>json string status: Task status message
    :>json string task: Task name
    :>json number total_changes: Total changes to process
    :>json string type: Operation Type
    :>json number updated_on: Unix timestamp of last operation update
    :code 200: Request completed successfully
    :code 401: CouchDB Server Administrator privileges required

    **Request**:

    .. code-block:: http

        GET /_active_tasks HTTP/1.1
        Accept: application/json
        Host: localhost:5984

    **Response**:

    .. code-block:: http

        HTTP/1.1 200 OK
        Cache-Control: must-revalidate
        Content-Length: 1690
        Content-Type: application/json
        Date: Sat, 10 Aug 2013 06:37:31 GMT
        Server: CouchDB (Erlang/OTP)

        [
            {
                "changes_done": 64438,
                "database": "mailbox",
                "pid": "<0.12986.1>",
                "progress": 84,
                "started_on": 1376116576,
                "total_changes": 76215,
                "type": "database_compaction",
                "updated_on": 1376116619
            },
            {
                "changes_done": 14443,
                "database": "mailbox",
                "design_document": "c9753817b3ba7c674d92361f24f59b9f",
                "pid": "<0.10461.3>",
                "progress": 18,
                "started_on": 1376116621,
                "total_changes": 76215,
                "type": "indexer",
                "updated_on": 1376116650
            },
            {
                "changes_done": 5454,
                "database": "mailbox",
                "design_document": "_design/meta",
                "pid": "<0.6838.4>",
                "progress": 7,
                "started_on": 1376116632,
                "total_changes": 76215,
                "type": "indexer",
                "updated_on": 1376116651
            },
            {
                "checkpointed_source_seq": 68585,
                "continuous": false,
                "doc_id": null,
                "doc_write_failures": 0,
                "docs_read": 4524,
                "docs_written": 4524,
                "missing_revisions_found": 4524,
                "pid": "<0.1538.5>",
                "progress": 44,
                "replication_id": "9bc1727d74d49d9e157e260bb8bbd1d5",
                "revisions_checked": 4524,
                "source": "mailbox",
                "source_seq": 154419,
                "started_on": 1376116644,
                "target": "http://mailsrv:5984/mailbox",
                "type": "replication",
                "updated_on": 1376116651
            }
        ]

.. _api/server/all_dbs:

=============
``/_all_dbs``
=============

.. http:get:: /_all_dbs
    :synopsis: Returns a list of all the databases

    Returns a list of all the databases in the CouchDB instance.

    :<header Accept: - :mimetype:`application/json`
                     - :mimetype:`text/plain`
    :query boolean descending: Return the databases in descending order by key.
      Default is ``false``.
    :query json endkey: Stop returning databases when the specified key is
      reached.
    :query json end_key: Alias for ``endkey`` param
    :query number limit: Limit the number of the returned databases to the
      specified number.
    :query number skip: Skip this number of databases before starting to return
      the results. Default is ``0``.
    :query json startkey: Return databases starting with the specified key.
    :query json start_key: Alias for ``startkey``.
    :>header Content-Type: - :mimetype:`application/json`
                           - :mimetype:`text/plain; charset=utf-8`
    :code 200: Request completed successfully

    **Request**:

    .. code-block:: http

        GET /_all_dbs HTTP/1.1
        Accept: application/json
        Host: localhost:5984

    **Response**:

    .. code-block:: http

        HTTP/1.1 200 OK
        Cache-Control: must-revalidate
        Content-Length: 52
        Content-Type: application/json
        Date: Sat, 10 Aug 2013 06:57:48 GMT
        Server: CouchDB (Erlang/OTP)

        [
           "_users",
           "contacts",
           "docs",
           "invoices",
           "locations"
        ]

.. _api/server/dbs_info:

==============
``/_dbs_info``
==============

.. versionadded:: 3.2

.. http:get:: /_dbs_info
    :synopsis: Returns all databases information

    Returns a list of all the databases information in the CouchDB instance.

    :<header Accept: - :mimetype:`application/json`
                     - :mimetype:`text/plain`
    :query boolean descending: Return databases information in descending order
      by key. Default is ``false``.
    :query json endkey: Stop returning databases information when the specified
      key is reached.
    :query json end_key: Alias for ``endkey`` param
    :query number limit: Limit the number of the returned databases information
      to the specified number.
    :query number skip: Skip this number of databases before starting to return
      the results. Default is ``0``.
    :query json startkey: Return databases information starting with the
      specified key.
    :query json start_key: Alias for ``startkey``.
    :>header Content-Type: - :mimetype:`application/json`
                           - :mimetype:`text/plain; charset=utf-8`
    :code 200: Request completed successfully

    **Request**:

    .. code-block:: http

        GET /_dbs_info HTTP/1.1
        Accept: application/json
        Host: localhost:5984

    **Response**:

    .. code-block:: http

        HTTP/1.1 200 OK
        Cache-Control: must-revalidate
        Content-Type: application/json
        Date: Thu, 18 Nov 2021 14:37:35 GMT
        Server: CouchDB (Erlang OTP/23)

        [
          {
            "key": "animals",
            "info": {
              "db_name": "animals",
              "update_seq": "52232",
              "sizes": {
                "file": 1178613587,
                "external": 1713103872,
                "active": 1162451555
              },
              "purge_seq": 0,
              "doc_del_count": 0,
              "doc_count": 52224,
              "disk_format_version": 6,
              "compact_running": false,
              "cluster": {
                "q": 8,
                "n": 3,
                "w": 2,
                "r": 2
              },
              "instance_start_time": "0"
            }
          }
        ]

.. versionadded:: 2.2

.. http:post:: /_dbs_info
    :synopsis: Returns information of a list of the specified databases

    Returns information of a list of the specified databases in the CouchDB
    instance. This enables you to request information about multiple databases
    in a single request, in place of multiple :get:`/{db}` requests.

    :<header Accept: - :mimetype:`application/json`
    :>header Content-Type: - :mimetype:`application/json`
    :<json array keys: Array of database names to be requested
    :code 200: Request completed successfully
    :code 400: Missing keys or exceeded keys in request

    **Request**:

    .. code-block:: http

        POST /_dbs_info HTTP/1.1
        Accept: application/json
        Host: localhost:5984
        Content-Type: application/json

        {
            "keys": [
                "animals",
                "plants"
            ]
        }

    **Response**:

    .. code-block:: http

        HTTP/1.1 200 OK
        Cache-Control: must-revalidate
        Content-Type: application/json
        Date: Sat, 20 Dec 2017 06:57:48 GMT
        Server: CouchDB (Erlang/OTP)

        [
          {
            "key": "animals",
            "info": {
              "db_name": "animals",
              "update_seq": "52232",
              "sizes": {
                "file": 1178613587,
                "external": 1713103872,
                "active": 1162451555
              },
              "purge_seq": 0,
              "doc_del_count": 0,
              "doc_count": 52224,
              "disk_format_version": 6,
              "compact_running": false,
              "cluster": {
                "q": 8,
                "n": 3,
                "w": 2,
                "r": 2
              },
              "instance_start_time": "0"
            }
          },
          {
            "key": "plants",
            "info": {
              "db_name": "plants",
              "update_seq": "303",
              "sizes": {
                "file": 3872387,
                "external": 2339,
                "active": 67475
              },
              "purge_seq": 0,
              "doc_del_count": 0,
              "doc_count": 11,
              "disk_format_version": 6,
              "compact_running": false,
              "cluster": {
                "q": 8,
                "n": 3,
                "w": 2,
                "r": 2
              },
              "instance_start_time": "0"
            }
          }
        ]

.. note::
    The supported number of the specified databases in the list can be limited
    by modifying the `max_db_number_for_dbs_info_req` entry in configuration
    file. The default limit is 100. Increasing the limit, while possible, creates
    load on the server so it is advisable to have more requests with 100 dbs,
    rather than a few requests with 1000s of dbs at a time.

.. _api/server/cluster_setup:

===================
``/_cluster_setup``
===================

.. versionadded:: 2.0
.. http:get:: /_cluster_setup
    :synopsis: Return the status of the cluster setup wizard

    Returns the status of the node or cluster, per the cluster setup wizard.

    :<header Accept: - :mimetype:`application/json`
                     - :mimetype:`text/plain`
    :query array ensure_dbs_exist: List of system databases to ensure exist
        on the node/cluster. Defaults to
        ``["_users","_replicator"]``.
    :>header Content-Type: - :mimetype:`application/json`
                           - :mimetype:`text/plain; charset=utf-8`
    :>json string state: Current ``state`` of the node and/or cluster (see
        below)
    :code 200: Request completed successfully

    The ``state`` returned indicates the current node or cluster state, and
    is one of the following:

    - ``cluster_disabled``: The current node is completely unconfigured.
    - ``single_node_disabled``: The current node is configured as a single
      (standalone) node (``[cluster] n=1``), but either does not have a
      server-level admin user defined, or does not have the standard system
      databases created. If the ``ensure_dbs_exist`` query parameter is
      specified, the list of databases provided overrides the default list
      of standard system databases.
    - ``single_node_enabled``: The current node is configured as a single
      (standalone) node, has a server-level admin user defined, and has
      the ``ensure_dbs_exist`` list (explicit or default) of databases
      created.
    - ``cluster_enabled``: The current node has ``[cluster] n`` > 1, is not
      bound to ``127.0.0.1`` and has a server-level admin user defined.
      However, the full set of standard system databases have not been
      created yet. If the ``ensure_dbs_exist`` query parameter is
      specified, the list of databases provided overrides the default list
      of standard system databases.
    - ``cluster_finished``: The current node has ``[cluster] n`` > 1, is not
      bound to ``127.0.0.1``, has a server-level admin user defined *and*
      has the ``ensure_dbs_exist`` list (explicit or default) of databases
      created.

    **Request**:

    .. code-block:: http

        GET /_cluster_setup HTTP/1.1
        Accept: application/json
        Host: localhost:5984

    **Response**:

    .. code-block:: http

        HTTP/1.1 200 OK
        X-CouchDB-Body-Time: 0
        X-Couch-Request-ID: 5c058bdd37
        Server: CouchDB/2.1.0-7f17678 (Erlang OTP/17)
        Date: Sun, 30 Jul 2017 06:33:18 GMT
        Content-Type: application/json
        Content-Length: 29
        Cache-Control: must-revalidate

        {"state":"cluster_enabled"}

.. http:post:: /_cluster_setup
    :synopsis: Sets up a node as a single node or as part of a cluster.

    Configure a node as a single (standalone) node, as part of a cluster,
    or finalise a cluster.

    :<header Accept: - :mimetype:`application/json`
                     - :mimetype:`text/plain`
    :<header Content-Type: :mimetype:`application/json`
    :<json string action: - **enable_single_node**: Configure the current node
                            as a single, standalone CouchDB server.
                          - **enable_cluster**: Configure the local or remote
                            node as one node, preparing it to be joined to a
                            new CouchDB cluster.
                          - **add_node**: Add the specified remote node to
                            this cluster's list of nodes, joining it to the
                            cluster.
                          - **finish_cluster**: Finalise the cluster by
                            creating the standard system databases.
    :<json string bind_address: The IP address to which to bind the current
        node. The special value ``0.0.0.0`` may be specified to bind to all
        interfaces on the host. (enable_cluster and enable_single_node only)
    :<json string username: The username of the server-level administrator to
        create. (enable_cluster and enable_single_node only), or the remote
        server's administrator username (add_node)
    :<json string password: The password for the server-level administrator to
        create. (enable_cluster and enable_single_node only), or the remote
        server's administrator username (add_node)
    :<json number port: The TCP port to which to bind this node
        (enable_cluster and enable_single_node only) or the TCP port to which
        to bind a remote node (add_node only).
    :<json number node_count: The total number of nodes to be joined into
        the cluster, including this one. Used to determine the value of the
        cluster's ``n``, up to a maximum of 3. (enable_cluster only)
    :<json string remote_node: The IP address of the remote node to setup as
        part of this cluster's list of nodes. (enable_cluster only)
    :<json string remote_current_user: The username of the server-level
        administrator authorized on the remote node. (enable_cluster only)
    :<json string remote_current_password: The password of the server-level
        administrator authorized on the remote node. (enable_cluster only)
    :<json string host: The remote node IP of the node to add to the cluster.
        (add_node only)
    :<json array ensure_dbs_exist: List of system databases to ensure exist
        on the node/cluster. Defaults to
        ``["_users","_replicator"]``.

    *No example request/response included here. For a worked example, please
    see* :ref:`cluster/setup/api`.

.. _api/server/db_updates:

================
``/_db_updates``
================

.. versionadded:: 1.4

.. http:get:: /_db_updates
    :synopsis: Return the server changes of databases

    Returns a list of all database events in the CouchDB instance. The
    existence of the ``_global_changes`` database is required to use this
    endpoint.

    :<header Accept: - :mimetype:`application/json`
                     - :mimetype:`text/plain`
    :query string feed: - **normal**: Returns all historical DB changes, then
                          closes the connection. *Default.*
                        - **longpoll**: Closes the connection after the first
                          event.
                        - **continuous**: Send a line of JSON per event.
                          Keeps the socket open until ``timeout``.
                        - **eventsource**: Like, ``continuous``, but sends
                          the events in `EventSource
                          <http://dev.w3.org/html5/eventsource/>`_ format.
    :query number timeout: Number of seconds until CouchDB closes the
      connection. Default is ``60``.
    :query number heartbeat: Period in *milliseconds* after which an empty
        line is sent in the results. Only applicable for ``longpoll``,
        ``continuous``, and ``eventsource`` feeds. Overrides any timeout to
        keep the feed alive indefinitely. Default is ``60000``. May be ``true``
        to use default value.
    :query string since: Return only updates since the specified sequence ID.
        If the sequence ID is specified but does not exist, all changes are returned.
        May be the string ``now`` to begin showing only new updates.
    :>header Content-Type: - :mimetype:`application/json`
                           - :mimetype:`text/plain; charset=utf-8`
    :>header Transfer-Encoding: ``chunked``
    :>json array results: An array of database events. For ``longpoll`` and
        ``continuous`` modes, the entire response is the contents of the
        ``results`` array.
    :>json string last_seq: The last sequence ID reported.
    :code 200: Request completed successfully
    :code 401: CouchDB Server Administrator privileges required

    The ``results`` field of database updates:

    :json string db_name: Database name.
    :json string type: A database event is one of ``created``, ``updated``,
      ``deleted``.
    :json json seq: Update sequence of the event.

    **Request**:

    .. code-block:: http

        GET /_db_updates HTTP/1.1
        Accept: application/json
        Host: localhost:5984

    **Response**:

    .. code-block:: http

        HTTP/1.1 200 OK
        Cache-Control: must-revalidate
        Content-Type: application/json
        Date: Sat, 18 Mar 2017 19:01:35 GMT
        Etag: "C1KU98Y6H0LGM7EQQYL6VSL07"
        Server: CouchDB/2.0.0 (Erlang OTP/17)
        Transfer-Encoding: chunked
        X-Couch-Request-ID: ad87efc7ff
        X-CouchDB-Body-Time: 0

        {
            "results":[
                {"db_name":"mailbox","type":"created","seq":"1-g1AAAAFReJzLYWBg4MhgTmHgzcvPy09JdcjLz8gvLskBCjMlMiTJ____PyuDOZExFyjAnmJhkWaeaIquGIf2JAUgmWQPMiGRAZcaB5CaePxqEkBq6vGqyWMBkgwNQAqobD4h"},
                {"db_name":"mailbox","type":"deleted","seq":"2-g1AAAAFReJzLYWBg4MhgTmHgzcvPy09JdcjLz8gvLskBCjMlMiTJ____PyuDOZEpFyjAnmJhkWaeaIquGIf2JAUgmWQPMiGRAZcaB5CaePxqEkBq6vGqyWMBkgwNQAqobD4hdQsg6vYTUncAou4-IXUPIOpA7ssCAIFHa60"},
            ],
            "last_seq": "2-g1AAAAFReJzLYWBg4MhgTmHgzcvPy09JdcjLz8gvLskBCjMlMiTJ____PyuDOZEpFyjAnmJhkWaeaIquGIf2JAUgmWQPMiGRAZcaB5CaePxqEkBq6vGqyWMBkgwNQAqobD4hdQsg6vYTUncAou4-IXUPIOpA7ssCAIFHa60"
        }

.. _api/server/membership:

================
``/_membership``
================

.. versionadded:: 2.0

.. http:get:: /_membership
    :synopsis: Returns a list of nodes

    Displays the nodes that are part of the cluster as ``cluster_nodes``. The
    field ``all_nodes`` displays all nodes this node knows about, including the
    ones that are part of the cluster. The endpoint is useful when setting up a
    cluster, see :ref:`cluster/nodes`

    :<header Accept: - :mimetype:`application/json`
                     - :mimetype:`text/plain`
    :>header Content-Type: - :mimetype:`application/json`
                           - :mimetype:`text/plain; charset=utf-8`
    :code 200: Request completed successfully

    **Request**:

    .. code-block:: http

        GET /_membership HTTP/1.1
        Accept: application/json
        Host: localhost:5984

    **Response**:

    .. code-block:: http

        HTTP/1.1 200 OK
        Cache-Control: must-revalidate
        Content-Type: application/json
        Date: Sat, 11 Jul 2015 07:02:41 GMT
        Server: CouchDB (Erlang/OTP)
        Content-Length: 142

        {
            "all_nodes": [
                "node1@127.0.0.1",
                "node2@127.0.0.1",
                "node3@127.0.0.1"
            ],
            "cluster_nodes": [
                "node1@127.0.0.1",
                "node2@127.0.0.1",
                "node3@127.0.0.1"
            ]
        }

.. _api/server/replicate:

===============
``/_replicate``
===============

.. http:post:: /_replicate
    :synopsis: Starts or cancels the replication

    Request, configure, or stop, a replication operation.

    :<header Accept: - :mimetype:`application/json`
                     - :mimetype:`text/plain`
    :<header Content-Type: :mimetype:`application/json`
    :<json boolean cancel: Cancels the replication
    :<json boolean continuous: Configure the replication to be continuous
    :<json boolean create_target: Creates the target database.
      Required administrator's privileges on target server.
    :<json object create_target_params: An object that contains parameters
      to be used when creating the target database. Can include the
      standard ``q`` and ``n`` parameters.
    :<json array doc_ids: Array of document IDs to be synchronized.
      ``doc_ids``, ``filter``, and ``selector`` are mutually exclusive.
    :<json string filter: The name of a :ref:`filter function <filterfun>`.
      ``doc_ids``, ``filter``, and ``selector`` are mutually exclusive.
    :<json json selector: A :ref:`selector <find/selectors>` to filter
      documents for synchronization. Has the same behavior as the
      :ref:`selector objects <selectorobj>` in replication documents.
      ``doc_ids``, ``filter``, and ``selector`` are mutually exclusive.
    :<json string source_proxy: Address of a proxy server through which
      replication from the source should occur (protocol can be "http" or
      "socks5")
    :<json string target_proxy: Address of a proxy server through which
      replication to the target should occur (protocol can be "http" or
      "socks5")
    :<json string/object source: Fully qualified source database URL or an
      object which contains the full URL of the source database with additional
      parameters like headers. Eg: 'http://example.com/source_db_name' or
      {"url":"url in here", "headers": {"header1":"value1", ...}} . For
      backwards compatibility, CouchDB 3.x will auto-convert bare database
      names by prepending the address and port CouchDB is listening on, to
      form a complete URL. This behaviour is deprecated in 3.x and will be
      removed in CouchDB 4.0.
    :<json string/object target: Fully qualified target database URL or an
      object which contains the full URL of the target database with additional
      parameters like headers. Eg: 'http://example.com/target_db_name' or
      {"url":"url in here", "headers": {"header1":"value1", ...}} . For
      backwards compatibility, CouchDB 3.x will auto-convert bare database
      names by prepending the address and port CouchDB is listening on, to
      form a complete URL. This behaviour is deprecated in 3.x and will be
      removed in CouchDB 4.0.
    :>header Content-Type: - :mimetype:`application/json`
                           - :mimetype:`text/plain; charset=utf-8`
    :>json array history: Replication history (see below)
    :>json boolean ok: Replication status
    :>json number replication_id_version: Replication protocol version
    :>json string session_id: Unique session ID
    :>json number source_last_seq: Last sequence number read from source
      database
    :code 200: Replication request successfully completed
    :code 202: Continuous replication request has been accepted
    :code 400: Invalid JSON data
    :code 401: CouchDB Server Administrator privileges required
    :code 404: Either the source or target DB is not found or attempt to
      cancel unknown replication task
    :code 500: JSON specification was invalid

    The specification of the replication request is controlled through the
    JSON content of the request. The JSON should be an object with the
    fields defining the source, target and other options.

    The `Replication history` is an array of objects with following structure:

    :json number doc_write_failures: Number of document write failures
    :json number docs_read:  Number of documents read
    :json number docs_written:  Number of documents written to target
    :json number end_last_seq:  Last sequence number in changes stream
    :json string end_time:  Date/Time replication operation completed in
      :rfc:`2822` format
    :json number missing_checked:  Number of missing documents checked
    :json number missing_found:  Number of missing documents found
    :json number recorded_seq:  Last recorded sequence number
    :json string session_id:  Session ID for this replication operation
    :json number start_last_seq:  First sequence number in changes stream
    :json string start_time:  Date/Time replication operation started in
      :rfc:`2822` format

.. note::
    As of CouchDB 2.0.0, fully qualified URLs are required for both the
    replication ``source`` and ``target`` parameters.

    **Request**

    .. code-block:: http

        POST /_replicate HTTP/1.1
        Accept: application/json
        Content-Length: 80
        Content-Type: application/json
        Host: localhost:5984

        {
            "source": "http://127.0.0.1:5984/db_a",
            "target": "http://127.0.0.1:5984/db_b"
        }

    **Response**

    .. code-block:: http

        HTTP/1.1 200 OK
        Cache-Control: must-revalidate
        Content-Length: 692
        Content-Type: application/json
        Date: Sun, 11 Aug 2013 20:38:50 GMT
        Server: CouchDB (Erlang/OTP)

        {
            "history": [
                {
                    "doc_write_failures": 0,
                    "docs_read": 10,
                    "docs_written": 10,
                    "end_last_seq": 28,
                    "end_time": "Sun, 11 Aug 2013 20:38:50 GMT",
                    "missing_checked": 10,
                    "missing_found": 10,
                    "recorded_seq": 28,
                    "session_id": "142a35854a08e205c47174d91b1f9628",
                    "start_last_seq": 1,
                    "start_time": "Sun, 11 Aug 2013 20:38:50 GMT"
                },
                {
                    "doc_write_failures": 0,
                    "docs_read": 1,
                    "docs_written": 1,
                    "end_last_seq": 1,
                    "end_time": "Sat, 10 Aug 2013 15:41:54 GMT",
                    "missing_checked": 1,
                    "missing_found": 1,
                    "recorded_seq": 1,
                    "session_id": "6314f35c51de3ac408af79d6ee0c1a09",
                    "start_last_seq": 0,
                    "start_time": "Sat, 10 Aug 2013 15:41:54 GMT"
                }
            ],
            "ok": true,
            "replication_id_version": 3,
            "session_id": "142a35854a08e205c47174d91b1f9628",
            "source_last_seq": 28
        }

Replication Operation
=====================

The aim of the replication is that at the end of the process, all active
documents on the source database are also in the destination database and all
documents that were deleted in the source databases are also deleted (if they
exist) on the destination database.

Replication can be described as either push or pull replication:

- *Pull replication* is where the ``source`` is the remote CouchDB instance,
  and the ``target`` is the local database.

  Pull replication is the most useful solution to use if your source database
  has a permanent IP address, and your destination (local) database may have a
  dynamically assigned IP address (for example, through DHCP). This is
  particularly important if you are replicating to a mobile or other device
  from a central server.

- *Push replication* is where the ``source`` is a local database, and
  ``target`` is a remote database.

Specifying the Source and Target Database
=========================================

You must use the URL specification of the CouchDB database if you want to
perform replication in either of the following two situations:

- Replication with a remote database (i.e. another instance of CouchDB on the
  same host, or a different host)

- Replication with a database that requires authentication

For example, to request replication between a database local to the CouchDB
instance to which you send the request, and a remote database you might use the
following request:

.. code-block:: http

    POST http://couchdb:5984/_replicate HTTP/1.1
    Content-Type: application/json
    Accept: application/json

    {
        "source" : "recipes",
        "target" : "http://coucdb-remote:5984/recipes",
    }

In all cases, the requested databases in the ``source`` and ``target``
specification must exist. If they do not, an error will be returned within the
JSON object:

.. code-block:: javascript

    {
        "error" : "db_not_found"
        "reason" : "could not open http://couchdb-remote:5984/ol1ka/",
    }

You can create the target database (providing your user credentials allow it)
by adding the ``create_target`` field to the request object:

.. code-block:: http

    POST http://couchdb:5984/_replicate HTTP/1.1
    Content-Type: application/json
    Accept: application/json

    {
        "create_target" : true
        "source" : "recipes",
        "target" : "http://couchdb-remote:5984/recipes",
    }

The ``create_target`` field is not destructive. If the database already
exists, the replication proceeds as normal.

Single Replication
==================

You can request replication of a database so that the two databases can be
synchronized. By default, the replication process occurs one time and
synchronizes the two databases together. For example, you can request a single
synchronization between two databases by supplying the ``source`` and
``target`` fields within the request JSON content.

.. code-block:: http

    POST http://couchdb:5984/_replicate HTTP/1.1
    Accept: application/json
    Content-Type: application/json

    {
        "source" : "recipes",
        "target" : "recipes-snapshot",
    }

In the above example, the databases ``recipes`` and ``recipes-snapshot`` will
be synchronized. These databases are local to the CouchDB instance where the
request was made. The response will be a JSON structure containing the success
(or failure) of the synchronization process, and statistics about the process:

.. code-block:: javascript

    {
        "ok" : true,
        "history" : [
            {
                "docs_read" : 1000,
                "session_id" : "52c2370f5027043d286daca4de247db0",
                "recorded_seq" : 1000,
                "end_last_seq" : 1000,
                "doc_write_failures" : 0,
                "start_time" : "Thu, 28 Oct 2010 10:24:13 GMT",
                "start_last_seq" : 0,
                "end_time" : "Thu, 28 Oct 2010 10:24:14 GMT",
                "missing_checked" : 0,
                "docs_written" : 1000,
                "missing_found" : 1000
            }
        ],
        "session_id" : "52c2370f5027043d286daca4de247db0",
        "source_last_seq" : 1000
    }

Continuous Replication
======================

Synchronization of a database with the previously noted methods happens only
once, at the time the replicate request is made. To have the target database
permanently replicated from the source, you must set the ``continuous`` field
of the JSON object within the request to true.

With continuous replication changes in the source database are replicated to
the target database in perpetuity until you specifically request that
replication ceases.

.. code-block:: http

    POST http://couchdb:5984/_replicate HTTP/1.1
    Accept: application/json
    Content-Type: application/json

    {
        "continuous" : true
        "source" : "recipes",
        "target" : "http://couchdb-remote:5984/recipes",
    }

Changes will be replicated between the two databases as long as a network
connection is available between the two instances.

.. note::
    Two keep two databases synchronized with each other, you need to set
    replication in both directions; that is, you must replicate from ``source``
    to ``target``, and separately from ``target`` to ``source``.

Canceling Continuous Replication
================================

You can cancel continuous replication by adding the ``cancel`` field to the
JSON request object and setting the value to true. Note that the structure of
the request must be identical to the original for the cancellation request to
be honoured. For example, if you requested continuous replication, the
cancellation request must also contain the ``continuous`` field.

For example, the replication request:

.. code-block:: http

    POST http://couchdb:5984/_replicate HTTP/1.1
    Content-Type: application/json
    Accept: application/json

    {
        "source" : "recipes",
        "target" : "http://couchdb-remote:5984/recipes",
        "create_target" : true,
        "continuous" : true
    }

Must be canceled using the request:

.. code-block:: http

    POST http://couchdb:5984/_replicate HTTP/1.1
    Accept: application/json
    Content-Type: application/json

    {
        "cancel" : true,
        "continuous" : true
        "create_target" : true,
        "source" : "recipes",
        "target" : "http://couchdb-remote:5984/recipes",
    }

Requesting cancellation of a replication that does not exist results in a 404
error.

.. _api/server/_scheduler/jobs:

====================
``/_scheduler/jobs``
====================

.. http:get:: /_scheduler/jobs
    :synopsis: Retrieve information about replication jobs

    List of replication jobs. Includes replications created via
    :ref:`api/server/replicate` endpoint as well as those created from
    replication documents. Does not include replications which have completed
    or have failed to start because replication documents were malformed. Each
    job description will include source and target information, replication id,
    a history of recent event, and a few other things.

    :<header Accept: - :mimetype:`application/json`
    :>header Content-Type: - :mimetype:`application/json`
    :query number limit: How many results to return
    :query number skip: How many result to skip starting at the beginning,
                        ordered by replication ID
    :>json number offset: How many results were skipped
    :>json number total_rows: Total number of replication jobs
    :>json string id: Replication ID.
    :>json string database: Replication document database
    :>json string doc_id: Replication document ID
    :>json list history: Timestamped history of events as a list of objects
    :>json string pid: Replication process ID
    :>json string node: Cluster node where the job is running
    :>json string source: Replication source
    :>json string target: Replication target
    :>json string start_time: Timestamp of when the replication was started
    :code 200: Request completed successfully
    :code 401: CouchDB Server Administrator privileges required

    **Request**:

    .. code-block:: http

        GET /_scheduler/jobs HTTP/1.1
        Accept: application/json
        Host: localhost:5984

    **Response**:

    .. code-block:: http

        HTTP/1.1 200 OK
        Cache-Control: must-revalidate
        Content-Length: 1690
        Content-Type: application/json
        Date: Sat, 29 Apr 2017 05:05:16 GMT
        Server: CouchDB (Erlang/OTP)

        {
            "jobs": [
                {
                    "database": "_replicator",
                    "doc_id": "cdyno-0000001-0000003",
                    "history": [
                        {
                            "timestamp": "2017-04-29T05:01:37Z",
                            "type": "started"
                        },
                        {
                            "timestamp": "2017-04-29T05:01:37Z",
                            "type": "added"
                        }
                    ],
                    "id": "8f5b1bd0be6f9166ccfd36fc8be8fc22+continuous",
                    "info": {
                        "changes_pending": 0,
                        "checkpointed_source_seq": "113-g1AAAACTeJzLYWBgYMpgTmHgz8tPSTV0MDQy1zMAQsMckEQiQ1L9____szKYE01ygQLsZsYGqcamiZjKcRqRxwIkGRqA1H-oSbZgk1KMLCzTDE0wdWUBAF6HJIQ",
                        "doc_write_failures": 0,
                        "docs_read": 113,
                        "docs_written": 113,
                        "missing_revisions_found": 113,
                        "revisions_checked": 113,
                        "source_seq": "113-g1AAAACTeJzLYWBgYMpgTmHgz8tPSTV0MDQy1zMAQsMckEQiQ1L9____szKYE01ygQLsZsYGqcamiZjKcRqRxwIkGRqA1H-oSbZgk1KMLCzTDE0wdWUBAF6HJIQ",
                        "through_seq": "113-g1AAAACTeJzLYWBgYMpgTmHgz8tPSTV0MDQy1zMAQsMckEQiQ1L9____szKYE01ygQLsZsYGqcamiZjKcRqRxwIkGRqA1H-oSbZgk1KMLCzTDE0wdWUBAF6HJIQ"
                    },
                    "node": "node1@127.0.0.1",
                    "pid": "<0.1850.0>",
                    "source": "http://myserver.com/foo",
                    "start_time": "2017-04-29T05:01:37Z",
                    "target": "http://adm:*****@localhost:15984/cdyno-0000003/",
                    "user": null
                },
                {
                    "database": "_replicator",
                    "doc_id": "cdyno-0000001-0000002",
                    "history": [
                        {
                            "timestamp": "2017-04-29T05:01:37Z",
                            "type": "started"
                        },
                        {
                            "timestamp": "2017-04-29T05:01:37Z",
                            "type": "added"
                        }
                    ],
                    "id": "e327d79214831ca4c11550b4a453c9ba+continuous",
                    "info": {
                        "changes_pending": null,
                        "checkpointed_source_seq": 0,
                        "doc_write_failures": 0,
                        "docs_read": 12,
                        "docs_written": 12,
                        "missing_revisions_found": 12,
                        "revisions_checked": 12,
                        "source_seq": "12-g1AAAACTeJzLYWBgYMpgTmHgz8tPSTV0MDQy1zMAQsMckEQiQ1L9____szKYE1lzgQLsBsZm5pZJJpjKcRqRxwIkGRqA1H-oSexgk4yMkhITjS0wdWUBADfEJBg",
                        "through_seq": "12-g1AAAACTeJzLYWBgYMpgTmHgz8tPSTV0MDQy1zMAQsMckEQiQ1L9____szKYE1lzgQLsBsZm5pZJJpjKcRqRxwIkGRqA1H-oSexgk4yMkhITjS0wdWUBADfEJBg"
                    },
                    "node": "node2@127.0.0.1",
                    "pid": "<0.1757.0>",
                    "source": "http://myserver.com/foo",
                    "start_time": "2017-04-29T05:01:37Z",
                    "target": "http://adm:*****@localhost:15984/cdyno-0000002/",
                    "user": null
                }
            ],
            "offset": 0,
            "total_rows": 2
         }

.. _api/server/_scheduler/docs:

====================
``/_scheduler/docs``
====================

.. versionchanged:: 2.1.0 Use this endpoint to monitor the state of
                    document-based replications. Previously needed to poll both
                    documents and ``_active_tasks`` to get a complete state
                    summary

.. versionchanged:: 3.0.0 In error states the `"info"` field switched
                    from being a string to being an object

.. http:get:: /_scheduler/docs
    :synopsis: Retrieve information about replication documents from the
               ``_replicator`` database.

    List of replication document states. Includes information about all the
    documents, even in ``completed`` and ``failed`` states. For each document
    it returns the document ID, the database, the replication ID, source and
    target, and other information.

    :<header Accept: - :mimetype:`application/json`
    :>header Content-Type: - :mimetype:`application/json`
    :query number limit: How many results to return
    :query number skip: How many result to skip starting at the beginning, if
                        ordered by document ID
    :>json number offset: How many results were skipped
    :>json number total_rows: Total number of replication documents.
    :>json string id: Replication ID, or ``null`` if state is ``completed`` or
                      ``failed``
    :>json string state: One of following states (see :ref:`replicator/states`
                         for descriptions): ``initializing``, ``running``,
                         ``completed``, ``pending``, ``crashing``, ``error``,
                         ``failed``
    :>json string database: Database where replication document came from
    :>json string doc_id: Replication document ID
    :>json string node: Cluster node where the job is running
    :>json string source: Replication source
    :>json string target: Replication target
    :>json string start_time: Timestamp of when the replication was started
    :>json string last_updated: Timestamp of last state update
    :>json object info: Will contain additional information about the
                        state. For errors, this will be an object with
                        an ``"error"`` field and string value. For
                        success states, see below.
    :>json number error_count: Consecutive errors count. Indicates how many
                               times in a row this replication has crashed.
                               Replication will be retried with an exponential
                               backoff based on this number. As soon as the
                               replication succeeds this count is reset to 0.
                               To can be used to get an idea why a particular
                               replication is not making progress.
    :code 200: Request completed successfully
    :code 401: CouchDB Server Administrator privileges required

    The ``info`` field of a scheduler doc:

    :json number revisions_checked: The count of revisions which have been
        checked since this replication began.
    :json number missing_revisions_found: The count of revisions which were
        found on the source, but missing from the target.
    :json number docs_read: The count of docs which have been read from the
        source.
    :json number docs_written: The count of docs which have been written to the
        target.
    :json number changes_pending: The count of changes not yet replicated.
    :json number doc_write_failures: The count of docs which failed to be
        written to the target.
    :json object checkpointed_source_seq: The source sequence id which was last
        successfully replicated.

    **Request**:

    .. code-block:: http

        GET /_scheduler/docs HTTP/1.1
        Accept: application/json
        Host: localhost:5984

    **Response**:

    .. code-block:: http

        HTTP/1.1 200 OK
        Content-Type: application/json
        Date: Sat, 29 Apr 2017 05:10:08 GMT
        Server: Server: CouchDB (Erlang/OTP)
        Transfer-Encoding: chunked

        {
            "docs": [
                {
                    "database": "_replicator",
                    "doc_id": "cdyno-0000001-0000002",
                    "error_count": 0,
                    "id": "e327d79214831ca4c11550b4a453c9ba+continuous",
                    "info": {
                        "changes_pending": 15,
                        "checkpointed_source_seq": "60-g1AAAACTeJzLYWBgYMpgTmHgz8tPSTV0MDQy1zMAQsMckEQiQ1L9____szKYEyVygQLsBsZm5pZJJpjKcRqRxwIkGRqA1H-oSSpgk4yMkhITjS0wdWUBAENCJEg",
                        "doc_write_failures": 0,
                        "docs_read": 67,
                        "docs_written": 67,
                        "missing_revisions_found": 67,
                        "revisions_checked": 67,
                        "source_seq": "67-g1AAAACTeJzLYWBgYMpgTmHgz8tPSTV0MDQy1zMAQsMckEQiQ1L9____szKYE2VygQLsBsZm5pZJJpjKcRqRxwIkGRqA1H-oSepgk4yMkhITjS0wdWUBAEVKJE8",
                        "through_seq": "67-g1AAAACTeJzLYWBgYMpgTmHgz8tPSTV0MDQy1zMAQsMckEQiQ1L9____szKYE2VygQLsBsZm5pZJJpjKcRqRxwIkGRqA1H-oSepgk4yMkhITjS0wdWUBAEVKJE8"
                    },
                    "last_updated": "2017-04-29T05:01:37Z",
                    "node": "node2@127.0.0.1",
                    "source_proxy": null,
                    "target_proxy": null,
                    "source": "http://myserver.com/foo",
                    "start_time": "2017-04-29T05:01:37Z",
                    "state": "running",
                    "target": "http://adm:*****@localhost:15984/cdyno-0000002/"
                },
                {
                    "database": "_replicator",
                    "doc_id": "cdyno-0000001-0000003",
                    "error_count": 0,
                    "id": "8f5b1bd0be6f9166ccfd36fc8be8fc22+continuous",
                    "info": {
                        "changes_pending": null,
                        "checkpointed_source_seq": 0,
                        "doc_write_failures": 0,
                        "docs_read": 12,
                        "docs_written": 12,
                        "missing_revisions_found": 12,
                        "revisions_checked": 12,
                        "source_seq": "12-g1AAAACTeJzLYWBgYMpgTmHgz8tPSTV0MDQy1zMAQsMckEQiQ1L9____szKYE1lzgQLsBsZm5pZJJpjKcRqRxwIkGRqA1H-oSexgk4yMkhITjS0wdWUBADfEJBg",
                        "through_seq": "12-g1AAAACTeJzLYWBgYMpgTmHgz8tPSTV0MDQy1zMAQsMckEQiQ1L9____szKYE1lzgQLsBsZm5pZJJpjKcRqRxwIkGRqA1H-oSexgk4yMkhITjS0wdWUBADfEJBg"
                    },
                    "last_updated": "2017-04-29T05:01:37Z",
                    "node": "node1@127.0.0.1",
                    "source_proxy": null,
                    "target_proxy": null,
                    "source": "http://myserver.com/foo",
                    "start_time": "2017-04-29T05:01:37Z",
                    "state": "running",
                    "target": "http://adm:*****@localhost:15984/cdyno-0000003/"
                }
            ],
            "offset": 0,
            "total_rows": 2
        }

.. http:get:: /_scheduler/docs/{replicator_db}
    :synopsis: Retrieve information about replication documents from a specific
               replicator database.

    Get information about replication documents from a replicator database.
    The default replicator database is ``_replicator`` but other replicator
    databases can exist if their name ends with the suffix ``/_replicator``.

    .. note:: As a convenience slashes (``/``) in replicator db names do not
       have to be escaped. So ``/_scheduler/docs/other/_replicator`` is valid
       and equivalent to ``/_scheduler/docs/other%2f_replicator``

    :<header Accept: - :mimetype:`application/json`
    :>header Content-Type: - :mimetype:`application/json`
    :query number limit: How many results to return
    :query number skip: How many result to skip starting at the beginning, if
                        ordered by document ID
    :>json number offset: How many results were skipped
    :>json number total_rows: Total number of replication documents.
    :>json string id: Replication ID, or ``null`` if state is ``completed`` or
                      ``failed``
    :>json string state: One of following states (see :ref:`replicator/states`
                         for descriptions): ``initializing``, ``running``,
                         ``completed``, ``pending``, ``crashing``, ``error``,
                         ``failed``
    :>json string database: Database where replication document came from
    :>json string doc_id: Replication document ID
    :>json string node: Cluster node where the job is running
    :>json string source: Replication source
    :>json string target: Replication target
    :>json string start_time: Timestamp of when the replication was started
    :>json string last_update: Timestamp of last state update
    :>json object info: Will contain additional information about the
                        state. For errors, this will be an object with
                        an ``"error"`` field and string value. For
                        success states, see below.
    :>json number error_count: Consecutive errors count. Indicates how many
                               times in a row this replication has crashed.
                               Replication will be retried with an exponential
                               backoff based on this number. As soon as the
                               replication succeeds this count is reset to 0.
                               To can be used to get an idea why a particular
                               replication is not making progress.
    :code 200: Request completed successfully
    :code 401: CouchDB Server Administrator privileges required

    The ``info`` field of a scheduler doc:

    :json number revisions_checked: The count of revisions which have been
        checked since this replication began.
    :json number missing_revisions_found: The count of revisions which were
        found on the source, but missing from the target.
    :json number docs_read: The count of docs which have been read from the
        source.
    :json number docs_written: The count of docs which have been written to the
        target.
    :json number changes_pending: The count of changes not yet replicated.
    :json number doc_write_failures: The count of docs which failed to be
        written to the target.
    :json object checkpointed_source_seq: The source sequence id which was last
        successfully replicated.

    **Request**:

    .. code-block:: http

        GET /_scheduler/docs/other/_replicator HTTP/1.1
        Accept: application/json
        Host: localhost:5984

    **Response**:

    .. code-block:: http

        HTTP/1.1 200 OK
        Content-Type: application/json
        Date: Sat, 29 Apr 2017 05:10:08 GMT
        Server: Server: CouchDB (Erlang/OTP)
        Transfer-Encoding: chunked

        {
            "docs": [
                {
                    "database": "other/_replicator",
                    "doc_id": "cdyno-0000001-0000002",
                    "error_count": 0,
                    "id": "e327d79214831ca4c11550b4a453c9ba+continuous",
                    "info": {
                        "changes_pending": 0,
                        "checkpointed_source_seq": "60-g1AAAACTeJzLYWBgYMpgTmHgz8tPSTV0MDQy1zMAQsMckEQiQ1L9____szKYEyVygQLsBsZm5pZJJpjKcRqRxwIkGRqA1H-oSSpgk4yMkhITjS0wdWUBAENCJEg",
                        "doc_write_failures": 0,
                        "docs_read": 67,
                        "docs_written": 67,
                        "missing_revisions_found": 67,
                        "revisions_checked": 67,
                        "source_seq": "67-g1AAAACTeJzLYWBgYMpgTmHgz8tPSTV0MDQy1zMAQsMckEQiQ1L9____szKYE2VygQLsBsZm5pZJJpjKcRqRxwIkGRqA1H-oSepgk4yMkhITjS0wdWUBAEVKJE8",
                        "through_seq": "67-g1AAAACTeJzLYWBgYMpgTmHgz8tPSTV0MDQy1zMAQsMckEQiQ1L9____szKYE2VygQLsBsZm5pZJJpjKcRqRxwIkGRqA1H-oSepgk4yMkhITjS0wdWUBAEVKJE8"
                    },
                    "last_updated": "2017-04-29T05:01:37Z",
                    "node": "node2@127.0.0.1",
                    "source_proxy": null,
                    "target_proxy": null,
                    "source": "http://myserver.com/foo",
                    "start_time": "2017-04-29T05:01:37Z",
                    "state": "running",
                    "target": "http://adm:*****@localhost:15984/cdyno-0000002/"
                }
            ],
            "offset": 0,
            "total_rows": 1
        }

.. http:get:: /_scheduler/docs/{replicator_db}/{docid}
    :synopsis: Retrieve information about a particular replication document

    .. note:: As a convenience slashes (``/``) in replicator db names do not
       have to be escaped. So ``/_scheduler/docs/other/_replicator`` is valid
       and equivalent to ``/_scheduler/docs/other%2f_replicator``

    :<header Accept: - :mimetype:`application/json`
    :>header Content-Type: - :mimetype:`application/json`
    :>json string id: Replication ID, or ``null`` if state is ``completed`` or
                      ``failed``
    :>json string state: One of following states (see :ref:`replicator/states`
                         for descriptions): ``initializing``, ``running``,
                         ``completed``, ``pending``, ``crashing``, ``error``,
                         ``failed``
    :>json string database: Database where replication document came from
    :>json string doc_id: Replication document ID
    :>json string node: Cluster node where the job is running
    :>json string source: Replication source
    :>json string target: Replication target
    :>json string start_time: Timestamp of when the replication was started
    :>json string last_update: Timestamp of last state update
    :>json object info: Will contain additional information about the
                        state. For errors, this will be an object with
                        an ``"error"`` field and string value. For
                        success states, see below.
    :>json number error_count: Consecutive errors count. Indicates how many
                               times in a row this replication has crashed.
                               Replication will be retried with an exponential
                               backoff based on this number. As soon as the
                               replication succeeds this count is reset to 0.
                               To can be used to get an idea why a particular
                               replication is not making progress.
    :code 200: Request completed successfully
    :code 401: CouchDB Server Administrator privileges required

    The ``info`` field of a scheduler doc:

    :json number revisions_checked: The count of revisions which have been
        checked since this replication began.
    :json number missing_revisions_found: The count of revisions which were
        found on the source, but missing from the target.
    :json number docs_read: The count of docs which have been read from the
        source.
    :json number docs_written: The count of docs which have been written to the
        target.
    :json number changes_pending: The count of changes not yet replicated.
    :json number doc_write_failures: The count of docs which failed to be
        written to the target.
    :json object checkpointed_source_seq: The source sequence id which was last
        successfully replicated.

     **Request**:

    .. code-block:: http

        GET /_scheduler/docs/other/_replicator/cdyno-0000001-0000002 HTTP/1.1
        Accept: application/json
        Host: localhost:5984

    **Response**:

    .. code-block:: http

        HTTP/1.1 200 OK
        Content-Type: application/json
        Date: Sat, 29 Apr 2017 05:10:08 GMT
        Server: Server: CouchDB (Erlang/OTP)
        Transfer-Encoding: chunked

        {
            "database": "other/_replicator",
            "doc_id": "cdyno-0000001-0000002",
            "error_count": 0,
            "id": "e327d79214831ca4c11550b4a453c9ba+continuous",
            "info": {
                "changes_pending": 0,
                "checkpointed_source_seq": "60-g1AAAACTeJzLYWBgYMpgTmHgz8tPSTV0MDQy1zMAQsMckEQiQ1L9____szKYEyVygQLsBsZm5pZJJpjKcRqRxwIkGRqA1H-oSSpgk4yMkhITjS0wdWUBAENCJEg",
                "doc_write_failures": 0,
                "docs_read": 67,
                "docs_written": 67,
                "missing_revisions_found": 67,
                "revisions_checked": 67,
                "source_seq": "67-g1AAAACTeJzLYWBgYMpgTmHgz8tPSTV0MDQy1zMAQsMckEQiQ1L9____szKYE2VygQLsBsZm5pZJJpjKcRqRxwIkGRqA1H-oSepgk4yMkhITjS0wdWUBAEVKJE8",
                "through_seq": "67-g1AAAACTeJzLYWBgYMpgTmHgz8tPSTV0MDQy1zMAQsMckEQiQ1L9____szKYE2VygQLsBsZm5pZJJpjKcRqRxwIkGRqA1H-oSepgk4yMkhITjS0wdWUBAEVKJE8"
            },
            "last_updated": "2017-04-29T05:01:37Z",
            "node": "node2@127.0.0.1",
            "source_proxy": null,
            "target_proxy": null,
            "source": "http://myserver.com/foo",
            "start_time": "2017-04-29T05:01:37Z",
            "state": "running",
            "target": "http://adm:*****@localhost:15984/cdyno-0000002/"
        }

.. _api/server/name:

======================
``/_node/{node-name}``
======================

.. http:get:: /_node/{node-name}
    :synopsis: Returns node name

    The ``/_node/{node-name}`` endpoint can be used to confirm the Erlang
    node name of the server that processes the request. This is most useful
    when accessing ``/_node/_local`` to retrieve this information. Repeatedly
    retrieving this information for a CouchDB endpoint can be useful to determine
    if a CouchDB cluster is correctly proxied through a reverse load balancer.

    :<header Accept: - :mimetype:`application/json`
                     - :mimetype:`text/plain`
    :>header Content-Type: - :mimetype:`application/json`
                           - :mimetype:`text/plain; charset=utf-8`
    :code 200: Request completed successfully

    **Request**:

    .. code-block:: http

        GET /_node/_local HTTP/1.1
        Accept: application/json
        Host: localhost:5984

    **Response**:

    .. code-block:: http

      HTTP/1.1 200 OK
      Cache-Control: must-revalidate
      Content-Length: 27
      Content-Type: application/json
      Date: Tue, 28 Jan 2020 19:25:51 GMT
      Server: CouchDB (Erlang OTP)
      X-Couch-Request-ID: 5b8db6c677
      X-CouchDB-Body-Time: 0

      {"name":"node1@127.0.0.1"}

.. _api/server/stats:

=============================
``/_node/{node-name}/_stats``
=============================

.. http:get:: /_node/{node-name}/_stats
    :synopsis: Returns server statistics

    The ``_stats`` resource returns a JSON object containing the statistics
    for the running server. The object is structured with top-level sections
    collating the statistics for a range of entries, with each individual
    statistic being easily identified, and the content of each statistic is
    self-describing.

    Statistics are sampled internally on a :ref:`configurable interval
    <config/stats>`. When monitoring the ``_stats`` endpoint, you need to use
    a polling frequency of at least twice this to observe accurate results.
    For example, if the :ref:`interval <config/stats>` is 10 seconds,
    poll ``_stats`` at least every 5 seconds.

    The literal string ``_local`` serves as an alias for the local node name, so
    for all stats URLs, ``{node-name}`` may be replaced with ``_local``, to
    interact with the local node's statistics.

    :<header Accept: - :mimetype:`application/json`
                     - :mimetype:`text/plain`
    :>header Content-Type: - :mimetype:`application/json`
                           - :mimetype:`text/plain; charset=utf-8`
    :code 200: Request completed successfully

    **Request**:

    .. code-block:: http

        GET /_node/_local/_stats/couchdb/request_time HTTP/1.1
        Accept: application/json
        Host: localhost:5984

    **Response**:

    .. code-block:: http

        HTTP/1.1 200 OK
        Cache-Control: must-revalidate
        Content-Length: 187
        Content-Type: application/json
        Date: Sat, 10 Aug 2013 11:41:11 GMT
        Server: CouchDB (Erlang/OTP)

        {
          "value": {
            "min": 0,
            "max": 0,
            "arithmetic_mean": 0,
            "geometric_mean": 0,
            "harmonic_mean": 0,
            "median": 0,
            "variance": 0,
            "standard_deviation": 0,
            "skewness": 0,
            "kurtosis": 0,
            "percentile": [
              [
                50,
                0
              ],
              [
                75,
                0
              ],
              [
                90,
                0
              ],
              [
                95,
                0
              ],
              [
                99,
                0
              ],
              [
                999,
                0
              ]
            ],
            "histogram": [
              [
                0,
                0
              ]
            ],
            "n": 0
          },
          "type": "histogram",
          "desc": "length of a request inside CouchDB without MochiWeb"
        }

The fields provide the current, minimum and maximum, and a collection of
statistical means and quantities. The quantity in each case is not defined, but
the descriptions below provide sufficient detail to determine units.

Statistics are reported by 'group'.  The statistics are divided into the
following top-level sections:

- ``couch_log``: Logging subsystem
- ``couch_replicator``: Replication scheduler and subsystem
- ``couchdb``: Primary CouchDB database operations
- ``fabric``: Cluster-related operations
- ``global_changes``: Global changes feed
- ``mem3``: Node membership-related statistics
- ``pread``: CouchDB file-related exceptions
- ``rexi``: Cluster internal RPC-related statistics

The type of the statistic is included in the ``type`` field, and is one of
the following:

- ``counter``: Monotonically increasing counter, resets on restart
- ``histogram``: Binned set of values with meaningful subdivisions.
  Scoped to the current :ref:`collection interval <config/stats>`.
- ``gauge``: Single numerical value that can go up and down

You can also access individual statistics by quoting the statistics sections
and statistic ID as part of the URL path. For example, to get the
``request_time`` statistics within the ``couchdb`` section for the target
node, you can use:

.. code-block:: http

    GET /_node/_local/_stats/couchdb/request_time HTTP/1.1

This returns an entire statistics object, as with the full request, but
containing only the requested individual statistic.

.. _api/server/prometheus:

==================================
``/_node/{node-name}/_prometheus``
==================================

.. http:get:: /_node/{node-name}/_prometheus
    :synopsis: Returns server statistics in prometheus format

    The ``_prometheus`` resource returns a text/plain response that consolidates our
    :ref:`api/server/stats`, and :ref:`api/server/system` endpoints. The format is
    determined by `Prometheus <https://prometheus.io/docs/introduction/overview/>`_.
    The format version is 2.0.

    **Request**:

    .. code-block:: http

        GET /_node/_local/_prometheus HTTP/1.1
        Accept: text/plain
        Host: localhost:5984

    **Response**:

    .. code-block:: http

        HTTP/1.1 200 OK
        Cache-Control: must-revalidate
        Content-Length: 187
        Content-Type: text/plain; version=2.0
        Date: Sat, 10 May 2020 11:41:11 GMT
        Server: CouchDB (Erlang/OTP)

        # TYPE couchdb_couch_log_requests_total counter
        couchdb_couch_log_requests_total{level="alert"} 0
        couchdb_couch_log_requests_total{level="critical"} 0
        couchdb_couch_log_requests_total{level="debug"} 0
        couchdb_couch_log_requests_total{level="emergency"} 0
        couchdb_couch_log_requests_total{level="error"} 0
        couchdb_couch_log_requests_total{level="info"} 8
        couchdb_couch_log_requests_total{level="notice"} 51
        couchdb_couch_log_requests_total{level="warning"} 0
        # TYPE couchdb_couch_replicator_changes_manager_deaths_total counter
        couchdb_couch_replicator_changes_manager_deaths_total 0
        # TYPE couchdb_couch_replicator_changes_queue_deaths_total counter
        couchdb_couch_replicator_changes_queue_deaths_total 0
        # TYPE couchdb_couch_replicator_changes_read_failures_total counter
        couchdb_couch_replicator_changes_read_failures_total 0
        # TYPE couchdb_couch_replicator_changes_reader_deaths_total counter
        couchdb_couch_replicator_changes_reader_deaths_total 0
        # TYPE couchdb_couch_replicator_checkpoints_failure_total counter
        couchdb_couch_replicator_checkpoints_failure_total 0
        # TYPE couchdb_couch_replicator_checkpoints_total counter
        couchdb_couch_replicator_checkpoints_total 0
        # TYPE couchdb_couch_replicator_cluster_is_stable gauge
        couchdb_couch_replicator_cluster_is_stable 1
        # TYPE couchdb_couch_replicator_connection_acquires_total counter
        couchdb_couch_replicator_connection_acquires_total 0
        # TYPE couchdb_couch_replicator_connection_closes_total counter
        couchdb_couch_replicator_connection_closes_total 0
        # TYPE couchdb_couch_replicator_connection_creates_total counter
        couchdb_couch_replicator_connection_creates_total 0
        # TYPE couchdb_couch_replicator_connection_owner_crashes_total counter
        couchdb_couch_replicator_connection_owner_crashes_total 0
        # TYPE couchdb_couch_replicator_connection_releases_total counter
        couchdb_couch_replicator_connection_releases_total 0
        # TYPE couchdb_couch_replicator_connection_worker_crashes_total counter
        couchdb_couch_replicator_connection_worker_crashes_total 0
        # TYPE couchdb_couch_replicator_db_scans_total counter
        couchdb_couch_replicator_db_scans_total 1
        # TYPE couchdb_couch_replicator_docs_completed_state_updates_total counter
        couchdb_couch_replicator_docs_completed_state_updates_total 0
        # TYPE couchdb_couch_replicator_docs_db_changes_total counter
        couchdb_couch_replicator_docs_db_changes_total 0
        # TYPE couchdb_couch_replicator_docs_dbs_created_total counter
        couchdb_couch_replicator_docs_dbs_created_total 0
        # TYPE couchdb_couch_replicator_docs_dbs_deleted_total counter
        couchdb_couch_replicator_docs_dbs_deleted_total 0
        # TYPE couchdb_couch_replicator_docs_dbs_found_total counter
        couchdb_couch_replicator_docs_dbs_found_total 2
        # TYPE couchdb_couch_replicator_docs_failed_state_updates_total counter
        couchdb_couch_replicator_docs_failed_state_updates_total 0
        # TYPE couchdb_couch_replicator_failed_starts_total counter
        couchdb_couch_replicator_failed_starts_total 0
        # TYPE couchdb_couch_replicator_jobs_adds_total counter
        couchdb_couch_replicator_jobs_adds_total 0
        # TYPE couchdb_couch_replicator_jobs_crashed gauge
        couchdb_couch_replicator_jobs_crashed 0
        # TYPE couchdb_couch_replicator_jobs_crashes_total counter
        couchdb_couch_replicator_jobs_crashes_total 0
        # TYPE couchdb_couch_replicator_jobs_duplicate_adds_total counter
        couchdb_couch_replicator_jobs_duplicate_adds_total 0
        # TYPE couchdb_couch_replicator_jobs_pending gauge
        couchdb_couch_replicator_jobs_pending 0
        # TYPE couchdb_couch_replicator_jobs_removes_total counter
        couchdb_couch_replicator_jobs_removes_total 0
        # TYPE couchdb_couch_replicator_jobs_running gauge
        couchdb_couch_replicator_jobs_running 0
        # TYPE couchdb_couch_replicator_jobs_starts_total counter
        couchdb_couch_replicator_jobs_starts_total 0
        # TYPE couchdb_couch_replicator_jobs_stops_total counter
        couchdb_couch_replicator_jobs_stops_total 0
        # TYPE couchdb_couch_replicator_jobs_total gauge
        couchdb_couch_replicator_jobs_total 0
        # TYPE couchdb_couch_replicator_requests_total counter
        couchdb_couch_replicator_requests_total 0
        # TYPE couchdb_couch_replicator_responses_failure_total counter
        couchdb_couch_replicator_responses_failure_total 0
        # TYPE couchdb_couch_replicator_responses_total counter
        couchdb_couch_replicator_responses_total 0
        # TYPE couchdb_couch_replicator_stream_responses_failure_total counter
        couchdb_couch_replicator_stream_responses_failure_total 0
        # TYPE couchdb_couch_replicator_stream_responses_total counter
        couchdb_couch_replicator_stream_responses_total 0
        # TYPE couchdb_couch_replicator_worker_deaths_total counter
        couchdb_couch_replicator_worker_deaths_total 0
        # TYPE couchdb_couch_replicator_workers_started_total counter
        couchdb_couch_replicator_workers_started_total 0
        # TYPE couchdb_auth_cache_requests_total counter
        couchdb_auth_cache_requests_total 0
        # TYPE couchdb_auth_cache_misses_total counter
        couchdb_auth_cache_misses_total 0
        # TYPE couchdb_collect_results_time_seconds summary
        couchdb_collect_results_time_seconds{quantile="0.5"} 0.0
        couchdb_collect_results_time_seconds{quantile="0.75"} 0.0
        couchdb_collect_results_time_seconds{quantile="0.9"} 0.0
        couchdb_collect_results_time_seconds{quantile="0.95"} 0.0
        couchdb_collect_results_time_seconds{quantile="0.99"} 0.0
        couchdb_collect_results_time_seconds{quantile="0.999"} 0.0
        couchdb_collect_results_time_seconds_sum 0.0
        couchdb_collect_results_time_seconds_count 0
        # TYPE couchdb_couch_server_lru_skip_total counter
        couchdb_couch_server_lru_skip_total 0
        # TYPE couchdb_database_purges_total counter
        couchdb_database_purges_total 0
        # TYPE couchdb_database_reads_total counter
        couchdb_database_reads_total 0
        # TYPE couchdb_database_writes_total counter
        couchdb_database_writes_total 0
        # TYPE couchdb_db_open_time_seconds summary
        couchdb_db_open_time_seconds{quantile="0.5"} 0.0
        couchdb_db_open_time_seconds{quantile="0.75"} 0.0
        couchdb_db_open_time_seconds{quantile="0.9"} 0.0
        couchdb_db_open_time_seconds{quantile="0.95"} 0.0
        couchdb_db_open_time_seconds{quantile="0.99"} 0.0
        couchdb_db_open_time_seconds{quantile="0.999"} 0.0
        couchdb_db_open_time_seconds_sum 0.0
        couchdb_db_open_time_seconds_count 0
        # TYPE couchdb_dbinfo_seconds summary
        couchdb_dbinfo_seconds{quantile="0.5"} 0.0
        couchdb_dbinfo_seconds{quantile="0.75"} 0.0
        couchdb_dbinfo_seconds{quantile="0.9"} 0.0
        couchdb_dbinfo_seconds{quantile="0.95"} 0.0
        couchdb_dbinfo_seconds{quantile="0.99"} 0.0
        couchdb_dbinfo_seconds{quantile="0.999"} 0.0
        couchdb_dbinfo_seconds_sum 0.0
        couchdb_dbinfo_seconds_count 0
        # TYPE couchdb_document_inserts_total counter
        couchdb_document_inserts_total 0
        # TYPE couchdb_document_purges_failure_total counter
        couchdb_document_purges_failure_total 0
        # TYPE couchdb_document_purges_success_total counter
        couchdb_document_purges_success_total 0
        # TYPE couchdb_document_purges_total_total counter
        couchdb_document_purges_total_total 0
        # TYPE couchdb_document_writes_total counter
        couchdb_document_writes_total 0
        # TYPE couchdb_httpd_aborted_requests_total counter
        couchdb_httpd_aborted_requests_total 0
        # TYPE couchdb_httpd_all_docs_timeouts_total counter
        couchdb_httpd_all_docs_timeouts_total 0
        # TYPE couchdb_httpd_bulk_docs_seconds summary
        couchdb_httpd_bulk_docs_seconds{quantile="0.5"} 0.0
        couchdb_httpd_bulk_docs_seconds{quantile="0.75"} 0.0
        couchdb_httpd_bulk_docs_seconds{quantile="0.9"} 0.0
        couchdb_httpd_bulk_docs_seconds{quantile="0.95"} 0.0
        couchdb_httpd_bulk_docs_seconds{quantile="0.99"} 0.0
        couchdb_httpd_bulk_docs_seconds{quantile="0.999"} 0.0
        couchdb_httpd_bulk_docs_seconds_sum 0.0
        couchdb_httpd_bulk_docs_seconds_count 0
        ...remaining couchdb metrics from _stats and _system

If an additional port config option is specified, then a client can call this API using
that port which does not require authentication. This option is ``false``(OFF) by default.
When the option ``true``(ON), the default ports for a 3 node cluster are ``17986``,
``27986``, ``37986``.
See :ref:`Configuration of Prometheus Endpoint <config/prometheus>` for details.

.. code-block:: http

        GET /_node/_local/_prometheus HTTP/1.1
        Accept: text/plain
        Host: localhost:17986

.. _api/server/system:

==============================
``/_node/{node-name}/_system``
==============================

.. http:get:: /_node/{node-name}/_system
    :synopsis: Returns system-level server statistics

    The ``_system`` resource returns a JSON object containing various
    system-level statistics for the running server. The object is structured
    with top-level sections collating the statistics for a range of entries,
    with each individual statistic being easily identified, and the content of
    each statistic is self-describing.

    The literal string ``_local`` serves as an alias for the local node name, so
    for all stats URLs, ``{node-name}`` may be replaced with ``_local``, to
    interact with the local node's statistics.

    :<header Accept: - :mimetype:`application/json`
                     - :mimetype:`text/plain`
    :>header Content-Type: - :mimetype:`application/json`
                           - :mimetype:`text/plain; charset=utf-8`
    :code 200: Request completed successfully

    **Request**:

    .. code-block:: http

        GET /_node/_local/_system HTTP/1.1
        Accept: application/json
        Host: localhost:5984

    **Response**:

    .. code-block:: http

        HTTP/1.1 200 OK
        Cache-Control: must-revalidate
        Content-Length: 187
        Content-Type: application/json
        Date: Sat, 10 Aug 2013 11:41:11 GMT
        Server: CouchDB (Erlang/OTP)

        {
          "uptime": 259,
          "memory": {
          ...
        }

    These statistics are generally intended for CouchDB developers only.

.. _api/server/restart:

===============================
``/_node/{node-name}/_restart``
===============================

.. http:post:: /_node/{node-name}/_restart
    :synopsis: Restarts CouchDB application on a given node

    This API is to facilitate integration testing only
    it is not meant to be used in production

    :code 200: Request completed successfully

.. _api/server/search_analyze:

==========================================
``/_search_analyze``
==========================================

.. warning::
    Search endpoints require a running search plugin connected to each cluster
    node. See :ref:`Search Plugin Installation <install/search>` for details.

.. versionadded:: 3.0

.. http:post:: /_search_analyze
    :synopsis: Tests the results of analyzer tokenization

    Tests the results of Lucene analyzer tokenization on sample text.

    :param field: Type of analyzer
    :param text:  Analyzer token you want to test
    :code 200: Request completed successfully
    :code 400: Request body is wrong (malformed or missing one of the mandatory fields)
    :code 500: A server error (or other kind of error) occurred

**Request**:

.. code-block:: http

    POST /_search_analyze HTTP/1.1
    Host: localhost:5984
    Content-Type: application/json

    {"analyzer":"english", "text":"running"}

**Response**:

.. code-block:: javascript

    {
        "tokens": [
            "run"
        ]
    }

.. _api/server/utils:

===========
``/_utils``
===========

.. http:get:: /_utils
    :synopsis: Redirects to /_utils/

    Accesses the built-in Fauxton administration interface for CouchDB.

    :>header Location: New URI location
    :code 301: Redirects to :get:`/_utils/`

.. http:get:: /_utils/
    :synopsis: CouchDB administration interface (Fauxton)

    :>header Content-Type: :mimetype:`text/html`
    :>header Last-Modified: Static files modification timestamp
    :code 200: Request completed successfully

.. _api/server/up:

========
``/_up``
========

.. versionadded:: 2.0

.. http:get:: /_up
    :synopsis: Health check endpoint

    Confirms that the server is up, running, and ready to respond to requests.
    If :config:option:`maintenance_mode <couchdb/maintenance_mode>` is
    ``true`` or ``nolb``, the endpoint will return a 404 response.

    :>header Content-Type: :mimetype:`application/json`
    :code 200: Request completed successfully
    :code 404: The server is unavailable for requests at this time.

    **Response**:

    .. code-block:: http

        HTTP/1.1 200 OK
        Cache-Control: must-revalidate
        Content-Length: 16
        Content-Type: application/json
        Date: Sat, 17 Mar 2018 04:46:26 GMT
        Server: CouchDB/2.2.0-f999071ec (Erlang OTP/19)
        X-Couch-Request-ID: c57a3b2787
        X-CouchDB-Body-Time: 0

        {"status":"ok"}

.. _api/server/uuids:

===========
``/_uuids``
===========

.. versionchanged:: 2.0.0

.. http:get:: /_uuids
    :synopsis: Generates a list of UUIDs from the server

    Requests one or more Universally Unique Identifiers (UUIDs) from the
    CouchDB instance. The response is a JSON object providing a list of UUIDs.

    :<header Accept: - :mimetype:`application/json`
                     - :mimetype:`text/plain`
    :query number count: Number of UUIDs to return. Default is ``1``.
    :>header Content-Type: - :mimetype:`application/json`
                           - :mimetype:`text/plain; charset=utf-8`
    :>header ETag: Response hash
    :code 200: Request completed successfully
    :code 400: Requested more UUIDs than is :config:option:`allowed
               <uuids/max_count>` to retrieve

    **Request**:

    .. code-block:: http

        GET /_uuids?count=10 HTTP/1.1
        Accept: application/json
        Host: localhost:5984

    **Response**:

    .. code-block:: http

        HTTP/1.1 200 OK
        Content-Length: 362
        Content-Type: application/json
        Date: Sat, 10 Aug 2013 11:46:25 GMT
        ETag: "DGRWWQFLUDWN5MRKSLKQ425XV"
        Expires: Fri, 01 Jan 1990 00:00:00 GMT
        Pragma: no-cache
        Server: CouchDB (Erlang/OTP)

        {
            "uuids": [
                "75480ca477454894678e22eec6002413",
                "75480ca477454894678e22eec600250b",
                "75480ca477454894678e22eec6002c41",
                "75480ca477454894678e22eec6003b90",
                "75480ca477454894678e22eec6003fca",
                "75480ca477454894678e22eec6004bef",
                "75480ca477454894678e22eec600528f",
                "75480ca477454894678e22eec6005e0b",
                "75480ca477454894678e22eec6006158",
                "75480ca477454894678e22eec6006161"
            ]
        }

The UUID type is determined by the :config:option:`UUID algorithm
<uuids/algorithm>` setting in the CouchDB configuration.

The UUID type may be changed at any time through the
:ref:`Configuration API <api/config/section/key>`. For example, the UUID type
could be changed to ``random`` by sending this HTTP request:

.. code-block:: http

    PUT http://couchdb:5984/_node/nonode@nohost/_config/uuids/algorithm HTTP/1.1
    Content-Type: application/json
    Accept: */*

    "random"

You can verify the change by obtaining a list of UUIDs:

.. code-block:: javascript

    {
        "uuids" : [
            "031aad7b469956cf2826fcb2a9260492",
            "6ec875e15e6b385120938df18ee8e496",
            "cff9e881516483911aa2f0e98949092d",
            "b89d37509d39dd712546f9510d4a9271",
            "2e0dbf7f6c4ad716f21938a016e4e59f"
        ]
    }

.. _api/server/favicon:

================
``/favicon.ico``
================

.. http:get:: /favicon.ico
    :synopsis: Returns the site icon

    Binary content for the `favicon.ico` site icon.

    :>header Content-Type: :mimetype:`image/x-icon`
    :code 200: Request completed successfully
    :code 404: The requested content could not be found

.. _api/server/reshard:

=============
``/_reshard``
=============

.. versionadded:: 2.4

.. http:get:: /_reshard
    :synopsis: Retrieve summary information about resharding on the cluster

    Returns a count of completed, failed, running, stopped, and total jobs
    along with the state of resharding on the cluster.

    :<header Accept: - :mimetype:`application/json`
    :>header Content-Type: - :mimetype:`application/json`

    :>json string state: ``stopped`` or ``running``
    :>json string state_reason: ``null`` or string describing additional
                                information or reason associated with the state
    :>json number completed: Count of completed resharding jobs
    :>json number failed: Count of failed resharding jobs
    :>json number running: Count of running resharding jobs
    :>json number stopped: Count of stopped resharding jobs
    :>json number total: Total count of resharding jobs

    :code 200: Request completed successfully
    :code 401: CouchDB Server Administrator privileges required

    **Request**:

    .. code-block:: http

        GET /_reshard HTTP/1.1
        Accept: application/json
        Host: localhost:5984

    **Response**:

    .. code-block:: http

        HTTP/1.1 200 OK
        Content-Type: application/json

        {
            "completed": 21,
            "failed": 0,
            "running": 3,
            "state": "running",
            "state_reason": null,
            "stopped": 0,
            "total": 24
        }

.. http:get:: /_reshard/state
    :synopsis: Retrieve the state of resharding on the cluster

    Returns the resharding state and optional information about the state.

    :<header Accept: - :mimetype:`application/json`
    :>header Content-Type: - :mimetype:`application/json`

    :>json string state: ``stopped`` or ``running``
    :>json string state_reason: Additional  information  or  reason  associated
                                with the state

    :code 200: Request completed successfully
    :code 401: CouchDB Server Administrator privileges required

    **Request**:

    .. code-block:: http

        GET /_reshard/state HTTP/1.1
        Accept: application/json
        Host: localhost:5984

    **Response**:

    .. code-block:: http

        HTTP/1.1 200 OK
        Content-Type: application/json

        {
            "reason": null,
            "state": "running"
        }

.. http:put:: /_reshard/state
    :synopsis: Change resharding state on the cluster

    Change the resharding state on the cluster. The states are
    ``stopped`` or ``running``. This starts and stops global resharding on all
    the nodes of the cluster. If there are any running jobs, they
    will be stopped when the state changes to ``stopped``. When the state
    changes back to ``running`` those job will continue running.

    :<header Accept: - :mimetype:`application/json`
    :>header Content-Type: - :mimetype:`application/json`

    :<json string state: ``stopped`` or ``running``
    :<json string state_reason: Optional string describing additional
                                information or reason associated with the state

    :>json boolean ok: ``true``

    :code 200: Request completed successfully
    :code 400: Invalid request. Could be a bad or missing state name.
    :code 401: CouchDB Server Administrator privileges required

    **Request**:

    .. code-block:: http

        PUT /_reshard/state HTTP/1.1
        Accept: application/json
        Host: localhost:5984

        {
            "state": "stopped",
            "reason": "Rebalancing in progress"
        }

    **Response**:

    .. code-block:: http

        HTTP/1.1 200 OK
        Content-Type: application/json

        {
            "ok": true
        }

.. http:get:: /_reshard/jobs
    :synopsis: Retrieve information about all the resharding jobs on the cluster

    .. note:: The shape of the response and the ``total_rows`` and ``offset``
              field in particular are meant to be consistent with the
              ``_scheduler/jobs`` endpoint.

    :<header Accept: - :mimetype:`application/json`
    :>header Content-Type: - :mimetype:`application/json`

    :>json list jobs: Array of json objects, one for each resharding job. For
                      the fields of each job see the /_reshard/job/{jobid}
                      endpoint.
    :>json number offset: Offset in the list of jobs object. Currently
                          hard-coded at ``0``.
    :>json number total_rows: Total number of resharding jobs on the cluster.

    :code 200: Request completed successfully
    :code 401: CouchDB Server Administrator privileges required

    **Request**:

    .. code-block:: http

        GET /_reshard/jobs HTTP/1.1
        Accept: application/json

    **Response**:

    .. code-block:: http

        HTTP/1.1 200 OK
        Content-Type: application/json

        {
            "jobs": [
                {
                    "history": [
                        {
                            "detail": null,
                            "timestamp": "2019-03-28T15:28:02Z",
                            "type": "new"
                        },
                        {
                            "detail": "initial_copy",
                            "timestamp": "2019-03-28T15:28:02Z",
                            "type": "running"
                        },
                        ...
                    ],
                    "id": "001-171d1211418996ff47bd610b1d1257fc4ca2628868def4a05e63e8f8fe50694a",
                    "job_state": "completed",
                    "node": "node1@127.0.0.1",
                    "source": "shards/00000000-1fffffff/d1.1553786862",
                    "split_state": "completed",
                    "start_time": "2019-03-28T15:28:02Z",
                    "state_info": {},
                    "target": [
                        "shards/00000000-0fffffff/d1.1553786862",
                        "shards/10000000-1fffffff/d1.1553786862"
                    ],
                    "type": "split",
                    "update_time": "2019-03-28T15:28:08Z"
                },
                ...
            ],
            "offset": 0,
            "total_rows": 24
        }

.. http:get:: /_reshard/jobs/{jobid}
    :synopsis: Retrieve information about a particular resharding job

    Get information about the resharding job identified by ``jobid``.

    :<header Accept: - :mimetype:`application/json`
    :>header Content-Type: - :mimetype:`application/json`

    :>json string id: Job ID.
    :>json string type: Currently only ``split`` is implemented.
    :>json string job_state: The running state of the job. Could be one of
                             ``new``, ``running``, ``stopped``, ``completed``
                             or ``failed``.
    :>json string split_state: State detail specific to shard splitting. It
                               indicates how far has shard splitting
                               progressed, and can be one of ``new``,
                               ``initial_copy``, ``topoff1``,
                               ``build_indices``, ``topoff2``,
                               ``copy_local_docs``, ``update_shardmap``,
                               ``wait_source_close``, ``topoff3``,
                               ``source_delete`` or ``completed``.
    :>json object state_info: Optional additional info associated with the
                              current state.
    :>json string source: For ``split`` jobs this will be the source shard.
    :>json list target: For ``split`` jobs this will be a list of two or more
                        target shards.
    :>json list history: List of json objects recording a job's state
                         transition history.

    :code 200: Request completed successfully
    :code 401: CouchDB Server Administrator privileges required

    **Request**:

    .. code-block:: http

        GET /_reshard/jobs/001-171d1211418996ff47bd610b1d1257fc4ca2628868def4a05e63e8f8fe50694a HTTP/1.1
        Accept: application/json

    **Response**:

    .. code-block:: http

        HTTP/1.1 200 OK
        Content-Type: application/json

        {

            "id": "001-171d1211418996ff47bd610b1d1257fc4ca2628868def4a05e63e8f8fe50694a",
            "job_state": "completed",
            "node": "node1@127.0.0.1",
            "source": "shards/00000000-1fffffff/d1.1553786862",
            "split_state": "completed",
            "start_time": "2019-03-28T15:28:02Z",
            "state_info": {},
            "target": [
                "shards/00000000-0fffffff/d1.1553786862",
                "shards/10000000-1fffffff/d1.1553786862"
            ],
            "type": "split",
            "update_time": "2019-03-28T15:28:08Z",
            "history": [
                {
                    "detail": null,
                    "timestamp": "2019-03-28T15:28:02Z",
                    "type": "new"
                },
                {
                    "detail": "initial_copy",
                    "timestamp": "2019-03-28T15:28:02Z",
                    "type": "running"
                },
                ...
            ]
        }

.. http:post:: /_reshard/jobs
    :synopsis: Create one or more resharding jobs

    Depending on what fields are specified in the request, one or more
    resharding jobs will be created. The response is a json array of results.
    Each result object represents a single resharding job for a particular node
    and range. Some of the responses could be successful and some could fail.
    Successful results will have the ``"ok": true`` key and and value, and
    failed jobs will have the ``"error": "{error_message}"`` key and value.

    :<header Accept: - :mimetype:`application/json`
    :>header Content-Type: - :mimetype:`application/json`

    :<json string type: Type of job. Currently only ``"split"`` is accepted.

    :<json string db: Database to split. This is mutually exclusive with the
                      ``"shard``" field.

    :<json string node: Split shards on a particular node. This is an optional
                        parameter. The value should be one of the nodes
                        returned from the ``_membership`` endpoint.

    :<json string range: Split shards copies in the given range. The range
                         format is ``hhhhhhhh-hhhhhhhh`` where ``h`` is a
                         hexadecimal digit. This format is used since this is
                         how the ranges are represented in the file system.
                         This is parameter is optional and is mutually
                         exclusive with the ``"shard"`` field.

    :<json string shard: Split a particular shard. The shard should be
                         specified as ``"shards/{range}/{db}.{suffix}"``. Where
                         ``range`` has the ``hhhhhhhh-hhhhhhhh`` format, ``db``
                         is the database name, and ``suffix`` is the shard
                         (timestamp) creation suffix.

    :>json boolean ok: ``true`` if job created successfully.

    :<json string error: Error message if a job could be not be created.

    :<json string node: Cluster node where the job was created and is running.

    :code 201: One or more jobs were successfully created
    :code 400: Invalid request. Parameter validation might have failed.
    :code 401: CouchDB Server Administrator privileges required
    :code 404: Db, node, range or shard was not found

    **Request**:

    .. code-block:: http

        POST /_reshard/jobs HTTP/1.1
        Accept: application/json
        Content-Type: application/json

       {
           "db": "db3",
           "range": "80000000-ffffffff",
           "type": "split"
       }

    **Response**:

    .. code-block:: http

        HTTP/1.1 201 Created
        Content-Type: application/json

        [
            {
                "id": "001-30d7848a6feeb826d5e3ea5bb7773d672af226fd34fd84a8fb1ca736285df557",
                "node": "node1@127.0.0.1",
                "ok": true,
                "shard": "shards/80000000-ffffffff/db3.1554148353"
            },
            {
                "id": "001-c2d734360b4cb3ff8b3feaccb2d787bf81ce2e773489eddd985ddd01d9de8e01",
                "node": "node2@127.0.0.1",
                "ok": true,
                "shard": "shards/80000000-ffffffff/db3.1554148353"
            }
        ]

.. http:delete:: /_reshard/jobs/{jobid}
    :synopsis: Remove a resharding job

    If the job is running, stop the job and then remove it.

    :>json boolean ok: ``true`` if the job was removed successfully.

    :code 200: The job was removed successfully
    :code 401: CouchDB Server Administrator privileges required
    :code 404: The job was not found

    **Request**:

    .. code-block:: http

        DELETE /_reshard/jobs/001-171d1211418996ff47bd610b1d1257fc4ca2628868def4a05e63e8f8fe50694a HTTP/1.1

    **Response**:

    .. code-block:: http

        HTTP/1.1 200 OK
        Content-Type: application/json

        {
            "ok": true
        }

.. http:get:: /_reshard/jobs/{jobid}/state
    :synopsis: Retrieve the state of a single resharding job

    Returns the running state of a resharding job identified by ``jobid``.

    :<header Accept: - :mimetype:`application/json`
    :>header Content-Type: - :mimetype:`application/json`

    :<json string state: One of ``new``, ``running``, ``stopped``,
                         ``completed`` or ``failed``.

    :<json string state_reason: Additional information associated with the
                                state

    :code 200: Request completed successfully
    :code 401: CouchDB Server Administrator privileges required
    :code 404: The job was not found

    **Request**:

    .. code-block:: http

        GET /_reshard/jobs/001-b3da04f969bbd682faaab5a6c373705cbcca23f732c386bb1a608cfbcfe9faff/state HTTP/1.1
        Accept: application/json
        Host: localhost:5984

    **Response**:

    .. code-block:: http

        HTTP/1.1 200 OK
        Content-Type: application/json

        {
            "reason": null,
            "state": "running"
        }

.. http:put:: /_reshard/jobs/{jobid}/state
    :synopsis: Change the state of a resharding job

    Change the state of a particular resharding job identified by ``jobid``.
    The state can be changed from ``stopped`` to ``running`` or from
    ``running`` to ``stopped``. If an individual job is ``stopped`` via this
    API it will stay ``stopped`` even after the global resharding state is
    toggled from ``stopped`` to ``running``. If the job is already
    ``completed`` its state will stay ``completed``.

    :<header Accept: - :mimetype:`application/json`
    :>header Content-Type: - :mimetype:`application/json`

    :<json string state: ``stopped`` or ``running``
    :<json string state_reason: Optional string describing additional
                                information or reason associated with the state

    :>json boolean ok: ``true``

    :code 200: Request completed successfully
    :code 400: Invalid request. Could be a bad state name, for example.
    :code 401: CouchDB Server Administrator privileges required
    :code 404: The job was not found

    **Request**:

    .. code-block:: http

        PUT /_reshard/state/001-b3da04f969bbd682faaab5a6c373705cbcca23f732c386bb1a608cfbcfe9faff/state HTTP/1.1
        Accept: application/json
        Host: localhost:5984

        {
            "state": "stopped",
            "reason": "Rebalancing in progress"
        }

    **Response**:

    .. code-block:: http

       HTTP/1.1 200 OK
       Content-Type: application/json

       {
            "ok": true
       }
