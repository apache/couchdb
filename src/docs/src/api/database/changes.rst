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

.. _api/db/changes:

================
``/db/_changes``
================

.. http:get:: /{db}/_changes
    :synopsis: Returns changes for the given database

    Returns a sorted list of changes made to documents in the database, in time
    order of application, can be obtained from the database's ``_changes``
    resource. Only the most recent change for a given document is guaranteed to
    be provided, for example if a document has had fields added, and then
    deleted, an API client checking for changes will not necessarily receive
    the intermediate state of added documents.

    This can be used to listen for update and modifications to the database for
    post processing or synchronization, and for practical purposes,
    a continuously connected ``_changes`` feed is a reasonable approach for
    generating a real-time log for most applications.

    :param db: Database name
    :<header Accept: - :mimetype:`application/json`
                     - :mimetype:`text/event-stream`
                     - :mimetype:`text/plain`
    :<header Last-Event-ID: ID of the last events received by the server on a
        previous connection. Overrides ``since`` query parameter.
    :query array doc_ids: List of document IDs to filter the changes feed as
        valid JSON array. Used with :ref:`_doc_ids <changes/filter/doc_ids>`
        filter. Since `length of URL is limited`_, it is better to use
        :post:`/{db}/_changes` instead.
    :query boolean conflicts: Includes `conflicts` information in response.
        Ignored if `include_docs` isn't ``true``. Default is ``false``.
    :query boolean descending: Return the change results in descending sequence
        order (most recent change first). Default is ``false``.
    :query string feed: - **normal** Specifies :ref:`Normal Polling Mode
                          <changes/normal>`. All past changes are returned
                          immediately. *Default.*
                        - **longpoll** Specifies :ref:`Long Polling Mode
                          <changes/longpoll>`. Waits until at least one change
                          has occurred, sends the change, then closes the
                          connection. Most commonly used in conjunction with
                          ``since=now``, to wait for the next change.
                        - **continuous** Sets :ref:`Continuous Mode
                          <changes/continuous>`. Sends a line of JSON per
                          event. Keeps the socket open until ``timeout``.
                        - **eventsource** Sets :ref:`Event Source Mode
                          <changes/eventsource>`. Works the same as Continuous
                          Mode, but sends the events in `EventSource
                          <http://dev.w3.org/html5/eventsource/>`_ format.
    :query string filter: Reference to a :ref:`filter function <filterfun>`
        from a design document that will filter whole stream emitting only
        filtered events. See the section `Change Notifications in the book
        CouchDB The Definitive Guide`_ for more information.
    :query number heartbeat: Period in *milliseconds* after which an empty
        line is sent in the results. Only applicable for :ref:`longpoll
        <changes/longpoll>`, :ref:`continuous <changes/continuous>`, and
        :ref:`eventsource <changes/eventsource>` feeds. Overrides any timeout
        to keep the feed alive indefinitely. Default is ``60000``. May be
        ``true`` to use default value.
    :query boolean include_docs: Include the associated document with each
        result. If there are conflicts, only the winning revision is returned.
        Default is ``false``.
    :query boolean attachments: Include the Base64-encoded content of
        :ref:`attachments <api/doc/attachments>` in the documents that
        are included if ``include_docs`` is ``true``. Ignored if ``include_docs``
        isn't ``true``. Default is ``false``.
    :query boolean att_encoding_info: Include encoding information in attachment
        stubs if ``include_docs`` is ``true`` and the particular attachment is
        compressed. Ignored if ``include_docs`` isn't ``true``.
        Default is ``false``.
    :query number last-event-id: Alias of `Last-Event-ID` header.
    :query number limit: Limit number of result rows to the specified value
        (note that using ``0`` here has the same effect as ``1``).
    :query since: Start the results from the change immediately after the given
        update sequence. Can be valid update sequence or ``now`` value.
        Default is ``0``.
    :query string style: Specifies how many revisions are returned in
        the changes array. The default, ``main_only``, will only return
        the current "winning" revision; ``all_docs`` will return all leaf
        revisions (including conflicts and deleted former conflicts).
    :query number timeout: Maximum period in *milliseconds* to wait for a change
        before the response is sent, even if there are no results.
        Only applicable for :ref:`longpoll <changes/longpoll>` or
        :ref:`continuous <changes/continuous>` feeds.
        Default value is specified by :config:option:`chttpd/changes_timeout`
        configuration option. Note that ``60000`` value is also the default
        maximum timeout to prevent undetected dead connections.
    :query string view: Allows to use view functions as filters. Documents
        counted as "passed" for view filter in case if map function emits
        at least one record for them.
        See :ref:`changes/filter/view` for more info.
    :query number seq_interval: When fetching changes in a batch, setting the
        *seq_interval* parameter tells CouchDB to only calculate the update seq
        with every Nth result returned. By setting **seq_interval=<batch size>**
        , where ``<batch size>`` is the number of results requested per batch,
        load can be reduced on the source CouchDB database; computing the seq
        value across many shards (esp. in highly-sharded databases) is expensive
        in a heavily loaded CouchDB cluster.
    :>header Cache-Control: ``no-cache`` if changes feed is
        :ref:`eventsource <changes/eventsource>`
    :>header Content-Type: - :mimetype:`application/json`
                           - :mimetype:`text/event-stream`
                           - :mimetype:`text/plain; charset=utf-8`
    :>header ETag: Response hash if changes feed is `normal`
    :>header Transfer-Encoding: ``chunked``
    :>json json last_seq: Last change update sequence
    :>json number pending: Count of remaining items in the feed
    :>json array results: Changes made to a database
    :code 200: Request completed successfully
    :code 400: Bad request

    The ``results`` field of database changes:

    :json array changes: List of document's leaves with single field ``rev``.
    :json string id: Document ID.
    :json json seq: Update sequence.
    :json bool deleted: ``true`` if the document is deleted.

    **Request**:

    .. code-block:: http

        GET /db/_changes?style=all_docs HTTP/1.1
        Accept: application/json
        Host: localhost:5984

    **Response**:

    .. code-block:: http

        HTTP/1.1 200 OK
        Cache-Control: must-revalidate
        Content-Type: application/json
        Date: Mon, 12 Aug 2013 00:54:58 GMT
        ETag: "6ASLEKEMSRABT0O5XY9UPO9Z"
        Server: CouchDB (Erlang/OTP)
        Transfer-Encoding: chunked

        {
            "last_seq": "5-g1AAAAIreJyVkEsKwjAURZ-toI5cgq5A0sQ0OrI70XyppcaRY92J7kR3ojupaSPUUgotgRd4yTlwbw4A0zRUMLdnpaMkwmyF3Ily9xBwEIuiKLI05KOTW0wkV4rruP29UyGWbordzwKVxWBNOGMKZhertDlarbr5pOT3DV4gudUC9-MPJX9tpEAYx4TQASns2E24ucuJ7rXJSL1BbEgf3vTwpmedCZkYa7Pulck7Xt7x_usFU2aIHOD4eEfVTVA5KMGUkqhNZV-8_o5i",
            "pending": 0,
            "results": [
                {
                    "changes": [
                        {
                            "rev": "2-7051cbe5c8faecd085a3fa619e6e6337"
                        }
                    ],
                    "id": "6478c2ae800dfc387396d14e1fc39626",
                    "seq": "3-g1AAAAG3eJzLYWBg4MhgTmHgz8tPSTV0MDQy1zMAQsMcoARTIkOS_P___7MSGXAqSVIAkkn2IFUZzIkMuUAee5pRqnGiuXkKA2dpXkpqWmZeagpu_Q4g_fGEbEkAqaqH2sIItsXAyMjM2NgUUwdOU_JYgCRDA5ACGjQfn30QlQsgKvcjfGaQZmaUmmZClM8gZhyAmHGfsG0PICrBPmQC22ZqbGRqamyIqSsLAAArcXo"
                },
                {
                    "changes": [
                        {
                            "rev": "3-7379b9e515b161226c6559d90c4dc49f"
                        }
                    ],
                    "deleted": true,
                    "id": "5bbc9ca465f1b0fcd62362168a7c8831",
                    "seq": "4-g1AAAAHXeJzLYWBg4MhgTmHgz8tPSTV0MDQy1zMAQsMcoARTIkOS_P___7MymBMZc4EC7MmJKSmJqWaYynEakaQAJJPsoaYwgE1JM0o1TjQ3T2HgLM1LSU3LzEtNwa3fAaQ_HqQ_kQG3qgSQqnoUtxoYGZkZG5uS4NY8FiDJ0ACkgAbNx2cfROUCiMr9CJ8ZpJkZpaaZEOUziBkHIGbcJ2zbA4hKsA-ZwLaZGhuZmhobYurKAgCz33kh"
                },
                {
                    "changes": [
                        {
                            "rev": "6-460637e73a6288cb24d532bf91f32969"
                        },
                        {
                            "rev": "5-eeaa298781f60b7bcae0c91bdedd1b87"
                        }
                    ],
                    "id": "729eb57437745e506b333068fff665ae",
                    "seq": "5-g1AAAAIReJyVkE0OgjAQRkcwUVceQU9g-mOpruQm2tI2SLCuXOtN9CZ6E70JFmpCCCFCmkyTdt6bfJMDwDQNFcztWWkcY8JXyB2cu49AgFwURZGloRid3MMkEUoJHbXbOxVy6arc_SxQWQzRVHCuYHaxSpuj1aqbj0t-3-AlSrZakn78oeSvjRSIkIhSNiCFHbsKN3c50b02mURvEB-yD296eNOzzoRMRLRZ98rkHS_veGcC_nR-fGe1gaCaxihhjOI2lX0BhniHaA"
                }
            ]
        }

.. _length of URL is limited: http://stackoverflow.com/a/417184/965635

.. versionchanged:: 0.11.0 added ``include_docs`` parameter
.. versionchanged:: 1.2.0 added ``view`` parameter and special value `_view`
   for ``filter`` one
.. versionchanged:: 1.3.0 ``since`` parameter could take `now` value to start
   listen changes since current seq number.
.. versionchanged:: 1.3.0 ``eventsource`` feed type added.
.. versionchanged:: 1.4.0 Support ``Last-Event-ID`` header.
.. versionchanged:: 1.6.0 added ``attachments`` and ``att_encoding_info``
   parameters
.. versionchanged:: 2.0.0 update sequences can be any valid json object,
   added ``seq_interval``

.. note::
    If the specified replicas of the shards in any given since value are
    unavailable, alternative replicas are selected, and the last known
    checkpoint between them is used. If this happens, you might see changes
    again that you have previously seen. Therefore, an application making use
    of the ``_changes`` feed should be ‘idempotent’, that is, able to receive the
    same data multiple times, safely.

.. note::
    Cloudant Sync and PouchDB already optimize the replication process by
    setting ``seq_interval`` parameter to the number of results expected per
    batch. This parameter increases throughput by reducing latency between
    sequential requests in bulk document transfers. This has resulted in up to
    a 20% replication performance improvement in highly-sharded databases.

.. warning::
    Using the ``attachments`` parameter to include attachments in the changes
    feed is not recommended for large attachment sizes. Also note that the
    Base64-encoding that is used leads to a 33% overhead (i.e. one third) in
    transfer size for attachments.

.. warning::
    The results returned by `_changes` are partially ordered. In other words,
    the order is not guaranteed to be preserved for multiple calls.

.. http:post:: /{db}/_changes
    :synopsis: Returns changes for the given database for certain document IDs

    Requests the database changes feed in the same way as
    :get:`/{db}/_changes` does, but is widely used with
    ``?filter=_doc_ids`` query parameter and allows one to pass a larger list of
    document IDs to filter.

    **Request**:

    .. code-block:: http

        POST /recipes/_changes?filter=_doc_ids HTTP/1.1
        Accept: application/json
        Content-Length: 40
        Content-Type: application/json
        Host: localhost:5984

        {
            "doc_ids": [
                "SpaghettiWithMeatballs"
            ]
        }

    **Response**:

    .. code-block:: http

        HTTP/1.1 200 OK
        Cache-Control: must-revalidate
        Content-Type: application/json
        Date: Sat, 28 Sep 2013 07:23:09 GMT
        ETag: "ARIHFWL3I7PIS0SPVTFU6TLR2"
        Server: CouchDB (Erlang OTP)
        Transfer-Encoding: chunked

        {
            "last_seq": "5-g1AAAAIreJyVkEsKwjAURZ-toI5cgq5A0sQ0OrI70XyppcaRY92J7kR3ojupaSPUUgotgRd4yTlwbw4A0zRUMLdnpaMkwmyF3Ily9xBwEIuiKLI05KOTW0wkV4rruP29UyGWbordzwKVxWBNOGMKZhertDlarbr5pOT3DV4gudUC9-MPJX9tpEAYx4TQASns2E24ucuJ7rXJSL1BbEgf3vTwpmedCZkYa7Pulck7Xt7x_usFU2aIHOD4eEfVTVA5KMGUkqhNZV8_o5i",
            "pending": 0,
            "results": [
                {
                    "changes": [
                        {
                            "rev": "13-bcb9d6388b60fd1e960d9ec4e8e3f29e"
                        }
                    ],
                    "id": "SpaghettiWithMeatballs",
                    "seq":  "5-g1AAAAIReJyVkE0OgjAQRkcwUVceQU9g-mOpruQm2tI2SLCuXOtN9CZ6E70JFmpCCCFCmkyTdt6bfJMDwDQNFcztWWkcY8JXyB2cu49AgFwURZGloRid3MMkEUoJHbXbOxVy6arc_SxQWQzRVHCuYHaxSpuj1aqbj0t-3-AlSrZakn78oeSvjRSIkIhSNiCFHbsKN3c50b02mURvEB-yD296eNOzzoRMRLRZ98rkHS_veGcC_nR-fGe1gaCaxihhjOI2lX0BhniHaA"
                }
            ]
        }

.. _changes:

Changes Feeds
=============

.. _changes/normal:

Polling
-------

By default all changes are immediately returned within the JSON body::

    GET /somedatabase/_changes HTTP/1.1

.. code-block:: javascript

    {"results":[
    {"seq":"1-g1AAAAF9eJzLYWBg4MhgTmHgz8tPSTV0MDQy1zMAQsMcoARTIkOS_P__7MSGXAqSVIAkkn2IFUZzIkMuUAee5pRqnGiuXkKA2dpXkpqWmZeagpu_Q4g_fGEbEkAqaqH2sIItsXAyMjM2NgUUwdOU_JYgCRDA5ACGjQfn30QlQsgKvcTVnkAovI-YZUPICpBvs0CAN1eY_c","id":"fresh","changes":[{"rev":"1-967a00dff5e02add41819138abb3284d"}]},
    {"seq":"3-g1AAAAG3eJzLYWBg4MhgTmHgz8tPSTV0MDQy1zMAQsMcoARTIkOS_P___7MSGXAqSVIAkkn2IFUZzIkMuUAee5pRqnGiuXkKA2dpXkpqWmZeagpu_Q4g_fGEbEkAqaqH2sIItsXAyMjM2NgUUwdOU_JYgCRDA5ACGjQfn30QlQsgKvcjfGaQZmaUmmZClM8gZhyAmHGfsG0PICrBPmQC22ZqbGRqamyIqSsLAAArcXo","id":"updated","changes":[{"rev":"2-7051cbe5c8faecd085a3fa619e6e6337CFCmkyTdt6bfJMDwDQNFcztWWkcY8JXyB2cu49AgFwURZGloRid3MMkEUoJHbXbOxVy6arc_SxQWQzRVHCuYHaxSpuj1aqbj0t-3-AlSrZakn78oeSvjRSIkIhSNiCFHbsKN3c50b02mURvEB-yD296eNOzzoRMRLRZ98rkHS_veGcC_nR-fGe1gaCaxihhjOI2lX0BhniHaA","id":"deleted","changes":[{"rev":"2-eec205a9d413992850a6e32678485900"}],"deleted":true}
    ],
    "last_seq":"5-g1AAAAIreJyVkEsKwjAURZ-toI5cgq5A0sQ0OrI70XyppcaRY92J7kR3ojupaSPUUgotgRd4yTlwbw4A0zRUMLdnpaMkwmyF3Ily9xBwEIuiKLI05KOTW0wkV4rruP29UyGWbordzwKVxWBNOGMKZhertDlarbr5pOT3DV4gudUC9-MPJX9tpEAYx4TQASns2E24ucuJ7rXJSL1BbEgf3vTwpmedCZkYa7Pulck7Xt7x_usFU2aIHOD4eEfVTVA5KMGUkqhNZV-8_o5i",
    "pending": 0}

``results`` is the list of changes in sequential order. New and changed
documents only differ in the value of the rev; deleted documents include the
``"deleted": true`` attribute. (In the ``style=all_docs mode``, deleted applies
only to the current/winning revision. The other revisions listed might be
deleted even if there is no deleted property; you have to ``GET`` them
individually to make sure.)

``last_seq`` is the update sequence of the last update returned (Equivalent
to the last item in the results).

Sending a ``since`` param in the query string skips all changes up to and
including the given update sequence:

.. code-block:: http

    GET /somedatabase/_changes?since=4-g1AAAAHXeJzLYWBg4MhgTmHgz8tPSTV0MDQy1zMAQsMcoARTIkOS_P___7MymBMZc4EC7MmJKSmJqWaYynEakaQAJJPsoaYwgE1JM0o1TjQ3T2HgLM1LSU3LzEtNwa3fAaQ_HqQ_kQG3qgSQqnoUtxoYGZkZG5uS4NY8FiDJ0ACkgAbNx2cfROUCiMr9CJ8ZpJkZpaaZEOUziBkHIGbcJ2zbA4hKsA-ZwLaZGhuZmhobYurKAgCz33kh HTTP/1.1

The return structure for ``normal`` and ``longpoll`` modes is a JSON
array of changes objects, and the last update sequence.

In the return format for ``continuous`` mode, the server sends a ``CRLF``
(carriage-return, linefeed) delimited line for each change. Each line
contains the `JSON object` described above.

You can also request the full contents of each document change (instead
of just the change notification) by using the ``include_docs`` parameter.

.. code-block:: javascript

    {
        "last_seq": "5-g1AAAAIreJyVkEsKwjAURZ-toI5cgq5A0sQ0OrI70XyppcaRY92J7kR3ojupaSPUUgotgRd4yTlwbw4A0zRUMLdnpaMkwmyF3Ily9xBwEIuiKLI05KOTW0wkV4rruP29UyGWbordzwKVxWBNOGMKZhertDlarbr5pOT3DV4gudUC9-MPJX9tpEAYx4TQASns2E24ucuJ7rXJSL1BbEgf3vTwpmedCZkYa7Pulck7Xt7x_usFU2aIHOD4eEfVTVA5KMGUkqhNZV-8_o5i",
        "pending": 0,
        "results": [
            {
                "changes": [
                    {
                        "rev": "2-eec205a9d413992850a6e32678485900"
                    }
                ],
                "deleted": true,
                "id": "deleted",
                "seq":  "5-g1AAAAIReJyVkE0OgjAQRkcwUVceQU9g-mOpruQm2tI2SLCuXOtN9CZ6E70JFmpCCCFCmkyTdt6bfJMDwDQNFcztWWkcY8JXyB2cu49AgFwURZGloRid3MMkEUoJHbXbOxVy6arc_SxQWQzRVHCuYHaxSpuj1aqbj0t-3-AlSrZakn78oeSvjRSIkIhSNiCFHbsKN3c50b02mURvEByD296eNOzzoRMRLRZ98rkHS_veGcC_nR-fGe1gaCaxihhjOI2lX0BhniHaA",
            }
        ]
    }

.. _changes/longpoll:

Long Polling
------------

The `longpoll` feed, probably most applicable for a browser, is a more
efficient form of polling that waits for a change to occur before the response
is sent. `longpoll` avoids the need to frequently poll CouchDB to discover
nothing has changed!

The request to the server will remain open until a change is made on the
database and is subsequently transferred, and then the connection will close.
This is low load for both server and client.

The response is basically the same JSON as is sent for the `normal` feed.

Because the wait for a change can be significant you can set a
timeout before the connection is automatically closed (the
``timeout`` argument). You can also set a heartbeat interval (using
the ``heartbeat`` query argument), which sends a newline to keep the
connection active.

Keep in mind that ``heartbeat`` means "Send a linefeed every ``x`` ms
if no change arrives, and hold the connection indefinitely" while ``timeout``
means "Hold this connection open for ``x`` ms, and if no change arrives in that
time, close the socket."  ``heartbeat`` overrides ``timeout``.

.. _changes/continuous:

Continuous
----------

Continually polling the CouchDB server is not ideal - setting up new HTTP
connections just to tell the client that nothing happened puts unnecessary
strain on CouchDB.

A continuous feed stays open and connected to the database until explicitly
closed and changes are sent to the client as they happen, i.e. in near
real-time.

As with the `longpoll` feed type you can set both the timeout and heartbeat
intervals to ensure that the connection is kept open for new changes and
updates.

Keep in mind that ``heartbeat`` means "Send a linefeed every ``x`` ms
if no change arrives, and hold the connection indefinitely" while ``timeout``
means "Hold this connection open for ``x`` ms, and if no change arrives in that
time, close the socket."  ``heartbeat`` overrides ``timeout``.

The continuous feed's response is a little different than the other feed types
to simplify the job of the client - each line of the response is either empty
or a JSON object representing a single change, as found in the normal feed's
results.

If `limit` has been specified the feed will end with a `{ last_seq }` object.

.. code-block:: http

    GET /somedatabase/_changes?feed=continuous HTTP/1.1

.. code-block:: javascript

    {"seq":"1-g1AAAAF9eJzLYWBg4MhgTmHgz8tPSTV0MDQy1zMAQsMcoARTIkOS_P___7MSGXAqSVIAkkn2IFUZzIkMuUAee5pRqnGiuXkKA2dpXkpqWmZeagpu_Q4g_fGEbEkAqaqH2sIItsXAyMjM2NgUUwdOU_JYgCRDA5ACGjQfn30QlQsgKvcTVnkAovI-YZUPICpBvs0CAN1eY_c","id":"fresh","changes":[{"rev":"5-g1AAAAHxeJzLYWBg4MhgTmHgz8tPSTV0MDQy1zMAQsMcoARTIkOS_P___7MymBOZcoEC7MmJKSmJqWaYynEakaQAJJPsoaYwgE1JM0o1TjQ3T2HgLM1LSU3LzEtNwa3fAaQ_HkV_kkGyZWqSEXH6E0D666H6GcH6DYyMzIyNTUnwRR4LkGRoAFJAg-YjwiMtOdXCwJyU8ICYtABi0n6EnwzSzIxS00yI8hPEjAMQM-5nJTIQUPkAovI_UGUWAA0SgOI","id":"updated","changes":[{"rev":"2-7051cbe5c8faecd085a3fa619e6e6337"}]}
    {"seq":"3-g1AAAAHReJzLYWBg4MhgTmHgz8tPSTV0MDQy1zMAQsMcoARTIkOS_P___7MymBOZcoEC7MmJKSmJqWaYynEakaQAJJPsoaYwgE1JM0o1TjQ3T2HgLM1LSU3LzEtNwa3fAaQ_HkV_kkGyZWqSEXH6E0D660H6ExlwqspjAZIMDUAKqHA-yCZGiEuTUy0MzEnxL8SkBRCT9iPcbJBmZpSaZkKUmyFmHICYcZ-wux9AVIJ8mAUABgp6XQ","id":"deleted","changes":[{"rev":"2-eec205a9d413992850a6e32678485900"}],"deleted":true}
    ... tum tee tum ...
    {"seq":"6-g1AAAAIreJyVkEsKwjAURWMrqCOXoCuQ9MU0OrI70XyppcaRY92J7kR3ojupaVNopRQsgRd4yTlwb44QmqahQnN7VjpKImAr7E6Uu4eAI7EoiiJLQx6c3GIiuVJcx93vvQqxdFPsaguqLAY04YwpNLtYpc3RatXPJyW__-EFllst4D_-UPLXmh9VPAaICaEDUtixm-jmLie6N30YqTeYDenDmx7e9GwyYRODNuu_MnnHyzverV6AMkPkAMfHO1rdUAKUkqhLZV-_0o5j","id":"updated","changes":[{"rev":"3-825cb35de44c433bfb2df415563a19de"}]}

Obviously, `... tum tee tum ...` does not appear in the actual response, but
represents a long pause before the change with seq 6 occurred.

.. _Change Notifications in the book CouchDB The Definitive Guide: http://guide.couchdb.org/draft/notifications.html

.. _changes/eventsource:

Event Source
------------

The `eventsource` feed provides push notifications that can be consumed in
the form of DOM events in the browser. Refer to the `W3C eventsource
specification`_ for further details. CouchDB also honours the ``Last-Event-ID``
parameter.

.. code-block:: http

    GET /somedatabase/_changes?feed=eventsource HTTP/1.1

.. code-block:: javascript

    // define the event handling function
    if (window.EventSource) {

        var source = new EventSource("/somedatabase/_changes?feed=eventsource");
        source.onerror = function(e) {
            alert('EventSource failed.');
        };

        var results = [];
        var sourceListener = function(e) {
            var data = JSON.parse(e.data);
            results.push(data);
        };

        // start listening for events
        source.addEventListener('message', sourceListener, false);

        // stop listening for events
        source.removeEventListener('message', sourceListener, false);

    }

If you set a heartbeat interval (using the ``heartbeat`` query argument),
CouchDB will send a ``hearbeat`` event that you can subscribe to with:

.. code-block:: javascript

    source.addEventListener('heartbeat', function () {}, false);

This can be monitored by the client application to restart the EventSource
connection if needed (i.e. if the TCP connection gets stuck in a half-open
state).

.. note::
    EventSource connections are subject to cross-origin resource sharing
    restrictions. You might need to configure :ref:`CORS support
    <cors>` to get the EventSource to work in your application.

.. _W3C eventsource specification: http://www.w3.org/TR/eventsource/

.. _changes/filter:

Filtering
=========

You can filter the contents of the changes feed in a number of ways. The
most basic way is to specify one or more document IDs to the query. This
causes the returned structure value to only contain changes for the
specified IDs. Note that the value of this query argument should be a
JSON formatted array.

You can also filter the ``_changes`` feed by defining a filter function
within a design document. The specification for the filter is the same
as for replication filters. You specify the name of the filter function
to the ``filter`` parameter, specifying the design document name and
:ref:`filter name <filterfun>`. For example:

.. code-block:: http

    GET /db/_changes?filter=design_doc/filtername HTTP/1.1

Additionally, a couple of built-in filters are available and described
below.

.. _changes/filter/doc_ids:

_doc_ids
--------

This filter accepts only changes for documents which ID in specified in
``doc_ids`` query parameter or payload's object array. See
:post:`/{db}/_changes` for an example.

.. _changes/filter/selector:

_selector
---------

.. versionadded:: 2.0

This filter accepts only changes for documents which match a specified
selector, defined using the same :ref:`selector
syntax <find/selectors>` used for :ref:`_find <api/db/_find>`.

This is significantly more efficient than using a JavaScript filter
function and is the recommended option if filtering on document attributes only.

Note that, unlike JavaScript filters, selectors do not have access to the
request object.

**Request**:

.. code-block:: http

    POST /recipes/_changes?filter=_selector HTTP/1.1
    Content-Type: application/json
    Host: localhost:5984

    {
        "selector": { "_id": { "$regex": "^_design/" } }
    }

**Response**:

.. code-block:: http

    HTTP/1.1 200 OK
    Cache-Control: must-revalidate
    Content-Type: application/json
    Date: Tue, 06 Sep 2016 20:03:23 GMT
    Etag: "1H8RGBCK3ABY6ACDM7ZSC30QK"
    Server: CouchDB (Erlang OTP/18)
    Transfer-Encoding: chunked

    {
        "last_seq": "11-g1AAAAIreJyVkEEKwjAQRUOrqCuPoCeQZGIaXdmbaNIk1FLjyrXeRG-iN9Gb1LQRaimFlsAEJnkP_s8RQtM0VGhuz0qTmABfYXdI7h4CgeSiKIosDUVwcotJIpQSOmp_71TIpZty97OgymJAU8G5QrOLVdocrVbdfFzy-wYvcbLVEvrxh5K_NlJggIhSNiCFHbmJbu5yonttMoneYD6kD296eNOzzoRNBNqse2Xyjpd3vP96AcYNTQY4Pt5RdTOuHIwCY5S0qewLwY6OaA",
        "pending": 0,
        "results": [
            {
                "changes": [
                    {
                        "rev": "10-304cae84fd862832ea9814f02920d4b2"
                    }
                ],
                "id": "_design/ingredients",
                "seq": "8-g1AAAAHxeJzLYWBg4MhgTmHgz8tPSTV0MDQy1zMAQsMcoARTIkOS_P___7MymBOZcoEC7MmJKSmJqWaYynEakaQAJJPsoaYwgE1JM0o1TjQ3T2HgLM1LSU3LzEtNwa3fAaQ_HkV_kkGyZWqSEXH6E0D666H6GcH6DYyMzIyNTUnwRR4LkGRoAFJAg-ZnJTIQULkAonI_ws0GaWZGqWkmRLkZYsYBiBn3Cdv2AKIS7ENWsG2mxkampsaGmLqyAOYpgEo"
            },
            {
                "changes": [
                    {
                        "rev": "123-6f7c1b7c97a9e4f0d22bdf130e8fd817"
                    }
                ],
                "deleted": true,
                "id": "_design/cookbook",
                "seq": "9-g1AAAAHxeJzLYWBg4MhgTmHgz8tPSTV0MDQy1zMAQsMcoARTIkOS_P___7MymBOZcoEC7MmJKSmJqWaYynEakaQAJJPsoaYwgE1JM0o1TjQ3T2HgLM1LSU3LzEtNwa3fAaQ_HkV_kkGyZWqSEXH6E0D661F8YWBkZGZsbEqCL_JYgCRDA5ACGjQ_K5GBgMoFEJX7EW42SDMzSk0zIcrNEDMOQMy4T9i2BxCVYB-ygm0zNTYyNTU2xNSVBQDnK4BL"
            },
            {
                "changes": [
                    {
                        "rev": "6-5b8a52c22580e922e792047cff3618f3"
                    }
                ],
                "deleted": true,
                "id": "_design/meta",
                "seq": "11-g1AAAAIReJyVkE0OgjAQRiegUVceQU9g-mOpruQm2tI2SLCuXOtN9CZ6E70JFmpCCCFCmkyTdt6bfJMDwDQNFcztWWkcY8JXyB2cu49AgFwURZGloQhO7mGSCKWEjtrtnQq5dFXufhaoLIZoKjhXMLtYpc3RatXNxyW_b_ASJVstST_-UPLXRgpESEQpG5DCjlyFm7uc6F6bTKI3iA_Zhzc9vOlZZ0ImItqse2Xyjpd3vDMBfzo_vrPawLiaxihhjOI2lX0BirqHbg"
            }
        ]
    }

.. _changes/filter/selector/missing:

Missing selector
################

If the selector object is missing from the request body,
the error message is similar to the following example:

.. code-block:: json

   {
      "error": "bad request",
      "reason": "Selector must be specified in POST payload"
   }

.. _changes/filter/selector/invalidjson:

Not a valid JSON object
#######################

If the selector object is not a well-formed JSON object,
the error message is similar to the following example:

.. code-block:: json

   {
      "error": "bad request",
      "reason": "Selector error: expected a JSON object"
   }

.. _changes/filter/selector/invalidselector:

Not a valid selector
####################

If the selector object does not contain a valid selection expression,
the error message is similar to the following example:

.. code-block:: json

   {
      "error": "bad request",
      "reason": "Selector error: expected a JSON object"
   }

.. _changes/filter/design:

_design
-------

The ``_design`` filter accepts only changes for any design document within the
requested database.

**Request**:

.. code-block:: http

    GET /recipes/_changes?filter=_design HTTP/1.1
    Accept: application/json
    Host: localhost:5984

**Response**:

.. code-block:: http

    HTTP/1.1 200 OK
    Cache-Control: must-revalidate
    Content-Type: application/json
    Date: Tue, 06 Sep 2016 12:55:12 GMT
    ETag: "ARIHFWL3I7PIS0SPVTFU6TLR2"
    Server: CouchDB (Erlang OTP)
    Transfer-Encoding: chunked

    {
        "last_seq": "11-g1AAAAIreJyVkEEKwjAQRUOrqCuPoCeQZGIaXdmbaNIk1FLjyrXeRG-iN9Gb1LQRaimFlsAEJnkP_s8RQtM0VGhuz0qTmABfYXdI7h4CgeSiKIosDUVwcotJIpQSOmp_71TIpZty97OgymJAU8G5QrOLVdocrVbdfFzy-wYvcbLVEvrxh5K_NlJggIhSNiCFHbmJbu5yonttMoneYD6kD296eNOzzoRNBNqse2Xyjpd3vP96AcYNTQY4Pt5RdTOuHIwCY5S0qewLwY6OaA",
        "pending": 0,
        "results": [
            {
                "changes": [
                    {
                        "rev": "10-304cae84fd862832ea9814f02920d4b2"
                    }
                ],
                "id": "_design/ingredients",
                "seq": "8-g1AAAAHxeJzLYWBg4MhgTmHgz8tPSTV0MDQy1zMAQsMcoARTIkOS_P___7MymBOZcoEC7MmJKSmJqWaYynEakaQAJJPsoaYwgE1JM0o1TjQ3T2HgLM1LSU3LzEtNwa3fAaQ_HkV_kkGyZWqSEXH6E0D666H6GcH6DYyMzIyNTUnwRR4LkGRoAFJAg-ZnJTIQULkAonI_ws0GaWZGqWkmRLkZYsYBiBn3Cdv2AKIS7ENWsG2mxkampsaGmLqyAOYpgEo"
            },
            {
                "changes": [
                    {
                        "rev": "123-6f7c1b7c97a9e4f0d22bdf130e8fd817"
                    }
                ],
                "deleted": true,
                "id": "_design/cookbook",
                "seq": "9-g1AAAAHxeJzLYWBg4MhgTmHgz8tPSTV0MDQy1zMAQsMcoARTIkOS_P___7MymBOZcoEC7MmJKSmJqWaYynEakaQAJJPsoaYwgE1JM0o1TjQ3T2HgLM1LSU3LzEtNwa3fAaQ_HkV_kkGyZWqSEXH6E0D661F8YWBkZGZsbEqCL_JYgCRDA5ACGjQ_K5GBgMoFEJX7EW42SDMzSk0zIcrNEDMOQMy4T9i2BxCVYB-ygm0zNTYyNTU2xNSVBQDnK4BL"
            },
            {
                "changes": [
                    {
                        "rev": "6-5b8a52c22580e922e792047cff3618f3"
                    }
                ],
                "deleted": true,
                "id": "_design/meta",
                "seq": "11-g1AAAAIReJyVkE0OgjAQRiegUVceQU9g-mOpruQm2tI2SLCuXOtN9CZ6E70JFmpCCCFCmkyTdt6bfJMDwDQNFcztWWkcY8JXyB2cu49AgFwURZGloQhO7mGSCKWEjtrtnQq5dFXufhaoLIZoKjhXMLtYpc3RatXNxyW_b_ASJVstST_-UPLXRgpESEQpG5DCjlyFm7uc6F6bTKI3iA_Zhzc9vOlZZ0ImItqse2Xyjpd3vDMBfzo_vrPawLiaxihhjOI2lX0BirqHbg"
            }
        ]
    }

.. _changes/filter/view:

_view
-----

.. versionadded:: 1.2

The special filter ``_view`` allows to use existing
:ref:`map function <mapfun>` as the :ref:`filter <filterfun>`. If the map
function emits anything for the processed document it counts as accepted and
the changes event emits to the feed. For most use-practice cases `filter`
functions are very similar to `map` ones, so this feature helps to reduce
amount of duplicated code.

.. warning::
    While :ref:`map functions <mapfun>` doesn't process the design documents,
    using ``_view`` filter forces them to do this. You need to be sure, that
    they are ready to handle documents with *alien* structure without panic.

.. note::
    Using ``_view`` filter doesn't queries the view index files, so you cannot
    use common :ref:`view query parameters <api/ddoc/view>` to additionally
    filter the changes feed by index key. Also, CouchDB doesn't returns
    the result instantly as it does for views - it really uses the specified
    map function as filter.

    Moreover, you cannot make such filters dynamic e.g. process the request
    query parameters or handle the :ref:`userctx_object` - the map function is
    only operates with the document.

**Request**:

.. code-block:: http

    GET /recipes/_changes?filter=_view&view=ingredients/by_recipe HTTP/1.1
    Accept: application/json
    Host: localhost:5984

**Response**:

.. code-block:: http

    HTTP/1.1 200 OK
    Cache-Control: must-revalidate
    Content-Type: application/json
    Date: Tue, 06 Sep 2016 12:57:56 GMT
    ETag: "ARIHFWL3I7PIS0SPVTFU6TLR2"
    Server: CouchDB (Erlang OTP)
    Transfer-Encoding: chunked

    {
        "last_seq": "11-g1AAAAIreJyVkEEKwjAQRUOrqCuPoCeQZGIaXdmbaNIk1FLjyrXeRG-iN9Gb1LQRaimFlsAEJnkP_s8RQtM0VGhuz0qTmABfYXdI7h4CgeSiKIosDUVwcotJIpQSOmp_71TIpZty97OgymJAU8G5QrOLVdocrVbdfFzy-wYvcbLVEvrxh5K_NlJggIhSNiCFHbmJbu5yonttMoneYD6kD296eNOzzoRNBNqse2Xyjpd3vP96AcYNTQY4Pt5RdTOuHIwCY5S0qewLwY6OaA",
        "results": [
            {
                "changes": [
                    {
                        "rev": "13-bcb9d6388b60fd1e960d9ec4e8e3f29e"
                    }
                ],
                "id": "SpaghettiWithMeatballs",
                "seq": "11-g1AAAAIReJyVkE0OgjAQRiegUVceQU9g-mOpruQm2tI2SLCuXOtN9CZ6E70JFmpCCCFCmkyTdt6bfJMDwDQNFcztWWkcY8JXyB2cu49AgFwURZGloQhO7mGSCKWEjtrtnQq5dFXufhaoLIZoKjhXMLtYpc3RatXNxyW_b_ASJVstST_-UPLXRgpESEQpG5DCjlyFm7uc6F6bTKI3iA_Zhzc9vOlZZ0ImItqse2Xyjpd3vDMBfzo_vrPawLiaxihhjOI2lX0BirqHbg"
            }
        ]
    }
