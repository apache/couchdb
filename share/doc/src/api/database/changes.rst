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
  be provided, for example if a document has had fields added, and then deleted,
  an API client checking for changes will not necessarily receive the
  intermediate state of added documents.

  This can be used to listen for update and modifications to the database for
  post processing or synchronization, and for practical purposes, a continuously
  connected ``_changes`` feed is a reasonable approach for generating a
  real-time log for most applications.

  :param db: Database name
  :<header Accept: - :mimetype:`application/json`
                   - :mimetype:`text/event-stream`
                   - :mimetype:`text/plain`
  :<header Last-Event-ID: ID of the last events received by the server on a
    previous connection. Overrides `since` query parameter.
  :query array doc_ids: List of document IDs to filter the changes feed as
   valid JSON array. Used with :ref:`_doc_ids <changes/filter/doc_ids>` filter.
   Since `length of URL is limited`_, it is better to use
   :post:`/{db}/_changes` instead.
  :query boolean conflicts: Includes `conflicts` information in response.
    Ignored if `include_docs` isn't ``true``. Default is ``false``.
  :query boolean descending: Return the change results in descending sequence
    order (most recent change first). Default is ``false``.
  :query string feed: see :ref:`changes`. Default is ``normal``.
  :query string filter: Reference to a :ref:`filter function <filterfun>`
    from a design document that will filter whole stream emitting only filtered
    events. See the section `Change Notifications in the book
    CouchDB The Definitive Guide`_ for more information.
  :query number heartbeat: Period in *milliseconds* after which an empty line is
    sent in the results. Only applicable for :ref:`longpoll <changes/longpoll>`
    or :ref:`continuous <changes/continuous>` feeds. Overrides any timeout to
    keep the feed alive indefinitely. Default is ``60000``. May be ``true`` to
    use default value.
  :query boolean include_docs: Include the associated document with each result.
    If there are conflicts, only the winning revision is returned.
    Default is ``false``.
  :query boolean attachments: Include the Base64-encoded content of
    :ref:`attachments <api/doc/attachments>` in the documents that are included
    if `include_docs` is ``true``. Ignored if `include_docs` isn't ``true``.
    Default is ``false``.
  :query boolean att_encoding_info: Include encoding information in attachment
    stubs if `include_docs` is ``true`` and the particular attachment is
    compressed. Ignored if `include_docs` isn't ``true``. Default is ``false``.
  :query number last-event-id: Alias of `Last-Event-ID` header.
  :query number limit: Limit number of result rows to the specified value
    (note that using ``0`` here has the same effect as ``1``).
  :query since: Start the results from the change immediately after the given
    sequence number. Can be integer number or ``now`` value. Default is ``0``.
  :query string style: Specifies how many revisions are returned in the changes
    array. The default, ``main_only``, will only return the current "winning"
    revision; ``all_docs`` will return all leaf revisions (including conflicts
    and deleted former conflicts).
  :query number timeout: Maximum period in *milliseconds* to wait for a change
    before the response is sent, even if there are no results. Only applicable
    for :ref:`longpoll <changes/longpoll>` or :ref:`continuous
    <changes/continuous>` feeds. Default value is specified by
    :config:option:`httpd/changes_timeout` configuration option.
    Note that ``60000`` value is also the default maximum timeout to prevent
    undetected dead connections.
  :query string view: Allows to use view functions as filters. Documents
    counted as "passed" for view filter in case if map function emits at least
    one record for them. See :ref:`changes/filter/view` for more info.
  :>header Cache-Control: ``no-cache`` if changes feed is
    :ref:`eventsource <changes/eventsource>`
  :>header Content-Type: - :mimetype:`application/json`
                         - :mimetype:`text/event-stream`
                         - :mimetype:`text/plain; charset=utf-8`
  :>header ETag: Response hash is changes feed is `normal`
  :>header Transfer-Encoding: ``chunked``
  :>json number last_seq: Last change sequence number
  :>json array results: Changes made to a database
  :code 200: Request completed successfully
  :code 400: Bad request

  The ``result`` field of database changes

  :json array changes: List of document`s leafs with single field ``rev``
  :json string id: Document ID
  :json number seq: Update sequence number

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
        "last_seq": 11,
        "results": [
            {
                "changes": [
                    {
                        "rev": "2-7051cbe5c8faecd085a3fa619e6e6337"
                    }
                ],
                "id": "6478c2ae800dfc387396d14e1fc39626",
                "seq": 6
            },
            {
                "changes": [
                    {
                        "rev": "3-7379b9e515b161226c6559d90c4dc49f"
                    }
                ],
                "deleted": true,
                "id": "5bbc9ca465f1b0fcd62362168a7c8831",
                "seq": 9
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
                "seq": 11
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

.. warning::
   Using the ``attachments`` parameter to include attachments in the changes
   feed is not recommended for large attachment sizes. Also note that the
   Base64-encoding that is used leads to a 33% overhead (i.e. one third) in
   transfer size for attachments.


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
        "last_seq": 38,
        "results": [
            {
                "changes": [
                    {
                        "rev": "13-bcb9d6388b60fd1e960d9ec4e8e3f29e"
                    }
                ],
                "id": "SpaghettiWithMeatballs",
                "seq": 38
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
    {"seq":1,"id":"fresh","changes":[{"rev":"1-967a00dff5e02add41819138abb3284d"}]},
    {"seq":3,"id":"updated","changes":[{"rev":"2-7051cbe5c8faecd085a3fa619e6e6337"}]},
    {"seq":5,"id":"deleted","changes":[{"rev":"2-eec205a9d413992850a6e32678485900"}],"deleted":true}
    ],
    "last_seq":5}

``results`` is the list of changes in sequential order. New and changed
documents only differ in the value of the rev; deleted documents include the
``"deleted": true`` attribute. (In the ``style=all_docs mode``, deleted applies
only to the current/winning revision. The other revisions listed might be
deleted even if there is no deleted property; you have to ``GET`` them
individually to make sure.)

``last_seq`` is the sequence number of the last update returned. (Currently it
will always be the same as the seq of the last item in results.)

Sending a ``since`` param in the query string skips all changes up to and
including the given sequence number::

    GET /somedatabase/_changes?since=3 HTTP/1.1


The return structure for ``normal`` and ``longpoll`` modes is a JSON
array of changes objects, and the last update sequence number.

In the return format for ``continuous`` mode, the server sends a ``CRLF``
(carriage-return, linefeed) delimited line for each change. Each line
contains the `JSON object` described above.

You can also request the full contents of each document change (instead
of just the change notification) by using the ``include_docs`` parameter.

.. code-block:: javascript

    {
        "last_seq": 5
        "results": [
            {
                "changes": [
                    {
                        "rev": "2-eec205a9d413992850a6e32678485900"
                    }
                ],
                "deleted": true,
                "id": "deleted",
                "seq": 5,
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
intervals to ensure that the connection is kept open for new changes
and updates.

The continuous feed's response is a little different than the other feed types
to simplify the job of the client - each line of the response is either empty
or a JSON object representing a single change, as found in the normal feed's
results.

.. code-block:: text

    GET /somedatabase/_changes?feed=continuous HTTP/1.1

.. code-block:: javascript

    {"seq":1,"id":"fresh","changes":[{"rev":"1-967a00dff5e02add41819138abb3284d"}]}
    {"seq":3,"id":"updated","changes":[{"rev":"2-7051cbe5c8faecd085a3fa619e6e6337"}]}
    {"seq":5,"id":"deleted","changes":[{"rev":"2-eec205a9d413992850a6e32678485900"}],"deleted":true}
    ... tum tee tum ...
    {"seq":6,"id":"updated","changes":[{"rev":"3-825cb35de44c433bfb2df415563a19de"}]}

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

.. code-block:: text

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

If you set a heartbeat interval (using the ``heartbeat`` query argument), CouchDB will
send a ``hearbeat`` event that you can subscribe to with:

.. code-block:: javascript

    source.addEventListener('heartbeat', function () {}, false);

This can be monitored by the client application to restart the EventSource connection if
needed (i.e. if the TCP connection gets stuck in a half-open state).

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

    GET /db/_changes?filter=design_doc/filtername

Additionally, there are couple of builtin filters are available and described
below.


.. _changes/filter/doc_ids:

_doc_ids
--------

This filter accepts only changes for documents which ID in specified in
``doc_ids`` query parameter or payload's object array. See
:post:`/{db}/_changes` for an example.


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
  Date: Sat, 28 Sep 2013 07:28:28 GMT
  ETag: "ARIHFWL3I7PIS0SPVTFU6TLR2"
  Server: CouchDB (Erlang OTP)
  Transfer-Encoding: chunked

  {
      "last_seq": 38,
      "results": [
          {
              "changes": [
                  {
                      "rev": "10-304cae84fd862832ea9814f02920d4b2"
                  }
              ],
              "id": "_design/ingredients",
              "seq": 29
          },
          {
              "changes": [
                  {
                      "rev": "123-6f7c1b7c97a9e4f0d22bdf130e8fd817"
                  }
              ],
              "deleted": true,
              "id": "_design/cookbook",
              "seq": 35
          },
          {
              "changes": [
                  {
                      "rev": "6-5b8a52c22580e922e792047cff3618f3"
                  }
              ],
              "deleted": true,
              "id": "_design/meta",
              "seq": 36
          }
      ]
  }


.. _changes/filter/view:

_view
-----

.. versionadded:: 1.2

The special filter ``_view`` allows to use existing :ref:`map function <mapfun>`
as the :ref:`filter <filterfun>`. If the map function emits anything for the
processed document it counts as accepted and the changes event emits to the
feed. For most use-practice cases `filter` functions are very similar to `map`
ones, so this feature helps to reduce amount of duplicated code.

.. warning::

   While :ref:`map functions <mapfun>` doesn't process the design documents,
   using ``_view`` filter forces them to do this. You need to be sure, that
   they are ready to handle documents with *alien* structure without panic
   crush.

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
  Date: Sat, 28 Sep 2013 07:36:40 GMT
  ETag: "ARIHFWL3I7PIS0SPVTFU6TLR2"
  Server: CouchDB (Erlang OTP)
  Transfer-Encoding: chunked

  {
      "last_seq": 38,
      "results": [
          {
              "changes": [
                  {
                      "rev": "13-bcb9d6388b60fd1e960d9ec4e8e3f29e"
                  }
              ],
              "id": "SpaghettiWithMeatballs",
              "seq": 38
          }
      ]
  }
