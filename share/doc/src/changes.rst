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

.. _changes:

============
Changes Feed
============

Polling
=======

A list of changes made to documents in the database, in the order they were
made, can be obtained from the database's ``_changes`` resource. You can query
the ``_changes`` resource by issuing a ``GET`` request with the following
(optional) parameters:

+--------------+----------------------------------------------+---------------+--------------+
| Parameter    | Value                                        | Default Value |  Notes       |
+==============+==============================================+===============+==============+
| since        | seqnum / now                                 | 0             | \(1)         |
+--------------+----------------------------------------------+---------------+--------------+
| limit        | maxsequences                                 | none          | \(2)         |
+--------------+----------------------------------------------+---------------+--------------+
| descending   | boolean                                      | false         | \(3)         |
+--------------+----------------------------------------------+---------------+--------------+
| feed         | normal / longpoll / continuous / eventsource | normal        | \(4)         |
+--------------+----------------------------------------------+---------------+--------------+
| heartbeat    | milliseconds                                 | 60000         | \(5)         |
+--------------+----------------------------------------------+---------------+--------------+
| timeout      | milliseconds                                 | 60000         | \(6)         |
+--------------+----------------------------------------------+---------------+--------------+
| filter       | designdoc/filtername / _view                 | none          | \(7)         |
+--------------+----------------------------------------------+---------------+--------------+
| include_docs | boolean                                      | false         | \(8)         |
+--------------+----------------------------------------------+---------------+--------------+
| style        | all_docs / main_only                         | main_only     | \(9)         |
+--------------+----------------------------------------------+---------------+--------------+
| view         | designdoc/filtername                         | none          | \(10)        |
+--------------+----------------------------------------------+---------------+--------------+

Notes:

(1) Start the results from the change immediately after the given sequence
    number.

(2) Limit number of result rows to the specified value (note that using 0 here
    has the same effect as 1).

(3) Return the change results in descending sequence order (most recent change
    first)

(4) Select the type of feed.

(5) Period in milliseconds after which an empty line is sent in the results.
    Only applicable for `longpoll` or `continuous` feeds. Overrides any timeout
    to keep the feed alive indefinitely.

(6) Maximum period in milliseconds to wait for a change before the response is
    sent, even if there are no results. Only applicable for `longpoll` or
    `continuous` feeds. Note that 60000 is also the default maximum timeout to
    prevent undetected dead connections.

    You can change the default maximum timeout in your ini-configuration:

    .. code-block:: ini

        [httpd]
        changes_timeout=#millisecs

(7) Reference to a :ref:`filter function <filterfun>` from a design document
    that will filter whole stream emitting only filtered events.
    See the `section in the book`_ for more information.

(8) Include the associated document with each result. If there are conflicts,
    only the winning revision is returned.

(9) Specifies how many revisions are returned in the changes array.
    The default, `main_only`, will only return the current "winning" revision;
    `all_docs` will return all leaf revisions (including conflicts and deleted
    former conflicts.)

(10) Allows to use view functions as filters. It requires to set ``filter``
     special value `_view` to enable this feature. Documents counted as "passed"
     for view filter in case if map function emits at least one record for them.

.. versionchanged:: 0.11.0 added ``include_docs`` parameter
.. versionchanged:: 1.2.0 added ``view`` parameter and special value `_view`
   for ``filter`` one
.. versionchanged:: 1.3.0 ``since`` parameter could take `now` value to start
   listen changes since current seq number.
.. versionchanged:: 1.3.0 ``eventsource`` feed type added.

By default all changes are immediately returned as a JSON object::

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

.. code-block:: javascript

    {"results":[
    {"seq":5,"id":"deleted","changes":[{"rev":"2-eec205a9d413992850a6e32678485900"}],"deleted":true}
    ],
    "last_seq":5} 

Long Polling
============

The `longpoll` feed (probably most useful used from a browser) is a more
efficient form of polling that waits for a change to occur before the response
is sent. `longpoll` avoids the need to frequently poll CouchDB to discover
nothing has changed!

The response is basically the same JSON as is sent for the normal feed.

A timeout limits the maximum length of time the connection is open. If there
are no changes before the timeout expires the response's results will be an
empty list.  

Continuous
==========

Polling the CouchDB server is not a good thing to do. Setting up new HTTP
connections just to tell the client that nothing happened puts unnecessary
strain on CouchDB.

A continuous feed stays open and connected to the database until explicitly
closed and changes are sent to the client as they happen, i.e. in near
real-time.

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

.. _section in the book: http://books.couchdb.org/relax/reference/change-notifications

Event Source
============

The `eventsource` feed provides push notifications that can be consumed in
the form of DOM events in the browser. Refer to the `W3C eventsource
specification`_ for further details. CouchDB honors the ``Last-Event-ID`` header,
and if it's present it will take precedence over the ``since`` query parameter.

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

.. note::

   EventSource connections are subject to cross-origin resource sharing
   restrictions. You might need to use the experimental :ref:`CORS support
   <cors>` to get the EventSource to work in your application.

.. _W3C eventsource specification: http://www.w3.org/TR/eventsource/
