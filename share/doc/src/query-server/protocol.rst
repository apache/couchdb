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


.. _query-server/protocol:

=====================
Query Server Protocol
=====================

The `Query Server` is an external process that communicates with CouchDB via a
JSON protocol over stdio  and processes all design functions calls:
`views`, `shows`, `lists`, `filters`, `updates` and `validate_doc_update`.

CouchDB communicates with the Query Server process though stdio interface by
JSON messages that terminated by newline character. Messages that are sent to
the Query Server are always `array`-typed that could be matched by the pattern
``[<command>, <*arguments>]\n``.

.. note::
   To simplify examples reading we omitted trailing ``\n`` character to let
   Sphinx highlight them well. Also, all examples contain formatted JSON values
   while real data transfers in compact mode without formatting spaces.

.. _qs/reset:

``reset``
=========

:Command: ``reset``
:Arguments: :ref:`Query server state <config/query_server_config>` (optional)
:Returns: ``true``

This resets the state of the Query Server and makes it forget all previous
input. If applicable, this is the point to run garbage collection.

CouchDB sends::

    ["reset"]

The Query Server answers::

    true

To set up new Query Server state the second argument is used with object data.
This argument is used

CouchDB sends::

    ["reset", {"reduce_limit": true, "timeout": 5000}]

The Query Server answers::

    true


.. _qs/add_lib:

``add_lib``
===========

:Command: ``add_lib``
:Arguments: CommonJS library object by ``views/lib`` path
:Returns: ``true``

Adds :ref:`CommonJS <commonjs>` library to Query Server state for further usage
in `map` functions.

CouchDB sends::

  [
    "add_lib",
    {
      "utils": "exports.MAGIC = 42;"
    }
  ]

The Query Server answers::

  true


.. note::

   This library shouldn't have any side effects nor track its own state
   or you'll have a lot of happy debugging time if something went wrong.
   Remember that a complete index rebuild is a heavy operation and this is
   the only way to fix your mistakes with shared state.

.. _qs/add_fun:

``add_fun``
-----------

:Command: ``add_fun``
:Arguments: Map function source code.
:Returns: ``true``

When creating or updating a view the Query Server gets sent the view function
for evaluation. The Query Server should parse, compile and evaluate the
function it receives to make it callable later. If this fails, the Query Server
returns an error. CouchDB might store several functions before sending in any 
actual documents.

CouchDB sends::

    [
      "add_fun",
      "function(doc) { if(doc.score > 50) emit(null, {'player_name': doc.name}); }"
    ]

The Query Server answers::

    true


.. _qs/map_doc:

``map_doc``
===========

:Command: ``map_doc``
:Arguments: Document object
:Returns: Array of key-value pairs per applied :ref:`function <qs/add_fun>`

When the view function is stored in the Query Server, CouchDB starts sending in
all the documents in the database, one at a time. The Query Server calls the
previously stored functions one after another with a document and stores its
result. When all functions have been called, the result is returned as a JSON
string.

CouchDB sends::

    [
      "map_doc",
      {
        "_id": "8877AFF9789988EE",
        "_rev": "3-235256484",
        "name": "John Smith",
        "score": 60
      }
    ]

If the function above is the only function stored, the Query Server answers::

    [
      [
        [null, {"player_name": "John Smith"}]
      ]
    ]

That is, an array with the result for every function for the given document.

If a document is to be excluded from the view, the array should be empty.

CouchDB sends::

    [
      "map_doc",
      {
        "_id": "9590AEB4585637FE",
        "_rev": "1-674684684",
        "name": "Jane Parker",
        "score": 43
      }
    ]

The Query Server answers::

    [[]]


.. _qs/reduce:

``reduce``
==========

:Command: ``reduce``
:Arguments:
  - Reduce function source
  - Array of :ref:`map function <mapfun>` results where each item represented
    in format ``[[key, id-of-doc], value]``
:Returns: Array with pair values: ``true`` and another array with reduced result

If the view has a reduce function defined, CouchDB will enter into the reduce
phase. The view server will receive a list of reduce functions and some map
results on which it can apply them.

CouchDB sends::

  [
    "reduce",
    [
      "function(k, v) { return sum(v); }"
    ],
    [
      [[1, "699b524273605d5d3e9d4fd0ff2cb272"], 10],
      [[2, "c081d0f69c13d2ce2050d684c7ba2843"], 20],
      [[null, "foobar"], 3]
    ]
  ]

The Query Server answers::

  [
    true,
    [33]
  ]

Note that even though the view server receives the map results in the form
``[[key, id-of-doc], value]``, the function may receive them in a different
form. For example, the JavaScript Query Server applies functions on the list of
keys and the list of values.

.. _qs/rereduce:

``rereduce``
============

:Command: ``rereduce``
:Arguments: List of values.

When building a view, CouchDB will apply the reduce step directly to the output
of the map step and the rereduce step to the output of a previous reduce step.

CouchDB will send a list of values, with no keys or document ids, to the
rereduce step.

CouchDB sends::

  [
    "rereduce",
    [
      "function(k, v, r) { return sum(v); }"
    ],
    [
      33,
      55,
      66
    ]
  ]

The Query Server answers::

  [
    true,
    [154]
  ]


.. _qs/ddoc:

``ddoc``
========

:Command: ``ddoc``
:Arguments: Array of objects.

  - First phase (ddoc initialization):

    - ``"new"``
    - Design document ``_id``
    - Design document object

  - Second phase (design function execution):

    - Design document ``_id``
    - Function path as an array of object keys
    - Array of function arguments

:Returns:

  - First phase (ddoc initialization): ``true``
  - Second phase (design function execution): custom object depending on
    executed function



This command acts in two phases: `ddoc` registration and `design function`
execution.

In the first phase CouchDB sends a full design document content to the Query
Server to let it cache it by ``_id`` value for further function execution.

To do this, CouchDB sends::

  [
    "ddoc",
    "new",
    "_design/temp",
    {
      "_id": "_design/temp",
      "_rev": "8-d7379de23a751dc2a19e5638a7bbc5cc",
      "language": "javascript",
      "shows": {
        "request": "function(doc,req){ return {json: req}; }",
        "hello": "function(doc,req){ return {body: 'Hello, ' + (doc || {})._id + '!'}; }"
      }
    }
  ]

The Query Server answers::

  true


After than this design document is ready to serve next subcommands - that's the
second phase.

.. note::

   Each ``ddoc`` subcommand is the root design document key, so they are not
   actually subcommands, but first elements of the JSON path that may be handled
   and processed.

   The pattern for subcommand execution is common:

   ``["ddoc", <design_doc_id>, [<subcommand>, <funcname>], [<argument1>, <argument2>, ...]]``


.. _qs/ddoc/shows:

``shows``
---------

:Command: ``ddoc``
:SubCommand: ``shows``
:Arguments:

  - Document object or ``null`` if document `id` wasn't specified in request
  - :ref:`request_object`

:Returns: Array with two elements:

  - ``"resp"``
  - :ref:`response_object`

Executes :ref:`show function <showfun>`.

Couchdb sends::

  [
    "ddoc",
    "_design/temp",
    [
        "shows",
        "doc"
    ],
    [
      null,
      {
        "info": {
          "db_name": "test",
          "doc_count": 8,
          "doc_del_count": 0,
          "update_seq": 105,
          "purge_seq": 0,
          "compact_running": false,
          "disk_size": 15818856,
          "data_size": 1535048,
          "instance_start_time": "1359952188595857",
          "disk_format_version": 6,
          "committed_update_seq": 105
        },
        "id": null,
        "uuid": "169cb4cc82427cc7322cb4463d0021bb",
        "method": "GET",
        "requested_path": [
          "api",
          "_design",
          "temp",
          "_show",
          "request"
        ],
        "path": [
          "api",
          "_design",
          "temp",
          "_show",
          "request"
        ],
        "raw_path": "/api/_design/temp/_show/request",
        "query": {},
        "headers": {
          "Accept": "*/*",
          "Host": "localhost:5984",
          "User-Agent": "curl/7.26.0"
        },
        "body": "undefined",
        "peer": "127.0.0.1",
        "form": {},
        "cookie": {},
        "userCtx": {
          "db": "api",
          "name": null,
          "roles": [
            "_admin"
          ]
        },
        "secObj": {}
      }
    ]
  ]

The Query Server sends::

  [
    "resp",
    {
      "body": "Hello, undefined!"
    }
  ]


.. _qs/ddoc/lists:

``lists``
---------

:Command: ``ddoc``
:SubCommand: ``lists``
:Arguments:

  - :ref:`view_head_info_object`:
  - :ref:`request_object`

:Returns: Array. See below for details.

Executes :ref:`list function <listfun>`.

The communication protocol for `list` functions is a bit complex so let's use
an example for illustration.

Let's assume that we have view a function that emits `id-rev` pairs::

  function(doc) {
    emit(doc._id, doc._rev);
  }

And we'd like to emulate ``_all_docs`` JSON response with list function. Our
*first* version of the list functions looks like this::

  function(head, req){
    start({'headers': {'Content-Type': 'application/json'}});
    var resp = head;
    var rows = [];
    while(row=getRow()){
      rows.push(row);
    }
    resp.rows = rows;
    return toJSON(resp);
  }

The whole communication session during list function execution could be divided
on three parts:

#. Initialization

   The first returned object from list function is an array of next structure::

      ["start", <chunks>, <headers>]

   Where ``<chunks>`` is an array of text chunks that will be sent to client
   and ``<headers>`` is an object with response HTTP headers.

   This message is sent from the Query Server to CouchDB on the
   :js:func:`start` call which initialize HTTP response to the client::

     [
       "start",
       [],
       {
         "headers": {
           "Content-Type": "application/json"
         }
       }
     ]

   After this, the list function may start to process view rows.

#. View Processing

   Since view results can be extremely large, it is not wise to pass all its
   rows in a single command. Instead, CouchDB can send view rows one by one
   to the Query Server allowing processing view and output generation in a
   streaming way.

   CouchDB sends a special array that carries view row data::

     [
       "list_row",
       {
         "id": "0cb42c267fe32d4b56b3500bc503e030",
         "key": "0cb42c267fe32d4b56b3500bc503e030",
         "value": "1-967a00dff5e02add41819138abb3284d"
       }
     ]

   If Query Server has something to return on this, it returns an array with a
   ``"chunks"`` item in the head and an array of data in the tail. Now, for our
   case it has nothing to return, so the response will be::

     [
       "chunks",
       []
     ]

   When there is no more view rows to process, CouchDB sends special message,
   that signs about that there is no more data to send from his side::

     ["list_end"]


#. Finalization

   The last stage of the communication process is the returning *list tail*:
   the last data chunk. After this, processing list function will be completed
   and client will receive complete response.

   For our example the last message will be the next::

     [
       "end",
       [
         "{\"total_rows\":2,\"offset\":0,\"rows\":[{\"id\":\"0cb42c267fe32d4b56b3500bc503e030\",\"key\":\"0cb42c267fe32d4b56b3500bc503e030\",\"value\":\"1-967a00dff5e02add41819138abb3284d\"},{\"id\":\"431926a69504bde41851eb3c18a27b1f\",\"key\":\"431926a69504bde41851eb3c18a27b1f\",\"value\":\"1-967a00dff5e02add41819138abb3284d\"}]}"
       ]
     ]

There, we had made a big mistake: we had returned out result in a single
message from the Query Server. That's ok when there are only a few rows in the
view result, but it's not acceptable for millions documents and millions view 
rows

Let's fix our list function and see the changes in communication::

  function(head, req){
    start({'headers': {'Content-Type': 'application/json'}});
    send('{');
    send('"total_rows":' + toJSON(head.total_rows) + ',');
    send('"offset":' + toJSON(head.offset) + ',');
    send('"rows":[');
    if (row=getRow()){
      send(toJSON(row));
    }
    while(row=getRow()){
      send(',' + toJSON(row));
    }
    send(']');
    return '}';
  }

"Wait, what?" - you'd like to ask. Yes, we'd build JSON response manually by
string chunks, but let's take a look on logs::

  [Wed, 24 Jul 2013 05:45:30 GMT] [debug] [<0.19191.1>] OS Process #Port<0.4444> Output :: ["start",["{","\"total_rows\":2,","\"offset\":0,","\"rows\":["],{"headers":{"Content-Type":"application/json"}}]
  [Wed, 24 Jul 2013 05:45:30 GMT] [info] [<0.18963.1>] 127.0.0.1 - - GET /blog/_design/post/_list/index/all_docs 200
  [Wed, 24 Jul 2013 05:45:30 GMT] [debug] [<0.19191.1>] OS Process #Port<0.4444> Input  :: ["list_row",{"id":"0cb42c267fe32d4b56b3500bc503e030","key":"0cb42c267fe32d4b56b3500bc503e030","value":"1-967a00dff5e02add41819138abb3284d"}]
  [Wed, 24 Jul 2013 05:45:30 GMT] [debug] [<0.19191.1>] OS Process #Port<0.4444> Output :: ["chunks",["{\"id\":\"0cb42c267fe32d4b56b3500bc503e030\",\"key\":\"0cb42c267fe32d4b56b3500bc503e030\",\"value\":\"1-967a00dff5e02add41819138abb3284d\"}"]]
  [Wed, 24 Jul 2013 05:45:30 GMT] [debug] [<0.19191.1>] OS Process #Port<0.4444> Input  :: ["list_row",{"id":"431926a69504bde41851eb3c18a27b1f","key":"431926a69504bde41851eb3c18a27b1f","value":"1-967a00dff5e02add41819138abb3284d"}]
  [Wed, 24 Jul 2013 05:45:30 GMT] [debug] [<0.19191.1>] OS Process #Port<0.4444> Output :: ["chunks",[",{\"id\":\"431926a69504bde41851eb3c18a27b1f\",\"key\":\"431926a69504bde41851eb3c18a27b1f\",\"value\":\"1-967a00dff5e02add41819138abb3284d\"}"]]
  [Wed, 24 Jul 2013 05:45:30 GMT] [debug] [<0.19191.1>] OS Process #Port<0.4444> Input  :: ["list_end"]
  [Wed, 24 Jul 2013 05:45:30 GMT] [debug] [<0.19191.1>] OS Process #Port<0.4444> Output :: ["end",["]","}"]]

Note, that now the Query Server sends response by lightweight chunks and if
our communication process was extremely slow, the client will see how response
data appears on their screen. Chunk by chunk, without waiting for the complete
result, like he have for our previous list function.

.. _qs/ddoc/updates:

``updates``
-----------

:Command: ``ddoc``
:SubCommand: ``updates``
:Arguments:

  - Document object or ``null`` if document `id` wasn't specified in request
  - :ref:`request_object`

:Returns: Array with there elements:

  - ``"up"``
  - Document object or ``null`` if nothing should be stored
  - :ref:`response_object`

Executes :ref:`update function <updatefun>`.

CouchDB sends::

    [
        "ddoc",
        "_design/id",
        [
            "updates",
            "nothing"
        ],
        [
            null,
            {
                "info": {
                    "db_name": "test",
                    "doc_count": 5,
                    "doc_del_count": 0,
                    "update_seq": 16,
                    "purge_seq": 0,
                    "compact_running": false,
                    "disk_size": 8044648,
                    "data_size": 7979601,
                    "instance_start_time": "1374612186131612",
                    "disk_format_version": 6,
                    "committed_update_seq": 16
                },
                "id": null,
                "uuid": "7b695cb34a03df0316c15ab529002e69",
                "method": "POST",
                "requested_path": [
                    "test",
                    "_design",
                    "1139",
                    "_update",
                    "nothing"
                ],
                "path": [
                    "test",
                    "_design",
                    "1139",
                    "_update",
                    "nothing"
                ],
                "raw_path": "/test/_design/1139/_update/nothing",
                "query": {},
                "headers": {
                    "Accept": "*/*",
                    "Accept-Encoding": "identity, gzip, deflate, compress",
                    "Content-Length": "0",
                    "Host": "localhost:5984"
                },
                "body": "",
                "peer": "127.0.0.1",
                "form": {},
                "cookie": {},
                "userCtx": {
                    "db": "test",
                    "name": null,
                    "roles": [
                        "_admin"
                    ]
                },
                "secObj": {}
            }
        ]
    ]

The Query Server answers::

  [
    "up",
    null,
    {"body": "document id wasn't provided"}
  ]

or in case of successful update::

  [
    "up",
    {
      "_id": "7b695cb34a03df0316c15ab529002e69",
      "hello": "world!"
    },
    {"body": "document was updated"}
  ]


.. _qs/ddoc/filters:

``filters``
-----------

:Command: ``ddoc``
:SubCommand: ``filters``
:Arguments:

  - Array of document objects
  - :ref:`request_object`

:Returns: Array of two elements:

  - ``true``
  - Array of booleans in the same order of input documents.

Executes :ref:`filter function <filterfun>`.

CouchDB sends::

  [
      "ddoc",
      "_design/test",
      [
          "filters",
          "random"
      ],
      [
          [
              {
                  "_id": "431926a69504bde41851eb3c18a27b1f",
                  "_rev": "1-967a00dff5e02add41819138abb3284d",
                  "_revisions": {
                      "start": 1,
                      "ids": [
                          "967a00dff5e02add41819138abb3284d"
                      ]
                  }
              },
              {
                  "_id": "0cb42c267fe32d4b56b3500bc503e030",
                  "_rev": "1-967a00dff5e02add41819138abb3284d",
                  "_revisions": {
                      "start": 1,
                      "ids": [
                          "967a00dff5e02add41819138abb3284d"
                      ]
                  }
              }
          ],
          {
              "info": {
                  "db_name": "test",
                  "doc_count": 5,
                  "doc_del_count": 0,
                  "update_seq": 19,
                  "purge_seq": 0,
                  "compact_running": false,
                  "disk_size": 8056936,
                  "data_size": 7979745,
                  "instance_start_time": "1374612186131612",
                  "disk_format_version": 6,
                  "committed_update_seq": 19
              },
              "id": null,
              "uuid": "7b695cb34a03df0316c15ab529023a81",
              "method": "GET",
              "requested_path": [
                  "test",
                  "_changes?filter=test",
                  "random"
              ],
              "path": [
                  "test",
                  "_changes"
              ],
              "raw_path": "/test/_changes?filter=test/random",
              "query": {
                  "filter": "test/random"
              },
              "headers": {
                  "Accept": "application/json",
                  "Accept-Encoding": "identity, gzip, deflate, compress",
                  "Content-Length": "0",
                  "Content-Type": "application/json; charset=utf-8",
                  "Host": "localhost:5984"
              },
              "body": "",
              "peer": "127.0.0.1",
              "form": {},
              "cookie": {},
              "userCtx": {
                  "db": "test",
                  "name": null,
                  "roles": [
                      "_admin"
                  ]
              },
              "secObj": {}
          }
      ]
  ]

The Query Server answers::

  [
    true,
    [
      true,
      false
    ]
  ]



.. _qs/ddoc/views:

``views``
---------

:Command: ``ddoc``
:SubCommand: ``views``
:Arguments: Array of document objects
:Returns: Array of two elements:

  - ``true``
  - Array of booleans in the same order of input documents.

.. versionadded:: 1.2

Executes :ref:`view function <viewfilter>` in place of the filter.

Acts in the same way as :ref:`qs/ddoc/filters` command.

.. _qs/ddoc/validate_doc_update:

``validate_doc_update``
-----------------------

:Command: ``ddoc``
:SubCommand: ``validate_doc_update``
:Arguments:

  - Document object that will be stored
  - Document object that will be replaced
  - :ref:`userctx_object`
  - :ref:`security_object`

:Returns: ``1``

Executes :ref:`validation function <vdufun>`.

CouchDB send::

  [
    "ddoc",
    "_design/id",
    ["validate_doc_update"],
    [
      {
        "_id": "docid",
        "_rev": "2-e0165f450f6c89dc6b071c075dde3c4d",
        "score": 10
      },
      {
        "_id": "docid",
        "_rev": "1-9f798c6ad72a406afdbf470b9eea8375",
        "score": 4
      },
      {
        "name": "Mike",
        "roles": ["player"]
      },
      {
        "admins": {},
        "members": []
      }
    ]
  ]

The Query Server answers::

  1

.. note::

   While the only valid response for this command is ``true`` to prevent
   document save the Query Server need to raise an error: ``forbidden`` or
   ``unauthorized`` - these errors will be turned into correct ``HTTP 403`` and
   ``HTTP 401`` responses respectively.


.. _qs/errors:

Raising errors
==============

When something went wrong the Query Server is able to inform CouchDB about
such a situation by sending special message in response of received command.

Error messages prevent further command execution and return an error description
to CouchDB. All errors are logically divided into two groups:

- `Common errors`. These errors only break the current Query Server command and
  return the error info to the CouchDB instance *without* terminating the Query
  Server  process.
- `Fatal errors`. The fatal errors signal about something really bad that hurts
  the overall Query Server process stability and productivity. For instance, if
  you're using Python Query Server and some design function is unable to import
  some third party module, it's better to count such error as fatal and
  terminate whole process or you still have to do the same after import fixing,
  but manually.

.. _qs/error:

``error``
---------

To raise an error, the Query Server have to answer::

  ["error", "error_name", "reason why"]

The ``"error_name"`` helps to classify problems by their type e.g. if it's
``"value_error"`` so probably user have entered wrong data, ``"not_found"``
notifies about missed resource and ``"type_error"`` definitely says about
invalid and non expected input from user.

The ``"reason why"`` is the error message that explains why it raised and, if
possible, what is needed to do to fix it.

For example, calling :ref:`updatefun` against non existent document could produce
next error message::

  ["error", "not_found", "Update function requires existent document"]


.. _qs/error/forbidden:

``forbidden``
-------------

The `forbidden` error is widely used by :ref:`vdufun` to stop further function
processing and prevent on disk store of the new document version. Since this
error actually is not an error, but an assertion against user actions, CouchDB
doesn't log it at `"error"` level, but returns `HTTP 403 Forbidden` response
with error information object.

To raise this error, the Query Server have to answer::

  {"forbidden": "reason why"}


.. _qs/error/unauthorized:

``unauthorized``
----------------

The `unauthorized` error mostly acts like `forbidden` one, but with
the meaning of *please authorize first*. This small difference helps end users
to understand what they can do to solve the problem. CouchDB doesn't log it at
`"error"` level, but returns `HTTP 401 Unauthorized` response with error
information object.

To raise this error, the Query Server have to answer::

  {"unauthorized": "reason why"}

.. _qs/log:

Logging
=======

At any time, the Query Server may send some information that will be saved in
CouchDB's log file. This is done by sending a special object with just one
field, log, on a separate line::

  ["log", "some message"]

CouchDB responds nothing, but writes received message into log file::

  [Sun, 13 Feb 2009 23:31:30 GMT] [info] [<0.72.0>] Query Server Log Message: some message

These messages are only logged at :ref:`info level <config/log/level>`.
