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


.. _api/ddoc/view:

``/db/_design/design-doc/_view/view-name``
==========================================

.. http:get:: /{db}/_design/{ddoc}/_view/{view}

  Executes the specified view function from the specified design document.

  :param db: Database name
  :param ddoc: Design document name
  :param view: View function name

  :<header Accept: - :mimetype:`application/json`
                   - :mimetype:`text/plain`

  :query boolean conflicts: Includes `conflicts` information in response.
    Ignored if `include_docs` isn't ``true``. Default is ``false``
  :query boolean descending: Return the documents in descending by key order.
    Default is ``false``
  :query string endkey: Stop returning records when the specified key is
    reached. *Optional*
  :query string end_key: Alias for `endkey` param
  :query string endkey_docid: Stop returning records when the specified
    document ID is reached. *Optional*
  :query string end_key_doc_id: Alias for `endkey_docid` param
  :query boolean group: Group the results using the reduce function to a group
    or single row. Default is ``false``
  :query number group_level: Specify the group level to be used. *Optional*
  :query boolean include_docs: Include the full content of the documents in
    the return. Default is ``false``
  :query boolean inclusive_end: Specifies whether the specified end key should
    be included in the result. Default is ``true``
  :query string key: Return only documents that match the specified key.
    *Optional*
  :query number limit: Limit the number of the returned documents to the
    specified number. *Optional*
  :query boolean reduce: Use the reduction function. Default is ``true``
  :query number skip: Skip this number of records before starting to return
    the results. Default is ``0``
  :query string stale: Allow the results from a stale view to be used.
    Supported values: ``ok`` and ``update_after``. *Optional*
  :query string startkey: Return records starting with the specified key.
    *Optional*
  :query string start_key: Alias for `startkey` param
  :query string startkey_docid: Return records starting with the specified
    document ID. *Optional*
  :query string start_key_doc_id: Alias for `startkey_docid` param
  :query boolean update_seq: Response includes an ``update_seq`` value
    indicating which sequence id of the database the view reflects.
    Default is ``false``

  :>header Content-Type: - :mimetype:`application/json`
                         - :mimetype:`text/plain; charset=utf-8`
  :>header ETag: Response signature
  :>header Transfer-Encoding: ``chunked``

  :>json number offset: Offset where the document list started
  :>json array rows: Array of view row objects. By default the information
    returned contains only the document ID and revision
  :>json number total_rows: Number of documents in the database/view
  :>json number update_seq: Current update sequence for the database

  :code 200: Request completed successfully
  :code 400: Invalid request
  :code 401: Read permission required
  :code 404: Specified database, design document or view is missed
  :code 500: View function execution error

  **Request**:

  .. code-block:: http

    GET /recipes/_design/ingredients/_view/by_name HTTP/1.1
    Accept: application/json
    Host: localhost:5984

  **Response**:

  .. code-block:: http

    HTTP/1.1 200 OK
    Cache-Control: must-revalidate
    Content-Type: application/json
    Date: Wed, 21 Aug 2013 09:12:06 GMT
    ETag: "2FOLSBSW4O6WB798XU4AQYA9B"
    Server: CouchDB (Erlang/OTP)
    Transfer-Encoding: chunked

    {
        "offset": 0,
        "rows": [
            {
                "id": "SpaghettiWithMeatballs",
                "key": "meatballs",
                "value": 1
            },
            {
                "id": "SpaghettiWithMeatballs",
                "key": "spaghetti",
                "value": 1
            },
            {
                "id": "SpaghettiWithMeatballs",
                "key": "tomato sauce",
                "value": 1
            }
        ],
        "total_rows": 3
    }


.. http:post:: /{db}/_design/{ddoc}/_view/{view}

  Executes the specified view function from the specified design document.
  Unlike the :get:`/{db}/_design/{ddoc}/_view/{view}`  method
  for accessing views, the :method:`POST` method supports the specification
  of explicit keys to be retrieved from the view results. The remainder of the
  :method:`POST` view functionality is identical to the
  :get:`/{db}/_design/{ddoc}/_view/{view}` API.

  **Request**:

  .. code-block:: http

    POST /recipes/_design/ingredients/_view/by_name HTTP/1.1
    Accept: application/json
    Content-Length: 37
    Host: localhost:5984

    {
        "keys": [
            "meatballs",
            "spaghetti"
        ]
    }

  **Response**:

  .. code-block:: http

    HTTP/1.1 200 OK
    Cache-Control: must-revalidate
    Content-Type: application/json
    Date: Wed, 21 Aug 2013 09:14:13 GMT
    ETag: "6R5NM8E872JIJF796VF7WI3FZ"
    Server: CouchDB (Erlang/OTP)
    Transfer-Encoding: chunked

    {
        "offset": 0,
        "rows": [
            {
                "id": "SpaghettiWithMeatballs",
                "key": "meatballs",
                "value": 1
            },
            {
                "id": "SpaghettiWithMeatballs",
                "key": "spaghetti",
                "value": 1
            }
        ],
        "total_rows": 3
    }


.. _api/ddoc/view/options:

View Options
------------

There are two view indexing options that can be defined in a design document
as boolean properties of an ``options`` object. Unlike the others querying
options, these aren't URL parameters because they take effect when the view
index is generated, not when it's accessed:

- **local_seq** (*boolean*): Makes documents' local sequence numbers available
  to map functions (as a ``_local_seq`` document property)
- **include_design** (*boolean*): Allows map functions to be called on design
  documents as well as regular documents

In additional to these options, you may specify :ref:`any other <api/ddoc/view>`
with their default value. E.g. having option ``"include_docs": true`` will
automatically includes document body for view results response. You still may
override such by explicitly defining same query parameter name with other value.

.. _api/ddoc/view/indexing:

Querying Views and Indexes
--------------------------

The definition of a view within a design document also creates an index
based on the key information defined within each view. The production
and use of the index significantly increases the speed of access and
searching or selecting documents from the view.

However, the index is not updated when new documents are added or
modified in the database. Instead, the index is generated or updated,
either when the view is first accessed, or when the view is accessed
after a document has been updated. In each case, the index is updated
before the view query is executed against the database.

View indexes are updated incrementally in the following situations:

-  A new document has been added to the database.

-  A document has been deleted from the database.

-  A document in the database has been updated.

View indexes are rebuilt entirely when the view definition changes. To
achieve this, a 'fingerprint' of the view definition is created when the
design document is updated. If the fingerprint changes, then the view
indexes are entirely rebuilt. This ensures that changes to the view
definitions are reflected in the view indexes.

.. note::
   View index rebuilds occur when one view from the same the view group
   (i.e. all the views defined within a single a design document) has
   been determined as needing a rebuild. For example, if if you have a
   design document with different views, and you update the database,
   all three view indexes within the design document will be updated.

Because the view is updated when it has been queried, it can result in a
delay in returned information when the view is accessed, especially if
there are a large number of documents in the database and the view index
does not exist. There are a number of ways to mitigate, but not
completely eliminate, these issues. These include:

-  Create the view definition (and associated design documents) on your
   database before allowing insertion or updates to the documents. If
   this is allowed while the view is being accessed, the index can be
   updated incrementally.

-  Manually force a view request from the database. You can do this
   either before users are allowed to use the view, or you can access
   the view manually after documents are added or updated.

-  Use the :ref:`changes feed <api/db/changes>` to monitor for changes to the
   database and then access the view to force the corresponding view
   index to be updated.

-  Use a monitor with the :ref:`update notification <update-notifications>`
   section of the CouchDB configuration file to monitor for changes to your
   database, and trigger a view query to force the view to be updated.

None of these can completely eliminate the need for the indexes to be
rebuilt or updated when the view is accessed, but they may lessen the
effects on end-users of the index update affecting the user experience.

Another alternative is to allow users to access a 'stale' version of the
view index, rather than forcing the index to be updated and displaying
the updated results. Using a stale view may not return the latest
information, but will return the results of the view query using an
existing version of the index.

For example, to access the existing stale view ``by_recipe`` in the
``recipes`` design document:

.. code-block:: text

    http://localhost:5984/recipes/_design/recipes/_view/by_recipe?stale=ok

Accessing a stale view:

-  Does not trigger a rebuild of the view indexes, even if there have
   been changes since the last access.

-  Returns the current version of the view index, if a current version
   exists.

-  Returns an empty result set if the given view index does exist.

As an alternative, you use the ``update_after`` value to the ``stale``
parameter. This causes the view to be returned as a stale view, but for
the update process to be triggered after the view information has been
returned to the client.

In addition to using stale views, you can also make use of the
``update_seq`` query argument. Using this query argument generates the
view information including the update sequence of the database from
which the view was generated. The returned value can be compared this to
the current update sequence exposed in the database information
(returned by :get:`/{db}`).


.. _api/ddoc/view/sorting:

Sorting Returned Rows
---------------------

Each element within the returned array is sorted using native UTF-8
sorting according to the contents of the key portion of the emitted
content. The basic order of output is as follows:

-  ``null``

-  ``false``

-  ``true``

-  Numbers

-  Text (case sensitive, lowercase first)

-  Arrays (according to the values of each element, in order)

-  Objects (according to the values of keys, in key order)

**Request**:

.. code-block:: http

  GET /db/_design/test/_view/sorting HTTP/1.1
  Accept: application/json
  Host: localhost:5984


**Response**:

.. code-block:: http

  HTTP/1.1 200 OK
  Cache-Control: must-revalidate
  Content-Type: application/json
  Date: Wed, 21 Aug 2013 10:09:25 GMT
  ETag: "8LA1LZPQ37B6R9U8BK9BGQH27"
  Server: CouchDB (Erlang/OTP)
  Transfer-Encoding: chunked
  
  {
      "offset": 0, 
      "rows": [
          {
              "id": "dummy-doc", 
              "key": null, 
              "value": null
          }, 
          {
              "id": "dummy-doc", 
              "key": false, 
              "value": null
          }, 
          {
              "id": "dummy-doc", 
              "key": true, 
              "value": null
          }, 
          {
              "id": "dummy-doc", 
              "key": 0, 
              "value": null
          }, 
          {
              "id": "dummy-doc", 
              "key": 1, 
              "value": null
          }, 
          {
              "id": "dummy-doc", 
              "key": 10, 
              "value": null
          }, 
          {
              "id": "dummy-doc", 
              "key": 42, 
              "value": null
          }, 
          {
              "id": "dummy-doc", 
              "key": "10", 
              "value": null
          }, 
          {
              "id": "dummy-doc", 
              "key": "hello", 
              "value": null
          }, 
          {
              "id": "dummy-doc", 
              "key": "Hello", 
              "value": null
          }, 
          {
              "id": "dummy-doc", 
              "key": "\u043f\u0440\u0438\u0432\u0435\u0442",
              "value": null
          }, 
          {
              "id": "dummy-doc", 
              "key": [], 
              "value": null
          }, 
          {
              "id": "dummy-doc", 
              "key": [
                  1, 
                  2, 
                  3
              ], 
              "value": null
          }, 
          {
              "id": "dummy-doc", 
              "key": [
                  2, 
                  3
              ], 
              "value": null
          }, 
          {
              "id": "dummy-doc", 
              "key": [
                  3
              ], 
              "value": null
          }, 
          {
              "id": "dummy-doc", 
              "key": {}, 
              "value": null
          }, 
          {
              "id": "dummy-doc", 
              "key": {
                  "foo": "bar"
              }, 
              "value": null
          }
      ], 
      "total_rows": 17
  }


You can reverse the order of the returned view information by using the
``descending`` query value set to true:

**Request**:

.. code-block:: http

  GET /db/_design/test/_view/sorting?descending=true HTTP/1.1
  Accept: application/json
  Host: localhost:5984


**Response**:

.. code-block:: http

  HTTP/1.1 200 OK
  Cache-Control: must-revalidate
  Content-Type: application/json
  Date: Wed, 21 Aug 2013 10:09:25 GMT
  ETag: "Z4N468R15JBT98OM0AMNSR8U"
  Server: CouchDB (Erlang/OTP)
  Transfer-Encoding: chunked
  
  {
      "offset": 0,
      "rows": [
          {
              "id": "dummy-doc",
              "key": {
                  "foo": "bar"
              },
              "value": null
          },
          {
              "id": "dummy-doc",
              "key": {},
              "value": null
          },
          {
              "id": "dummy-doc",
              "key": [
                  3
              ],
              "value": null
          },
          {
              "id": "dummy-doc",
              "key": [
                  2,
                  3
              ],
              "value": null
          },
          {
              "id": "dummy-doc",
              "key": [
                  1,
                  2,
                  3
              ],
              "value": null
          },
          {
              "id": "dummy-doc",
              "key": [],
              "value": null
          },
          {
              "id": "dummy-doc",
              "key": "\u043f\u0440\u0438\u0432\u0435\u0442",
              "value": null
          },
          {
              "id": "dummy-doc",
              "key": "Hello",
              "value": null
          },
          {
              "id": "dummy-doc",
              "key": "hello",
              "value": null
          },
          {
              "id": "dummy-doc",
              "key": "10",
              "value": null
          },
          {
              "id": "dummy-doc",
              "key": 42,
              "value": null
          },
          {
              "id": "dummy-doc",
              "key": 10,
              "value": null
          },
          {
              "id": "dummy-doc",
              "key": 1,
              "value": null
          },
          {
              "id": "dummy-doc",
              "key": 0,
              "value": null
          },
          {
              "id": "dummy-doc",
              "key": true,
              "value": null
          },
          {
              "id": "dummy-doc",
              "key": false,
              "value": null
          },
          {
              "id": "dummy-doc",
              "key": null,
              "value": null
          }
      ],
      "total_rows": 17
  }


Sorting order and startkey/endkey
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The sorting direction is applied before the filtering applied using the
``startkey`` and ``endkey`` query arguments. For example the following
query:

.. code-block:: http

    GET http://couchdb:5984/recipes/_design/recipes/_view/by_ingredient?startkey=%22carrots%22&endkey=%22egg%22
    Accept: application/json

will operate correctly when listing all the matching entries between
``carrots`` and ``egg``. If the order of output is reversed with the
``descending`` query argument, the view request will return no entries:

.. code-block:: http


  GET /recipes/_design/recipes/_view/by_ingredient?descending=true&startkey=%22carrots%22&endkey=%22egg%22 HTTP/1.1
  Accept: application/json
  Host: localhost:5984

  {
     "total_rows" : 26453,
     "rows" : [],
     "offset" : 21882
  }

The results will be empty because the entries in the view are reversed
before the key filter is applied, and therefore the ``endkey`` of “egg”
will be seen before the ``startkey`` of “carrots”, resulting in an empty
list.

Instead, you should reverse the values supplied to the ``startkey`` and
``endkey`` parameters to match the descending sorting applied to the
keys. Changing the previous example to:

.. code-block:: http

  GET /recipes/_design/recipes/_view/by_ingredient?descending=true&startkey=%22egg%22&endkey=%22carrots%22 HTTP/1.1
  Accept: application/json
  Host: localhost:5984


.. _api/ddoc/view/sorting/raw:

Raw collation
^^^^^^^^^^^^^

By default CouchDB using `ICU`_ driver for sorting view results. It's possible
use binary collation instead for faster view builds where Unicode collation is
not important.

To use raw collation add ``"collation": "raw"`` key-value pair to the design
documents ``options`` object at the root level. After that, views will be
regenerated and new order applied.

.. seealso::

   :ref:`views/collation`

.. _ICU: http://site.icu-project.org/

.. _api/ddoc/view/limiting:

Using Limits and Skipping Rows
------------------------------

By default requestion views result returns all records for it. That's ok when
they are small, but this may lead to problems when there are billions of them
since the clients might have to read them all and consume all available memory.

But it's possible to reduce output result rows by specifying ``limit`` query
parameter. For example, retrieving the list of recipes using the ``by_title``
view and limited to 5 returns only 5 records, while there are total 2667 records
in view:

**Request**:

.. code-block:: http

  GET /recipes/_design/recipes/_view/by_title?limit=5 HTTP/1.1
  Accept: application/json
  Host: localhost:5984

**Response**:

.. code-block:: http

  HTTP/1.1 200 OK
  Cache-Control: must-revalidate
  Content-Type: application/json
  Date: Wed, 21 Aug 2013 09:14:13 GMT
  ETag: "9Q6Q2GZKPH8D5F8L7PB6DBSS9"
  Server: CouchDB (Erlang/OTP)
  Transfer-Encoding: chunked

  {
     "offset" : 0,
     "rows" : [
        {
           "id" : "3-tiersalmonspinachandavocadoterrine",
           "key" : "3-tier salmon, spinach and avocado terrine",
           "value" : [
              null,
              "3-tier salmon, spinach and avocado terrine"
           ]
        },
        {
           "id" : "Aberffrawcake",
           "key" : "Aberffraw cake",
           "value" : [
              null,
              "Aberffraw cake"
           ]
        },
        {
           "id" : "Adukiandorangecasserole-microwave",
           "key" : "Aduki and orange casserole - microwave",
           "value" : [
              null,
              "Aduki and orange casserole - microwave"
           ]
        },
        {
           "id" : "Aioli-garlicmayonnaise",
           "key" : "Aioli - garlic mayonnaise",
           "value" : [
              null,
              "Aioli - garlic mayonnaise"
           ]
        },
        {
           "id" : "Alabamapeanutchicken",
           "key" : "Alabama peanut chicken",
           "value" : [
              null,
              "Alabama peanut chicken"
           ]
        }
     ],
     "total_rows" : 2667
  }

To omit some records you may use ``skip`` query parameter:

**Request**:

.. code-block:: http

  GET /recipes/_design/recipes/_view/by_title?limit=3&skip=2 HTTP/1.1
  Accept: application/json
  Host: localhost:5984

**Response**:

.. code-block:: http

  HTTP/1.1 200 OK
  Cache-Control: must-revalidate
  Content-Type: application/json
  Date: Wed, 21 Aug 2013 09:14:13 GMT
  ETag: "H3G7YZSNIVRRHO5FXPE16NJHN"
  Server: CouchDB (Erlang/OTP)
  Transfer-Encoding: chunked

  {
     "offset" : 2,
     "rows" : [
        {
           "id" : "Adukiandorangecasserole-microwave",
           "key" : "Aduki and orange casserole - microwave",
           "value" : [
              null,
              "Aduki and orange casserole - microwave"
           ]
        },
        {
           "id" : "Aioli-garlicmayonnaise",
           "key" : "Aioli - garlic mayonnaise",
           "value" : [
              null,
              "Aioli - garlic mayonnaise"
           ]
        },
        {
           "id" : "Alabamapeanutchicken",
           "key" : "Alabama peanut chicken",
           "value" : [
              null,
              "Alabama peanut chicken"
           ]
        }
     ],
     "total_rows" : 2667
  }

.. warning::

   Using ``limit`` and ``skip`` parameters is not recommended for results
   pagination. Read :ref:`pagination recipe <views/pagination>` why it's so
   and how to make it better.
