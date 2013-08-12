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


.. _api/db/all_docs:

``/db/_all_docs``
=================

.. http:get:: /{db}/_all_docs

  Returns a JSON structure of all of the documents in a given database.
  The information is returned as a JSON structure containing meta
  information about the return structure, and the list documents and basic
  contents, consisting the ID, revision and key. The key is generated from
  the document ID.

  :param db: Database name
  :<header Accept: - :mimetype:`application/json`
                   - :mimetype:`text/plain`
  :query boolean conflicts: Includes `conflicts` information in response.
    Ignored if `include_docs` isn't ``true``. Default is ``false``.
  :query boolean descending: Return the documents in descending by key order.
    Default is ``false``.
  :query string endkey: Stop returning records when the specified key is
    reached. *Optional*.
  :query string end_key: Alias for `endkey` param.
  :query string endkey_docid: Stop returning records when the specified
    document ID is reached. *Optional*.
  :query string end_key_doc_id: Alias for `endkey_docid` param.
  :query boolean group: Group the results using the reduce function to a group
    or single row. Default is ``false``.
  :query number group_level: Specify the group level to be used. *Optional*.
  :query boolean include_docs: Include the full content of the documents in
    the return. Default is ``false``.
  :query boolean inclusive_end: Specifies whether the specified end key should
    be included in the result. Default is ``true``.
  :query string key: Return only documents that match the specified key.
    *Optional*.
  :query number limit: Limit the number of the returned documents to the
    specified number. *Optional*.
  :query boolean reduce: Use the reduction function. Default is ``true``.
  :query number skip: Skip this number of records before starting to return
    the results. Default is ``0``.
  :query string stale: Allow the results from a stale view to be used.
    Supported values: ``ok`` and ``update_after``. *Optional*.
  :query string startkey: Return records starting with the specified key.
    *Optional*.
  :query string start_key: Alias for `startkey` param.
  :query string startkey_docid: Return records starting with the specified
    document ID. *Optional*.
  :query string start_key_doc_id: Alias for `startkey_docid` param.
  :query boolean update_seq: Response includes an ``update_seq`` value
    indicating which sequence id of the database the view reflects.
    Default is ``false``.
  :>header Content-Type: - :mimetype:`application/json`
                         - :mimetype:`text/plain; charset=utf-8`
  :>header ETag: Response signature
  :>json number offset: Offset where the document list started
  :>json array rows: Array of view row objects. By default the information
    returned contains only the document ID and revision.
  :>json number total_rows: Number of documents in the database/view
  :>json number update_seq: Current update sequence for the database
  :code 200: Request completed successfully

  **Request**:

  .. code-block:: http

    GET /db/_all_docs HTTP/1.1
    Accept: application/json
    Host: localhost:5984

  **Response**:

  .. code-block:: http

    HTTP/1.1 200 OK
    Cache-Control: must-revalidate
    Content-Type: application/json
    Date: Sat, 10 Aug 2013 16:22:56 GMT
    ETag: "1W2DJUZFZSZD9K78UFA3GZWB4"
    Server: CouchDB/1.4.0 (Erlang OTP/R16B)
    Transfer-Encoding: chunked

    {
        "offset": 0,
        "rows": [
            {
                "id": "16e458537602f5ef2a710089dffd9453",
                "key": "16e458537602f5ef2a710089dffd9453",
                "value": {
                    "rev": "1-967a00dff5e02add41819138abb3284d"
                }
            },
            {
                "id": "a4c51cdfa2069f3e905c431114001aff",
                "key": "a4c51cdfa2069f3e905c431114001aff",
                "value": {
                    "rev": "1-967a00dff5e02add41819138abb3284d"
                }
            },
            {
                "id": "a4c51cdfa2069f3e905c4311140034aa",
                "key": "a4c51cdfa2069f3e905c4311140034aa",
                "value": {
                    "rev": "5-6182c9c954200ab5e3c6bd5e76a1549f"
                }
            },
            {
                "id": "a4c51cdfa2069f3e905c431114003597",
                "key": "a4c51cdfa2069f3e905c431114003597",
                "value": {
                    "rev": "2-7051cbe5c8faecd085a3fa619e6e6337"
                }
            },
            {
                "id": "f4ca7773ddea715afebc4b4b15d4f0b3",
                "key": "f4ca7773ddea715afebc4b4b15d4f0b3",
                "value": {
                    "rev": "2-7051cbe5c8faecd085a3fa619e6e6337"
                }
            }
        ],
        "total_rows": 5
    }


.. http:post:: /{db}/_all_docs

  The ``POST`` to ``_all_docs`` allows to specify multiple keys to be
  selected from the database. This enables you to request multiple
  documents in a single request, in place of multiple :ref:`api/doc.get`
  requests.

  The request body should contain a list of the keys to be returned as an
  array to a ``keys`` object. For example:

  .. code-block:: http

    POST /db/_all_docs HTTP/1.1
    Accept: application/json
    Content-Length: 70
    Content-Type: application/json
    Host: localhost:5984

    {
       "keys" : [
          "Zingylemontart",
          "Yogurtraita"
       ]
    }

  The return JSON is the all documents structure, but with only the
  selected keys in the output:

  .. code-block:: javascript

      {
         "total_rows" : 2666,
         "rows" : [
            {
               "value" : {
                  "rev" : "1-a3544d296de19e6f5b932ea77d886942"
               },
               "id" : "Zingylemontart",
               "key" : "Zingylemontart"
            },
            {
               "value" : {
                  "rev" : "1-91635098bfe7d40197a1b98d7ee085fc"
               },
               "id" : "Yogurtraita",
               "key" : "Yogurtraita"
            }
         ],
         "offset" : 0
      }


.. _api/db/bulk_docs:

``/db/_bulk_docs``
==================

.. http:post:: /{db}/_bulk_docs

  The bulk document API allows you to create and update multiple documents
  at the same time within a single request. The basic operation is similar
  to creating or updating a single document, except that you batch the
  document structure and information.

  When creating new documents the document ID (``_id``) is optional.

  For updating existing documents, you must provide the document ID, revision
  information (``_rev``), and new document values.

  In case of batch deleting documents all fields as document ID, revision
  information and deletion status (``_deleted``) are required.

  :param db: Database name
  :<header Accept: - :mimetype:`application/json`
                   - :mimetype:`text/plain`
  :<header Content-Type: :mimetype:`application/json`
  :<header X-Couch-Full-Commit: Overrides server's
    :ref:`commit policy <config/couchdb/delayed_commits>`. *Optional*.
    Default is ``true``.
  :<json boolean all_or_nothing: Sets the database commit mode to use
    `all-or-nothing` semantics. Default is ``false``. *Optional*.
  :<json array docs: List of documents objects
  :<json boolean new_edits: Default is ``false``. *Optional*.
  :>header Content-Type: - :mimetype:`application/json`
                         - :mimetype:`text/plain; charset=utf-8`
  :>jsonarr string id: Document ID
  :>jsonarr string rev: New document revision token. *Optional*. Available
    if document have saved without errors.
  :>jsonarr string error: Error type. *Optional*.
  :>jsonarr string reason: Error reason. *Optional*.
  :code 201: Document(s) have been created or updated
  :code 400: Invalid request`s JSON data
  :code 500: Malformed data provided

  **Request**:

  .. code-block:: http

    POST /db/_bulk_docs HTTP/1.1
    Accept: application/json
    Content-Length: 109
    Content-Type:application/json
    Host: localhost:5984

    {
      "docs": [
        {
          "_id": "FishStew"
        },
        {
          "_id": "LambStew",
          "_rev": "2-0786321986194c92dd3b57dfbfc741ce",
          "_deleted": true
        }
      ]
    }

  **Response**:

  .. code-block:: http

    HTTP/1.1 201 Created
    Cache-Control: must-revalidate
    Content-Length: 144
    Content-Type: appliaction/json
    Date: Mon, 12 Aug 2013 00:15:05 GMT
    Server: CouchDB/1.4.0 (Erlang OTP/R16B)

    [
      {
        "ok": true,
        "id": "FishStew",
        "rev":" 1-967a00dff5e02add41819138abb3284d"
      },
      {
        "ok": true,
        "id": "LambStew",
        "rev": "3-f9c62b2169d0999103e9f41949090807"
      }
    ]


Inserting Documents in Bulk
---------------------------

To insert documents in bulk into a database you need to supply a JSON
structure with the array of documents that you want to add to the
database. Using this method you can either include a document ID, or
allow the document ID to be automatically generated.

For example, the following inserts three new documents, two with the
supplied document IDs, and one which will have a document ID generated:

.. code-block:: javascript

    {
       "docs" : [
          {
             "_id" : "FishStew",
             "servings" : 4,
             "subtitle" : "Delicious with fresh bread",
             "title" : "Fish Stew"
          },
          {
             "_id" : "LambStew",
             "servings" : 6,
             "subtitle" : "Delicious with scone topping",
             "title" : "Lamb Stew"
          },
          {
             "servings" : 8,
             "subtitle" : "Delicious with suet dumplings",
             "title" : "Beef Stew"
          },
       ]
    }


The return type from a bulk insertion will be 201, with the content of
the returned structure indicating specific success or otherwise messages
on a per-document basis.

The return structure from the example above contains a list of the
documents created, here with the combination and their revision IDs:

.. code-block:: http

    POST http://couchdb:5984/recipes/_bulk_docs
    Content-Type: application/json

    [
       {
          "id" : "FishStew",
          "rev" : "1-9c65296036141e575d32ba9c034dd3ee",
       },
       {
          "id" : "LambStew",
          "rev" : "1-34c318924a8f327223eed702ddfdc66d",
       },
       {
          "id" : "7f7638c86173eb440b8890839ff35433",
          "rev" : "1-857c7cbeb6c8dd1dd34a0c73e8da3c44",
       }
    ]


The content and structure of the returned JSON will depend on the transaction
semantics being used for the bulk update; see :ref:`api/db/bulk_docs/semantics`
for more information. Conflicts and validation errors when updating documents in
bulk must be handled separately; see :ref:`api/db/bulk_docs/validation`.

Updating Documents in Bulk
--------------------------

The bulk document update procedure is similar to the insertion
procedure, except that you must specify the document ID and current
revision for every document in the bulk update JSON string.

For example, you could send the following request:

.. code-block:: http

    POST http://couchdb:5984/recipes/_bulk_docs
    Content-Type: application/json

    {
       "docs" : [
          {
             "_id" : "FishStew",
             "_rev" : "1-9c65296036141e575d32ba9c034dd3ee",
             "servings" : 4,
             "subtitle" : "Delicious with freshly baked bread",
             "title" : "Fish Stew"
          },
          {
             "_id" : "LambStew",
             "_rev" : "1-34c318924a8f327223eed702ddfdc66d",
             "servings" : 6,
             "subtitle" : "Serve with a wholemeal scone topping",
             "title" : "Lamb Stew"
          },
          {
             "_id" : "7f7638c86173eb440b8890839ff35433"
             "_rev" : "1-857c7cbeb6c8dd1dd34a0c73e8da3c44",
             "servings" : 8,
             "subtitle" : "Hand-made dumplings make a great accompaniment",
             "title" : "Beef Stew"
          }
       ]
    }

The return structure is the JSON of the updated documents, with the new
revision and ID information:

.. code-block:: javascript

    [
       {
          "id" : "FishStew",
          "rev" : "2-e7af4c4e9981d960ecf78605d79b06d1"
       },
       {
          "id" : "LambStew",
          "rev" : "2-0786321986194c92dd3b57dfbfc741ce"
       },
       {
          "id" : "7f7638c86173eb440b8890839ff35433",
          "rev" : "2-bdd3bf3563bee516b96885a66c743f8e"
       }
    ]

You can optionally delete documents during a bulk update by adding the
``_deleted`` field with a value of ``true`` to each document ID/revision
combination within the submitted JSON structure.

The return type from a bulk insertion will be :code:`201`, with the content of
the returned structure indicating specific success or otherwise messages
on a per-document basis.

The content and structure of the returned JSON will depend on the transaction
semantics being used for the bulk update; see :ref:`api/db/bulk_docs/semantics`
for more information. Conflicts and validation errors when updating documents in
bulk must be handled separately; see :ref:`api/db/bulk_docs/validation`.

.. _api/db/bulk_docs/semantics:

Bulk Documents Transaction Semantics
------------------------------------

CouchDB supports two different modes for updating (or inserting)
documents using the bulk documentation system. Each mode affects both
the state of the documents in the event of system failure, and the level
of conflict checking performed on each document. The two modes are:

-  ``non-atomic``

   The default mode is non-atomic, that is, CouchDB will only guarantee
   that some of the documents will be saved when you send the request.
   The response will contain the list of documents successfully inserted
   or updated during the process. In the event of a crash, some of the
   documents may have been successfully saved, and some will have been
   lost.

   In this mode, the response structure will indicate whether the
   document was updated by supplying the new ``_rev`` parameter
   indicating a new document revision was created. If the update failed,
   then you will get an ``error`` of type ``conflict``. For example:

   .. code-block:: javascript

       [
          {
             "id" : "FishStew",
             "error" : "conflict",
             "reason" : "Document update conflict."
          },
          {
             "id" : "LambStew",
             "error" : "conflict",
             "reason" : "Document update conflict."
          },
          {
             "id" : "7f7638c86173eb440b8890839ff35433",
             "error" : "conflict",
             "reason" : "Document update conflict."
          }
       ]


   In this case no new revision has been created and you will need to
   submit the document update, with the correct revision tag, to update
   the document.

-  ``all-or-nothing``

   In all-or-nothing mode, either all documents are written to the
   database, or no documents are written to the database, in the event
   of a system failure during commit.

   In addition, the per-document conflict checking is not performed.
   Instead a new revision of the document is created, even if the new
   revision is in conflict with the current revision in the database.
   The returned structure contains the list of documents with new
   revisions:

   .. code-block:: javascript

       [
          {
             "id" : "FishStew",
             "rev" : "2-e7af4c4e9981d960ecf78605d79b06d1"
          },
          {
             "id" : "LambStew",
             "rev" : "2-0786321986194c92dd3b57dfbfc741ce"
          },
          {
             "id" : "7f7638c86173eb440b8890839ff35433",
             "rev" : "2-bdd3bf3563bee516b96885a66c743f8e"
          }
       ]

   When updating documents using this mode the revision of a document
   included in views will be arbitrary. You can check the conflict
   status for a document by using the ``conflicts=true`` query argument
   when accessing the view. Conflicts should be handled individually to
   ensure the consistency of your database.

   To use this mode, you must include the ``all_or_nothing`` field (set
   to true) within the main body of the JSON of the request.

The effects of different database operations on the different modes are
summarized below:

* **Transaction Mode**: ``Non-atomic``

  * **Transaction**: ``Insert``

    * **Cause**: Requested document ID already exists
    * **Resolution**: Resubmit with different document ID, or update the
      existing document

  * **Transaction**: ``Update``

    * **Cause**: Revision missing or incorrect
    * **Resolution**: Resubmit with correct revision

* **Transaction Mode**: ``All-or-nothing``

  * **Transaction**: ``Insert`` / ``Update``

    * **Cause**: Additional revision inserted
    * **Resolution**: Resolve conflicted revisions

Replication of documents is independent of the type of insert or update.
The documents and revisions created during a bulk insert or update are
replicated in the same way as any other document. This can mean that if
you make use of the all-or-nothing mode the exact list of documents,
revisions (and their conflict state) may or may not be replicated to
other databases correctly.

.. _api/db/bulk_docs/validation:

Bulk Document Validation and Conflict Errors
--------------------------------------------

The JSON returned by the ``_bulk_docs`` operation consists of an array
of JSON structures, one for each document in the original submission.
The returned JSON structure should be examined to ensure that all of the
documents submitted in the original request were successfully added to
the database.

When a document (or document revision) is not correctly committed to the
database because of an error, you should check the ``error`` field to
determine error type and course of action. Errors will be one of the
following type:

-  ``conflict``

   The document as submitted is in conflict. If you used the default
   bulk transaction mode then the new revision will not have been
   created and you will need to re-submit the document to the database.
   If you used ``all-or-nothing`` mode then you will need to manually
   resolve the conflicted revisions of the document.

   Conflict resolution of documents added using the bulk docs interface
   is identical to the resolution procedures used when resolving
   conflict errors during replication.

-  ``forbidden``

   Entries with this error type indicate that the validation routine
   applied to the document during submission has returned an error.

   For example, if your validation routine includes the following:

   .. code-block:: javascript

        throw({forbidden: 'invalid recipe ingredient'});

   The error returned will be:

   .. code-block:: javascript

       {
          "id" : "7f7638c86173eb440b8890839ff35433",
          "error" : "forbidden",
          "reason" : "invalid recipe ingredient"
       }
