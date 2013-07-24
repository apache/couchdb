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


.. _api/db/bulk_docs:
.. _api/db/bulk_docs.post:

``POST /db/_bulk_docs``
=======================

* **Method**: ``POST /db/_bulk_docs``
* **Request**: JSON of the docs and updates to be applied
* **Response**: JSON success statement
* **Admin Privileges Required**: no

* **HTTP Headers**:

  * **Header**: ``X-Couch-Full-Commit``

    * **Description**: Overrides server's commit policy.
    * **Optional**: yes
    * **Values**:

      * **true**: Ensures that any non-committed changes are committed to
        physical storage.
      * **false**: Uses the delay commit in opposite to ``true`` value. CouchDB
        responses quickly, but without any guarantees that all data are
        successfully stored on disk.

* **Return Codes**:

  * **201**:
    Document(s) have been created or updated

The bulk document API allows you to create and update multiple documents
at the same time within a single request. The basic operation is similar
to creating or updating a single document, except that you batch the
document structure and information and . When creating new documents the
document ID is optional. For updating existing documents, you must
provide the document ID, revision information, and new document values.

For both inserts and updates the basic structure of the JSON is the
same:

+----------------------------------+-------------------------------------------+
| Field                            | Description                               |
+==================================+===========================================+
| all_or_nothing (optional)        | Sets the database commit mode to use      |
|                                  | all-or-nothing semantics                  |
+----------------------------------+-------------------------------------------+
| docs [array]                     | Bulk Documents Document                   |
+----------------------------------+-------------------------------------------+
|         _id (optional)           | List of changes, field-by-field, for this |
|                                  | document                                  |
+----------------------------------+-------------------------------------------+
|         _rev (optional)          | Document ID                               |
+----------------------------------+-------------------------------------------+
|         _deleted (optional)      | Update sequence number                    |
+----------------------------------+-------------------------------------------+

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

The return type from a bulk insertion will be 201, with the content of
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

The exact structure of the returned information is:

+----------------------------------+-------------------------------------------+
| Field                            | Description                               |
+==================================+===========================================+
| docs [array]                     | Bulk Documents Document                   |
+----------------------------------+-------------------------------------------+
|         id                       | Document ID                               |
+----------------------------------+-------------------------------------------+
|         error                    | Error type                                |
+----------------------------------+-------------------------------------------+
|         reason                   | Error string with extended reason         |
+----------------------------------+-------------------------------------------+

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
