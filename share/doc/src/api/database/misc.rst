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


.. _api/db/purge:
.. _api/db/purge.post:

``POST /db/_purge``
===================

* **Method**: ``POST /db/_purge``
* **Request**: JSON of the document IDs/revisions to be purged
* **Response**: JSON structure with purged documents and purge sequence
* **Admin Privileges Required**: no

A database purge permanently removes the references to deleted documents
from the database. Deleting a document within CouchDB does not actually
remove the document from the database, instead, the document is marked as
a deleted (and a new revision is created). This is to ensure that
deleted documents are replicated to other databases as having been
deleted. This also means that you can check the status of a document and
identify that the document has been deleted.

The purge operation removes the references to the deleted documents from
the database. The purging of old documents is not replicated to other
databases. If you are replicating between databases and have deleted a
large number of documents you should run purge on each database.

.. note::

   Purging documents does not remove the space used by them on disk. To
   reclaim disk space, you should run a database compact (see
   :ref:`api/db/compact`), and compact views (see :ref:`api/db/compact/ddoc`).

To perform a purge operation you must send a request including the JSON
of the document IDs that you want to purge. For example:

.. code-block:: http

    POST http://couchdb:5984/recipes/_purge
    Content-Type: application/json

    {
      "FishStew" : [
        "17-b3eb5ac6fbaef4428d712e66483dcb79"
        ]
    }

The format of the request must include the document ID and one or more
revisions that must be purged.

The response will contain the purge sequence number, and a list of the
document IDs and revisions successfully purged.

.. code-block:: javascript

    {
       "purged" : {
          "FishStew" : [
             "17-b3eb5ac6fbaef4428d712e66483dcb79"
          ]
       },
       "purge_seq" : 11
    }

Updating Indexes
----------------

The number of purges on a database is tracked using a purge sequence.
This is used by the view indexer to optimize the updating of views that
contain the purged documents.

When the indexer identifies that the purge sequence on a database has
changed, it compares the purge sequence of the database with that stored
in the view index. If the difference between the stored sequence and
database is sequence is only 1, then the indexer uses a cached list of
the most recently purged documents, and then removes these documents
from the index individually. This prevents completely rebuilding the
index from scratch.

If the difference between the stored sequence number and current
database sequence is greater than 1, then the view index is entirely
rebuilt. This is an expensive operation as every document in the
database must be examined.

.. _api/db/missing_revs:
.. _api/db/missing_revs.post:

``POST /db/_missing_revs``
==========================

* **Method**: ``POST /db/_missing_revs``
* **Request**: JSON list of document revisions
* **Response**: JSON of missing revisions
* **Admin Privileges Required**: no

.. _api/db/revs_diff:
.. _api/db/revs_diff.post:

``POST /db/_revs_diff``
=======================

* **Method**: ``POST /db/_revs_diff``
* **Request**: JSON list of document revisions
* **Response**: JSON list of differences from supplied document/revision list
* **Admin Privileges Required**: no



.. _api/db/revs_limit:
.. _api/db/revs_limit.get:

``GET /db/_revs_limit``
=======================

* **Method**: ``GET /db/_revs_limit``
* **Request**: None
* **Response**: The current revision limit setting
* **Admin Privileges Required**: no


Gets the current ``revs_limit`` (revision limit) setting.

For example to get the current limit:

.. code-block:: http

    GET http://couchdb:5984/recipes/_revs_limit
    Content-Type: application/json

The returned information is the current setting as a numerical scalar:

.. code-block:: javascript

    1000

.. _api/db/revs_limit.put:

``PUT /db/_revs_limit``
=======================

* **Method**: ``PUT /db/_revs_limit``
* **Request**: A scalar integer of the revision limit setting
* **Response**: Confirmation of setting of the revision limit
* **Admin Privileges Required**: no

Sets the maximum number of document revisions that will be tracked by
CouchDB, even after compaction has occurred. You can set the revision
limit on a database by using ``PUT`` with a scalar integer of the limit
that you want to set as the request body.

For example to set the revs limit to 100 for the ``recipes`` database:

.. code-block:: http

    PUT http://couchdb:5984/recipes/_revs_limit
    Content-Type: application/json

    100

If the setting was successful, a JSON status object will be returned:

.. code-block:: javascript

    {
       "ok" : true
    }

.. _JSON object: #table-couchdb-api-db_db-json-changes

