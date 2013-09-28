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

.. _api/local:

========================================
Local (non-replicating) Document Methods
========================================

The Local (non-replicating) document interface allows you to create
local documents that are not replicated to other databases. These
documents can be used to hold configuration or other information that is
required specifically on the local CouchDB instance.

Local documents have the following limitations:

-  Local documents are not replicated to other databases.

-  The ID of the local document must be known for the document to
   accessed. You cannot obtain a list of local documents from the
   database.

-  Local documents are not output by views, or the :ref:`api/db/all_docs` view.

Local documents can be used when you want to store configuration or
other information for the current (local) instance of a given database.

A list of the available methods and URL paths are provided below:

+--------+-------------------------+-------------------------------------------+
| Method | Path                    | Description                               |
+========+=========================+===========================================+
| GET    | /db/_local/id           | Returns the latest revision of the        |
|        |                         | non-replicated document                   |
+--------+-------------------------+-------------------------------------------+
| PUT    | /db/_local/id           | Inserts a new version of the              |
|        |                         | non-replicated document                   |
+--------+-------------------------+-------------------------------------------+
| DELETE | /db/_local/id           | Deletes the non-replicated document       |
+--------+-------------------------+-------------------------------------------+
| COPY   | /db/_local/id           | Copies the non-replicated document        |
+--------+-------------------------+-------------------------------------------+

.. _api/local/doc:

``/db/_local/id``
========================

.. http:get:: /{db}/_local/{docid}

  Gets the specified local document. The semantics are identical to
  accessing a standard document in the specified database, except that the
  document is not replicated. See :get:`/{db}/{docid}`.

.. http:put:: /{db}/_local/{docid}

  Stores the specified local document. The semantics are identical to
  storing a standard document in the specified database, except that the
  document is not replicated. See :put:`/{db}/{docid}`.

.. http:delete:: /{db}/_local/{docid}

  Deletes the specified local document. The semantics are identical to
  deleting a standard document in the specified database, except that the
  document is not replicated. See :delete:`/{db}/{docid}`.

.. http:copy:: /{db}/_local/{docid}

  Copies the specified local document. The semantics are identical to
  copying a standard document in the specified database, except that the
  document is not replicated. See :copy:`/{db}/{docid}`.
