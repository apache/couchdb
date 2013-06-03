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

-  Local documents are not output by views, or the ``_all_docs`` view.

Local documents can be used when you want to store configuration or
other information for the current (local) instance of a given database.

A list of the available methods and URL paths are provided below:

+--------+-------------------------+-------------------------------------------+
| Method | Path                    | Description                               |
+========+=========================+===========================================+
| GET    | /db/_local/local-doc    | Returns the latest revision of the        |
|        |                         | non-replicated document                   |
+--------+-------------------------+-------------------------------------------+
| PUT    | /db/_local/local-doc    | Inserts a new version of the              |
|        |                         | non-replicated document                   |
+--------+-------------------------+-------------------------------------------+
| DELETE | /db/_local/local-doc    | Deletes the non-replicated document       |
+--------+-------------------------+-------------------------------------------+
| COPY   | /db/_local/local-doc    | Copies the non-replicated document        |
+--------+-------------------------+-------------------------------------------+

.. _api/local/doc:
.. _api/local/doc.get:

``GET /db/_local/local-doc``
============================

* **Method**: ``GET /db/_local/local-doc``
* **Request**: None
* **Response**: JSON of the returned document
* **Admin Privileges Required**: no
* **Query Arguments**:

  * **Argument**: rev

    * **Description**:  Specify the revision to return
    * **Optional**: yes
    * **Type**: string
    * **Supported Values**:

      * **true**: Includes the revisions

  * **Argument**: revs

    * **Description**:  Return a list of the revisions for the document
    * **Optional**: yes
    * **Type**: boolean

  * **Argument**: revs_info

    * **Description**:  Return a list of detailed revision information for
      the document
    * **Optional**: yes
    * **Type**: boolean
    * **Supported Values**

      * **true**: Includes the revisions

* **Return Codes**:

  * **400**:
    The format of the request or revision was invalid
  * **404**:
    The specified document or revision cannot be found, or has been deleted

Gets the specified local document. The semantics are identical to
accessing a standard document in the specified database, except that the
document is not replicated. See :ref:`api/doc.get`.

``PUT /db/_local/local-doc``
============================

* **Method**: ``PUT /db/_local/local-doc``
* **Request**: JSON of the document
* **Response**: JSON with the committed document information
* **Admin Privileges Required**: no
* **Return Codes**:

  * **201**:
    Document has been created successfully

Stores the specified local document. The semantics are identical to
storing a standard document in the specified database, except that the
document is not replicated. See :ref:`api/doc.put`.

``DELETE /db/_local/local-doc``
===============================

* **Method**: ``DELETE /db/_local/local-doc``
* **Request**: None
* **Response**: JSON with the deleted document information
* **Admin Privileges Required**: no
* **Query Arguments**:

  * **Argument**: rev

    * **Description**: Current revision of the document for validation
    * **Optional**: yes
    * **Type**: string

* **HTTP Headers**

  * **Header**: ``If-Match``

    * **Description**: Current revision of the document for validation
    * **Optional**: yes

* **Return Codes**:

  * **409**:
    Supplied revision is incorrect or missing

Deletes the specified local document. The semantics are identical to
deleting a standard document in the specified database, except that the
document is not replicated. See :ref:`api/doc.delete`.

``COPY /db/_local/local-doc``
=============================

* **Method**: ``COPY /db/_local/local-doc``
* **Request**: None
* **Response**: JSON of the copied document
* **Admin Privileges Required**: no
* **Query Arguments**:

  * **Argument**: rev

    * **Description**: Revision to copy from
    * **Optional**: yes
    * **Type**: string

* **HTTP Headers**

  * **Header**: ``Destination``

    * **Description**: Destination document (and optional revision)
    * **Optional**: no

Copies the specified local document. The semantics are identical to
copying a standard document in the specified database, except that the
document is not replicated. See :ref:`api/doc.copy`.
