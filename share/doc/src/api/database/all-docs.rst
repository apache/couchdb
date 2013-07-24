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
.. _api/db/all_docs.get:

``GET /db/_all_docs``
=====================

* **Method**: ``GET /db/_all_docs``
* **Request**: None
* **Response**: JSON object containing document information, ordered by the
  document ID
* **Admin Privileges Required**: no
* **Query Arguments**:

  * **Argument**: descending

    * **Description**:  Return the documents in descending by key order
    * **Optional**: yes
    * **Type**: boolean
    * **Default**: false

  * **Argument**: endkey

    * **Description**:  Stop returning records when the specified key is reached
    * **Optional**: yes
    * **Type**: string

  * **Argument**: endkey_docid

    * **Description**:  Stop returning records when the specified document ID is
      reached
    * **Optional**: yes
    * **Type**: string

  * **Argument**: group

    * **Description**:  Group the results using the reduce function to a group
      or single row
    * **Optional**: yes
    * **Type**: boolean
    * **Default**: false

  * **Argument**: group_level

    * **Description**:  Specify the group level to be used
    * **Optional**: yes
    * **Type**: numeric

  * **Argument**: include_docs

    * **Description**:  Include the full content of the documents in the return
    * **Optional**: yes
    * **Type**: boolean
    * **Default**: false

  * **Argument**: inclusive_end

    * **Description**:  Specifies whether the specified end key should be
      included in the result
    * **Optional**: yes
    * **Type**: boolean
    * **Default**: true

  * **Argument**: key

    * **Description**:  Return only documents that match the specified key
    * **Optional**: yes
    * **Type**: string

  * **Argument**: limit

    * **Description**:  Limit the number of the returned documents to the
      specified number
    * **Optional**: yes
    * **Type**: numeric

  * **Argument**: reduce

    * **Description**:  Use the reduction function
    * **Optional**: yes
    * **Type**: boolean
    * **Default**: true

  * **Argument**: skip

    * **Description**:  Skip this number of records before starting to return
      the results
    * **Optional**: yes
    * **Type**: numeric
    * **Default**: 0

  * **Argument**: stale

    * **Description**:  Allow the results from a stale view to be used
    * **Optional**: yes
    * **Type**: string
    * **Default**:
    * **Supported Values**:

      * **ok**: Allow stale views

  * **Argument**: startkey

    * **Description**:  Return records starting with the specified key
    * **Optional**: yes
    * **Type**: string

  * **Argument**: startkey_docid

    * **Description**:  Return records starting with the specified document ID
    * **Optional**: yes
    * **Type**: string

Returns a JSON structure of all of the documents in a given database.
The information is returned as a JSON structure containing meta
information about the return structure, and the list documents and basic
contents, consisting the ID, revision and key. The key is generated from
the document ID.

+----------------------------------+-------------------------------------------+
| Field                            | Description                               |
+==================================+===========================================+
| offset                           | Offset where the document list started    |
+----------------------------------+-------------------------------------------+
| rows [array]                     | Array of document object                  |
+----------------------------------+-------------------------------------------+
| total_rows                       | Number of documents in the database/view  |
+----------------------------------+-------------------------------------------+
| update_seq                       | Current update sequence for the database  |
+----------------------------------+-------------------------------------------+

By default the information returned contains only the document ID and
revision. For example, the request:

.. code-block:: http

    GET http://couchdb:5984/recipes/_all_docs
    Accept: application/json

Returns the following structure:

.. code-block:: javascript

    {
       "total_rows" : 18386,
       "rows" : [
          {
             "value" : {
                "rev" : "1-bc0d5aed1e339b1cc1f29578f3220a45"
             },
             "id" : "Aberffrawcake",
             "key" : "Aberffrawcake"
          },
          {
             "value" : {
                "rev" : "3-68a20c89a5e70357c20148f8e82ca331"
             },
             "id" : "Adukiandorangecasserole-microwave",
             "key" : "Adukiandorangecasserole-microwave"
          },
          {
             "value" : {
                "rev" : "3-9b2851ed9b6f655cc4eb087808406c60"
             },
             "id" : "Aioli-garlicmayonnaise",
             "key" : "Aioli-garlicmayonnaise"
          },
          ...
             ],
       "offset" : 0
    }

The information is returned in the form of a temporary view of all the
database documents, with the returned key consisting of the ID of the
document. The remainder of the interface is therefore identical to the
View query arguments and their behavior.

.. _api/db/all_docs.post:

``POST /db/_all_docs``
======================

* **Method**: ``POST /db/_all_docs``
* **Request**: JSON of the document IDs you want included
* **Response**: JSON of the returned view
* **Admin Privileges Required**: no

The ``POST`` to ``_all_docs`` allows to specify multiple keys to be
selected from the database. This enables you to request multiple
documents in a single request, in place of multiple
:ref:`api/doc.get` requests.

The request body should contain a list of the keys to be returned as an
array to a ``keys`` object. For example:

.. code-block:: http

    POST http://couchdb:5984/recipes/_all_docs
    User-Agent: MyApp/0.1 libwww-perl/5.837

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
