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

.. _api/db.head:

``HEAD /db``
============

* **Method**: ``HEAD /db``
* **Request**: None
* **Response**: None
* **Admin Privileges Required**: no
* **Return Codes**:

  * **200**:
    Database exists.

  * **404**:
    Requested database not found.

Returns the HTTP Headers containing a minimal amount of information
about the specified database. Since the response body is empty this method
is a lightweight way to check is database exists or not.

.. _api/db.get:

``GET /db``
===========

* **Method**: ``GET /db``
* **Request**: None
* **Response**: Information about the database in JSON format
* **Admin Privileges Required**: no
* **Return Codes**:

  * **404**:
    The requested content could not be found. The returned content will include
    further information, as a JSON object, if available.

Gets information about the specified database. For example, to retrieve
the information for the database ``recipe``:

.. code-block:: http

    GET http://couchdb:5984/recipes
    Accept: application/json

The JSON response contains meta information about the database. A sample
of the JSON returned for an empty database is provided below:

.. code-block:: javascript

    {
       "compact_running" : false,
       "committed_update_seq" : 375048,
       "disk_format_version" : 5,
       "disk_size" : 33153123,
       "doc_count" : 18386,
       "doc_del_count" : 0,
       "db_name" : "recipes",
       "instance_start_time" : "1290700340925570",
       "purge_seq" : 10,
       "update_seq" : 375048
    }


The elements of the returned structure are shown in the table below:

+----------------------------------+-------------------------------------------+
| Field                            | Description                               |
+==================================+===========================================+
| committed_update_seq             | The number of committed update.           |
+----------------------------------+-------------------------------------------+
| compact_running                  | Set to true if the database compaction    |
|                                  | routine is operating on this database.    |
+----------------------------------+-------------------------------------------+
| db_name                          | The name of the database.                 |
+----------------------------------+-------------------------------------------+
| disk_format_version              | The version of the physical format used   |
|                                  | for the data when it is stored on disk.   |
+----------------------------------+-------------------------------------------+
| disk_size                        | Size in bytes of the data as stored on the|
|                                  | disk. Views indexes are not included in   |
|                                  | the calculation.                          |
+----------------------------------+-------------------------------------------+
| doc_count                        | A count of the documents in the specified |
|                                  | database.                                 |
+----------------------------------+-------------------------------------------+
| doc_del_count                    | Number of deleted documents               |
+----------------------------------+-------------------------------------------+
| instance_start_time              | Timestamp of when the database was        |
|                                  | opened, expressed in microseconds since   |
|                                  | the epoch.                                |
+----------------------------------+-------------------------------------------+
| purge_seq                        | The number of purge operations on the     |
|                                  | database.                                 |
+----------------------------------+-------------------------------------------+
| update_seq                       | The current number of updates to the      |
|                                  | database.                                 |
+----------------------------------+-------------------------------------------+

.. _api/db.put:

``PUT /db``
===========

* **Method**: ``PUT /db``
* **Request**: None
* **Response**: JSON success statement
* **Admin Privileges Required**: no
* **Return Codes**:

  * **400**:
    Invalid database name

  * **412**:
    Database already exists

Creates a new database. The database name must be composed of one or
more of the following characters:

-  Lowercase characters (``a-z``)

-  Name must begin with a lowercase letter

-  Digits (``0-9``)

-  Any of the characters ``_``, ``$``, ``(``, ``)``, ``+``, ``-``, and
   ``/``.

Trying to create a database that does not meet these requirements will
return an error quoting these restrictions.

To create the database ``recipes``:

.. code-block:: http

    PUT http://couchdb:5984/recipes
    Content-Type: application/json

The returned content contains the JSON status:

.. code-block:: javascript

    {
       "ok" : true
    }

Anything should be treated as an error, and the problem should be taken
form the HTTP response code.

.. _api/db.delete:

``DELETE /db``
==============

* **Method**: ``DELETE /db``
* **Request**: None
* **Response**: JSON success statement
* **Admin Privileges Required**: no
* **Return Codes**:

  * **200**:
    Database has been deleted

  * **404**:
    The requested content could not be found. The returned content will include
    further information, as a JSON object, if available.

Deletes the specified database, and all the documents and attachments
contained within it.

To delete the database ``recipes`` you would send the request:

.. code-block:: http

    DELETE http://couchdb:5984/recipes
    Content-Type: application/json

If successful, the returned JSON will indicate success

.. code-block:: javascript

    {
       "ok" : true
    }
