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


.. _api/db/compact:
.. _api/db/compact.post:

``POST /db/_compact``
=====================

* **Method**: ``POST /db/_compact``
* **Request**: None
* **Response**: JSON success statement
* **Admin Privileges Required**: yes
* **Return Codes**:

  * **202**:
    Compaction request has been accepted

  * **404**:
    The requested content could not be found. The returned content will include
    further information, as a JSON object, if available.

Request compaction of the specified database. Compaction compresses the
disk database file by performing the following operations:

-  Writes a new version of the database file, removing any unused
   sections from the new version during write. Because a new file is
   temporary created for this purpose, you will need twice the current
   storage space of the specified database in order for the compaction
   routine to complete.

-  Removes old revisions of documents from the database, up to the
   per-database limit specified by the ``_revs_limit`` database
   parameter.

Compaction can only be requested on an individual database; you cannot
compact all the databases for a CouchDB instance. The compaction process
runs as a background process.

You can determine if the compaction process is operating on a database
by obtaining the database meta information, the ``compact_running``
value of the returned database structure will be set to true. See
:http:get:`/{db}`.

You can also obtain a list of running processes to determine whether
compaction is currently running. See :ref:`api/server/active_tasks`.

.. _api/db/compact/ddoc:
.. _api/db/compact/ddoc.post:

``POST /db/_compact/design-doc``
================================

* **Method**: ``POST /db/_compact/design-doc``
* **Request**: None
* **Response**: JSON success statement
* **Admin Privileges Required**: yes
* **Return Codes**:

  * **202**:
    Compaction request has been accepted

  * **404**:
    The requested content could not be found. The returned content will include
    further information, as a JSON object, if available.

Compacts the view indexes associated with the specified design document.
You can use this in place of the full database compaction if you know a
specific set of view indexes have been affected by a recent database
change.

For example, to compact the views associated with the ``recipes`` design
document:

.. code-block:: http

    POST http://couchdb:5984/recipes/_compact/recipes
    Content-Type: application/json

CouchDB will immediately return with a status indicating that the
compaction request has been received (HTTP status code 202):

.. code-block:: javascript

    {
       "ok" : true
    }





.. _api/db/ensure_full_commit:
.. _api/db/ensure_full_commit.post:

``POST /db/_ensure_full_commit``
================================

* **Method**: ``POST /db/_ensure_full_commit``
* **Request**: None
* **Response**: JSON success statement
* **Admin Privileges Required**: no
* **Return Codes**:

  * **202**:
    Commit completed successfully

  * **404**:
    The requested content could not be found. The returned content will include
    further information, as a JSON object, if available.


Commits any recent changes to the specified database to disk. You should
call this if you want to ensure that recent changes have been written.
For example, to commit all the changes to disk for the database
``recipes`` you would use:

.. code-block:: http

    POST http://couchdb:5984/recipes/_ensure_full_commit
    Content-Type: application/json

This returns a status message, containing the success message and the
timestamp for when the CouchDB instance was started:

.. code-block:: javascript

    {
      "ok" : true,
      "instance_start_time" : "1288186189373361"
    }


.. _api/db/view_cleanup:
.. _api/db/view_cleanup.post:

``POST /db/_view_cleanup``
==========================

* **Method**: ``POST /db/_view_cleanup``
* **Request**: None
* **Response**: JSON success statement
* **Admin Privileges Required**: yes

Cleans up the cached view output on disk for a given view. For example:

.. code-block:: http

    POST http://couchdb:5984/recipes/_view_cleanup
    Content-Type: application/json

If the request is successful, a basic status message us returned:

.. code-block:: javascript

    {
       "ok" : true
    }
