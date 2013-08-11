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

.. _api/db:

================
Database Methods
================

The Database methods provide an interface to an entire database withing
CouchDB. These are database, rather than document, level requests.

A list of the available methods and URL paths are provided below:

+--------+-------------------------+-------------------------------------------+
| Method | Path                    | Description                               |
+========+=========================+===========================================+
| HEAD   | /db                     | Checks database existence                 |
+--------+-------------------------+-------------------------------------------+
| GET    | /db                     | Returns database information              |
+--------+-------------------------+-------------------------------------------+
| PUT    | /db                     | Create a new database                     |
+--------+-------------------------+-------------------------------------------+
| DELETE | /db                     | Delete an existing database               |
+--------+-------------------------+-------------------------------------------+
| GET    | /db/_all_docs           | Returns a built-in view of all documents  |
|        |                         | in this database                          |
+--------+-------------------------+-------------------------------------------+
| POST   | /db/_all_docs           | Returns certain rows from the built-in    |
|        |                         | view of all documents                     |
+--------+-------------------------+-------------------------------------------+
| POST   | /db/_bulk_docs          | Insert multiple documents in to the       |
|        |                         | database in a single request              |
+--------+-------------------------+-------------------------------------------+
| GET    | /db/_changes            | Returns changes for the given database    |
+--------+-------------------------+-------------------------------------------+
| POST   | /db/_compact            | Starts a compaction for the database      |
+--------+-------------------------+-------------------------------------------+
| POST   | /db/_compact/design-doc | Starts a compaction for all the views in  |
|        |                         | the selected design document              |
+--------+-------------------------+-------------------------------------------+
| POST   | /db/_ensure_full_commit | Makes sure all uncommitted changes are    |
|        |                         | written and synchronized to the disk      |
+--------+-------------------------+-------------------------------------------+
| POST   | /db/_missing_revs       | Given a list of document revisions,       |
|        |                         | returns the document revisions that do not|
|        |                         | exist in the database                     |
+--------+-------------------------+-------------------------------------------+
| POST   | /db/_purge              | Purge some historical documents entirely  |
|        |                         | from database history                     |
+--------+-------------------------+-------------------------------------------+
| POST   | /db/_revs_diff          | Given a list of document revisions,       |
|        |                         | returns differences between the given     |
|        |                         | revisions and ones that are in the        |
|        |                         | database                                  |
+--------+-------------------------+-------------------------------------------+
| GET    | /db/_revs_limit         | Gets the limit of historical revisions to |
|        |                         | store for a single document in the        |
|        |                         | database                                  |
+--------+-------------------------+-------------------------------------------+
| PUT    | /db/_revs_limit         | Sets the limit of historical revisions to |
|        |                         | store for a single document in the        |
|        |                         | database                                  |
+--------+-------------------------+-------------------------------------------+
| GET    | /db/_security           | Returns the special security object for   |
|        |                         | the database                              |
+--------+-------------------------+-------------------------------------------+
| PUT    | /db/_security           | Sets the special security object for the  |
|        |                         | database                                  |
+--------+-------------------------+-------------------------------------------+
| POST   | /db/_temp_view          | Execute a given view function for all     |
|        |                         | documents and return the result           |
+--------+-------------------------+-------------------------------------------+
| POST   | /db/_view_cleanup       | Removes view files that are not used by   |
|        |                         | any design document                       |
+--------+-------------------------+-------------------------------------------+

For all the database methods, the database name within the URL path
should be the database name that you wish to perform the operation on.
For example, to obtain the meta information for the database
``recipes``, you would use the HTTP request:

.. code-block:: http

    GET /recipes

For clarity, the form below is used in the URL paths:

.. code-block:: http

    GET /db

Where ``db`` is the name of any database.

.. toctree::

   common
   bulk-api
   changes
   compact
   security
   temp-views
   misc
