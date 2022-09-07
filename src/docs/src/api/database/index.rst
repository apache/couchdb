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

.. _api/database:

=========
Databases
=========

The Database endpoint provides an interface to an entire database with in
CouchDB. These are database-level, rather than document-level requests.

For all these requests, the database name within the URL path
should be the database name that you wish to perform the operation on.
For example, to obtain the meta information for the database
``recipes``, you would use the HTTP request:

.. code-block:: none

    GET /recipes

For clarity, the form below is used in the URL paths:

.. code-block:: none

    GET /db

Where ``db`` is the name of any database.

.. toctree::
    common
    bulk-api
    find
    shard
    changes
    compact
    security
    misc
