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


.. _api/db/security:
.. _api/db/security.get:

``GET /db/_security``
=====================

* **Method**: ``GET /db/_security``
* **Request**: None
* **Response**: JSON of the security object
* **Admin Privileges Required**: no

Gets the current security object from the specified database. The
security object consists of two compulsory elements, ``admins`` and
``readers``, which are used to specify the list of users and/or roles
that have admin and reader rights to the database respectively. Any
additional fields in the security object are optional. The entire
security object is made available to validation and other internal
functions so that the database can control and limit functionality.

To get the existing security object you would send the following
request:

.. code-block:: javascript

    {
       "admins" : {
          "roles" : [],
          "names" : [
             "mc",
             "slp"
          ]
       },
       "readers" : {
          "roles" : [],
          "names" : [
             "tim",
             "brian"
          ]
       }
    }

Security object structure is:

* **admins**: Roles/Users with admin privileges

  * **roles** [array]: List of roles with parent privilege
  * **users** [array]: List of users with parent privilege

* **readers**: Roles/Users with reader privileges

  * **roles** [array]: List of roles with parent privilege
  * **users** [array]: List of users with parent privilege

.. note::
   If the security object for a database has never been set, then the
   value returned will be empty.

.. _api/db/security.put:

``PUT /db/_security``
=====================

* **Method**: ``PUT /db/_security``
* **Request**: JSON specifying the admin and user security for the database
* **Response**: JSON status message
* **Admin Privileges Required**: no

Sets the security object for the given database.For example, to set the
security object for the ``recipes`` database:

.. code-block:: javascript

    PUT http://couchdb:5984/recipes/_security
    Content-Type: application/json

    {
       "admins" : {
          "roles" : [],
          "names" : [
             "mc",
             "slp"
          ]
       },
       "readers" : {
          "roles" : [],
          "names" : [
             "tim",
             "brian"
          ]
       }
    }

If the setting was successful, a JSON status object will be returned:

.. code-block:: javascript

    {
       "ok" : true
    }
