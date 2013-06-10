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

.. highlight:: ini

.. _config/admins:

``[admins]`` :: Configuring Server Administrators
=================================================

A default CouchDB install provides admin-level access to all connecting users.
This configuration is known as `Admin Party`, and is not recommended for
in-production usage. You can crash the party simply by creating the first
admin account. CouchDB server administrators and passwords are not stored
in the ``_users`` database, but in the ``local.ini`` file, which should be
appropriately secured and readable only by system administrators::

  [admins]
  ;admin = mysecretpassword
  admin = -hashed-6d3c30241ba0aaa4e16c6ea99224f915687ed8cd,7f4a3e05e0cbc6f48a0035e3508eef90
  architect = -pbkdf2-43ecbd256a70a3a2f7de40d2374b6c3002918834,921a12f74df0c1052b3e562a23cd227f,10000

Administrators can be added directly to the ``[admins]`` section, and when
CouchDB is restarted, the passwords will be salted and encrypted. You may
also use the HTTP interface to create administrator accounts; this way,
you don't need to restart CouchDB, and there's no need to temporarily store
or transmit passwords in plaintext. The HTTP ``_config/admins`` endpoint
supports querying, deleting or creating new admin accounts:

.. code-block:: http

   GET /_config/admins HTTP/1.1
   Accept: application/json
   Host: localhost:5984

.. code-block:: http

   HTTP/1.1 200 OK
   Cache-Control: must-revalidate
   Content-Length: 196
   Content-Type: application/json
   Date: Fri, 30 Nov 2012 11:37:18 GMT
   Server: CouchDB/1.3.0 (Erlang OTP/R15B03)

.. code-block:: json

   {
     "admin": "-hashed-6d3c30241ba0aaa4e16c6ea99224f915687ed8cd,7f4a3e05e0cbc6f48a0035e3508eef90",
     "architect": "-pbkdf2-43ecbd256a70a3a2f7de40d2374b6c3002918834,921a12f74df0c1052b3e562a23cd227f,10000"
   }

If you already have a salted, encrypted password string (for example,
from an old ``local.ini`` file, or from a different CouchDB server), then
you can store the "raw" encrypted string, without having CouchDB doubly
encrypt it.

.. code-block:: http

   PUT /_config/admins/architect?raw=true HTTP/1.1
   Accept: application/json
   Content-Type: application/json
   Content-Length: 89
   Host: localhost:5984

   "-pbkdf2-43ecbd256a70a3a2f7de40d2374b6c3002918834,921a12f74df0c1052b3e562a23cd227f,10000"

.. code-block:: http

   HTTP/1.1 200 OK
   Cache-Control: must-revalidate
   Content-Length: 89
   Content-Type: application/json
   Date: Fri, 30 Nov 2012 11:39:18 GMT
   Server: CouchDB/1.3.0 (Erlang OTP/R15B02)

   "-pbkdf2-43ecbd256a70a3a2f7de40d2374b6c3002918834,921a12f74df0c1052b3e562a23cd227f,10000"

Further details are available in `security`, including configuring the
work factor for ``PBKDF2``, and the algorithm itself at
`PBKDF2 (RFC-2898) <http://tools.ietf.org/html/rfc2898>`_.

.. versionchanged:: 1.3 `PBKDF2` server-side hashed salted password support
   added, now as a synchronous call for the ``_config/admins`` API.
