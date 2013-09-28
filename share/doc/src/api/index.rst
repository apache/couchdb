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

.. _api:

=============
API Reference
=============

The components of the API URL path help determine the part of the
CouchDB server that is being accessed. The result is the structure of
the URL request both identifies and effectively describes the area of
the database you are accessing.

As with all URLs, the individual components are separated by a forward
slash.

As a general rule, URL components and JSON fields starting with the
``_`` (underscore) character represent a special component or entity
within the server or returned object. For example, the URL fragment
``/_all_dbs`` gets a list of all of the databases in a CouchDB instance.

This reference is structured according to the URL structure, as below.

.. toctree::
   :maxdepth: 2

   basics
   server/index
   database/index
   document/index
   ddoc/index
   local
