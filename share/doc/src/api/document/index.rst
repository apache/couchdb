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

.. _api/doc:

================
Document Methods
================

The CouchDB API Server Document methods detail how to create, read,
update and delete documents within a database.

A list of the available methods and URL paths are provided below:

+--------+-------------------------+-------------------------------------------+
| Method | Path                    | Description                               |
+========+=========================+===========================================+
| POST   | /db                     | Create a new document                     |
+--------+-------------------------+-------------------------------------------+
| GET    | /db/doc                 | Returns the latest revision of the        |
|        |                         | document                                  |
+--------+-------------------------+-------------------------------------------+
| HEAD   | /db/doc                 | Returns bare information in the HTTP      |
|        |                         | Headers for the document                  |
+--------+-------------------------+-------------------------------------------+
| PUT    | /db/doc                 | Inserts a new document, or new version    |
|        |                         | of an existing document                   |
+--------+-------------------------+-------------------------------------------+
| DELETE | /db/doc                 | Deletes the document                      |
+--------+-------------------------+-------------------------------------------+
| COPY   | /db/doc                 | Copies the document                       |
+--------+-------------------------+-------------------------------------------+
| GET    | /db/doc/attachment      | Gets the attachment of a document         |
+--------+-------------------------+-------------------------------------------+
| PUT    | /db/doc/attachment      | Adds an attachment of a document          |
+--------+-------------------------+-------------------------------------------+
| DELETE | /db/doc/attachment      | Deletes an attachment of a document       |
+--------+-------------------------+-------------------------------------------+


.. toctree::

   common
   attachments
