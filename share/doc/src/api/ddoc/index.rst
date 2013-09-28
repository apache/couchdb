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


.. _api/design-docs:

=======================
Design Document Methods
=======================

In CouchDB, design documents provide the main interface for building a
CouchDB application. The design document defines the views used to
extract information from CouchDB through one or more views. Design
documents are created within your CouchDB instance in the same way as
you create database documents, but the content and definition of the
documents is different. Design Documents are named using an ID defined
with the design document URL path, and this URL can then be used to
access the database contents.

Views and lists operate together to provide automated (and formatted)
output from your database.

.. toctree::

   common
   views
   render
   rewrites
