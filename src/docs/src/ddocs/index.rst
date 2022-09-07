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

.. _indexes:

================
Design Documents
================

CouchDB supports special documents within databases known as "design
documents". These documents, mostly driven by JavaScript you write, are used
to build indexes, validate document updates, format query results, and filter
replications.

.. toctree::
    :maxdepth: 2

    ddocs
    views/index
    search

*Note*: Previously, the functionality provided by CouchDB's design documents,
in combination with document attachments, was referred to as "CouchApps." The
general principle was that entire web applications could be hosted in CouchDB,
without need for an additional application server.

Use of CouchDB as a combined standalone database and application server is no
longer recommended. There are significant limitations to a pure CouchDB web
server application stack, including but not limited to: fully-fledged
fine-grained security, robust templating and scaffolding, complete developer
tooling, and most importantly, a thriving ecosystem of developers, modules and
frameworks to choose from.

The developers of CouchDB believe that web developers should pick "the right
tool for the right job". Use CouchDB as your database layer, in conjunction
with any number of other server-side web application frameworks, such as the
entire Node.JS ecosystem, Python's Django and Flask, PHP's Drupal, Java's
Apache Struts, and more.
