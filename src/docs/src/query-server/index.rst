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

.. _query-server:

============
Query Server
============

The `Query server` is an external process that communicates with CouchDB by JSON
protocol through stdio interface and processes all
:ref:`design functions <ddocs>` calls, such as JavaScript :ref:`views <viewfun>`.

The default query server is written in
:ref:`JavaScript <query-server/js>`, running via `Mozilla SpiderMonkey`_.
You can use :ref:`other languages <config/query_servers>` by setting a Query
server key in the ``language`` property of a design document or the
`Content-Type` header of a `temporary view`. Design documents that do not
specify a ``language`` property are assumed to be of type `javascript`.

.. _Mozilla SpiderMonkey: https://spidermonkey.dev/

.. toctree::
    :maxdepth: 2

    protocol
    javascript
    erlang
