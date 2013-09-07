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


.. _intro:

============
Introduction
============

CouchDB is a database that completely embraces the web. Store your data with
JSON documents. Access your documents with your web browser, :ref:`via HTTP
<api/basics>`. :ref:`Query <api/doc>`, :ref:`combine <views>`,
and :ref:`transform <listfun>` your documents with :ref:`JavaScript
<query-server/js>`. CouchDB works well with modern web and mobile apps. You can
even serve web apps directly out of CouchDB. And you can distribute your data,
or your apps, efficiently using CouchDB’s :ref:`incremental replication
<replication/intro>`. CouchDB supports master-master setups with
:ref:`automatic conflict <replication/conflicts>` detection.

CouchDB comes with a suite of features, such as on-the-fly document
transformation and real-time :ref:`change notifications <changes>`, that makes
:ref:`web app <couchapp>` development a breeze. It even comes with an easy
to use :ref:`web administration console <intro/futon>`. You guessed it,
served up directly out of CouchDB! We care a lot about `distributed scaling`_.
CouchDB is highly available and partition tolerant, but is also :ref:`eventually
consistent <intro/consistency>`. And we care *a lot* about your data.
CouchDB has a fault-tolerant storage engine that puts the safety of your data
first.

In this section you'll learn about every basic bit of CouchDB, see upon what
conceptions and technologies it built and walk through short tutorial that
teach how to use CouchDB.

.. _distributed scaling: http://en.wikipedia.org/wiki/CAP_theorem

.. toctree::
   :maxdepth: 2

   overview
   why
   consistency
   tour
   api
   futon
   curl
