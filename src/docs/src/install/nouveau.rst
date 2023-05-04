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

.. _install/nouveau:

===========================
Nouveau Server Installation
===========================

.. versionadded:: 4.0

.. highlight:: ini

CouchDB can build and query full-text search indexes using an external Java
service that embeds `Apache Lucene <https://lucene.apache.org>`_. Typically, this
service is installed on the same host as CouchDB and communicates with it over
the loopback network.

Nouveau server is runtime-compatible with Java 11 or higher.

Enable Nouveau
==============

You need to enable nouveau in CouchDB configuration;

    .. code-block:: ini

        [nouveau]
        enable = true

Installation of Binary Packages
===============================

The nouveau server code is contained in a single ``jar`` file that ships with a compatible
CouchDB release, named ``server-$version-dist.jar``

To start the nouveau server::

    java -jar /path/to/server-$version-dist.jar server /path/to/nouveau.yaml

We ship a basic ``nouveau.yaml`` configuration with useful defaults;
that file for details.

**nouveau.yaml**::

    maxIndexesOpen: 100
    commitIntervalSeconds: 30
    idleSeconds: 60
    rootDir: target/indexes

As a `DropWizard <https://dropwizard.io>`_ project you can also use the many
configuration options that it supports. See `configuration reference
<https://www.dropwizard.io/en/latest/manual/configuration.html>`_.

By default Nouveau will attempt a clean shutdown if sent a ``TERM``
signal, committing any outstanding index updates, completing any
in-progress segment merges, and finally closes all indexes. This is
not essential and you may safely kill the JVM without letting it do
this, though any uncommitted changes are necessarily lost. Once the
JVM is started again this indexing work will be attempted again.
