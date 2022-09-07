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

.. _install/search:

==========================
Search Plugin Installation
==========================

.. versionadded:: 3.0

.. highlight:: ini

CouchDB can build and query full-text search indexes using an external Java
service that embeds `Apache Lucene <http://lucene.apache.org>`_. Typically, this
service is installed on the same host as CouchDB and communicates with it over
the loopback network.

The search plugin is runtime-compatible with Java JDKs 6, 7 and 8. Building a
release from source requires JDK 6. **It will not work with any newer version of
Java.** Sorry about that.

Installation of Binary Packages
===============================

Binary packages that bundle all the necessary dependencies of the search plugin are
available on `GitHub`_.  The files in each release should be unpacked into a directory on
the Java classpath. If you do not have a classpath already set, or you wish to explicitly
set the classpath location for Clouseau, then add the line::

    -classpath '/path/to/clouseau/*'

to the server command below. If clouseau is installed in ``/opt/clouseau`` the line would be::

    -classpath '/opt/clouseau/*'

The service expects to find a couple of configuration files
conventionally called ``clouseau.ini`` and ``log4j.properties`` with the following
content:

**clouseau.ini**::

    [clouseau]

    ; the name of the Erlang node created by the service, leave this unchanged
    name=clouseau@127.0.0.1

    ; set this to the same distributed Erlang cookie used by the CouchDB nodes
    cookie=monster

    ; the path where you would like to store the search index files
    dir=/path/to/index/storage

    ; the number of search indexes that can be open simultaneously
    max_indexes_open=500

**log4j.properties**::

    log4j.rootLogger=debug, CONSOLE
    log4j.appender.CONSOLE=org.apache.log4j.ConsoleAppender
    log4j.appender.CONSOLE.layout=org.apache.log4j.PatternLayout
    log4j.appender.CONSOLE.layout.ConversionPattern=%d{ISO8601} %c [%p] %m%n

Once these files are in place the service can be started with an invocation like
the following::

    java -server \
         -Xmx2G \
         -Dsun.net.inetaddr.ttl=30 \
         -Dsun.net.inetaddr.negative.ttl=30 \
         -Dlog4j.configuration=file:/path/to/log4j.properties \
         -XX:OnOutOfMemoryError="kill -9 %p" \
         -XX:+UseConcMarkSweepGC \
         -XX:+CMSParallelRemarkEnabled \
         com.cloudant.clouseau.Main \
         /path/to/clouseau.ini

Chef
====

The CouchDB `cookbook`_ can build the search plugin from source and install it
on a server alongside CouchDB.

Kubernetes
==========

Users running CouchDB on Kubernetes via the `Helm chart`_ can add the search
service to each CouchDB Pod by setting ``enableSearch: true`` in the chart
values.

Additional Details
==================

The :ref:`Search User Guide <ddoc/search>` provides detailed information on
creating and querying full-text indexes using this plugin.

The source code for the plugin and additional configuration documentation is
available on GitHub at https://github.com/cloudant-labs/clouseau.

.. _GitHub: https://github.com/cloudant-labs/clouseau/releases
.. _cookbook: https://supermarket.chef.io/cookbooks/couchdb
.. _Helm chart: https://github.com/apache/couchdb-helm
