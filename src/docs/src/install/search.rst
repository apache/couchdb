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

CouchDB can build and query full-text search indexes using an external Java
service that embeds `Apache Lucene <http://lucene.apache.org>`_. Typically, this
service is installed on the same host as CouchDB and communicates with it over
the loopback network.

The search plugin is runtime-compatible with Java JDKs 21 and later. Building a
release from source requires at least JDK 17. **It will not work with any older
version of Java.** Sorry about that.

Installation of Binary Packages
===============================

Binary packages that bundle all the necessary dependencies of the search plugin are
available on `GitHub`_.  The files in each release should be unpacked into a directory on
the Java classpath. If you do not have a classpath already set, or you wish to explicitly
set the classpath location for Clouseau, then add the line::

    -classpath '/path/to/clouseau/*'

to the server command below. If clouseau is installed in ``/opt/clouseau`` the line would be::

    -classpath '/opt/clouseau/*'

The service expects to find its configuration file conventionally called ``clouseau.conf``
with the following content:

**clouseau.conf**::

    logger: {
      format: Raw
      output: Stdout
      level: debug
    }
    config: [
      {
        node: {
          # the name of the Erlang node created by the service, leave this unchanged
          name: clouseau
          domain: 127.0.0.1

          # set this to the same distributed Erlang cookie used by the CouchDB nodes
          cookie: brumbrum
        }
        clouseau: {
          # the path where you would like to store the search index files
          dir: /path/to/index/storage

          # the number of search indexes that can be open simultaneously
          max_indexes_open: 500
        }
      }
    ]

Once this file is in place the service can be started with an invocation like
the following::

    java -server \
         -Xmx2G \
         -Dsun.net.inetaddr.ttl=30 \
         -Dsun.net.inetaddr.negative.ttl=30 \
         -XX:+ExitOnOutOfMemoryError \
         -XX:+UseG1GC \
         -XX:+ParallelRefProcEnabled \
         com.cloudant.ziose.clouseau.Main \
         /path/to/clouseau.conf

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
