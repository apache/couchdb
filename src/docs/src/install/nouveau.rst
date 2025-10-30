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

.. versionadded:: 3.4.0

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

The Java side of nouveau is a set of ``jar`` files, one for nouveau itself and the rest
for dependencies (like Lucene and Dropwizard).

To start the nouveau server::

    java -jar /path/to/nouveau.jar server /path/to/nouveau.yaml

Ensure that all the jar files from the release are in the same directory as
nouveau.jar

We ship a basic ``nouveau.yaml`` configuration with useful defaults;
see that file for details.

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

Securing Nouveau
================

By default Nouveau uses HTTP without client authentication as it is commonly
deployed on the same server as CouchDB itself. It is however possible to deploy
Nouveau with robust security and this is strongly recommended when Nouveau is
running on a separate server, even over a private network you trust.

To enforce secure communications between CouchDB and the Nouveau server you need
to modify configuration in both.

We use mutual TLS to achieve private communication and ensure the identity of
client and server.

Configuring CouchDB to authenticate to Nouveau
----------------------------------------------

You can configure CouchDB to connect to a secured Nouveau server as follows;

  .. code-block:: ini

    [nouveau]
    url = https://hostname:port
    ssl_key_file = path to keyfile
    ssl_cert_file = path to certfile
    ssl_cacert_file = path to cacertfile
    ssl_password = password

You must set ``ssl_key_file`` and ``ssl_cert_file`` to activate client
certificates. You might also need to set ``ssl_password`` if the
``ssl_key_file`` is password-protected and might need to set ``ssl_cacert_file``
if the Nouveau server's certificate is signed by a private CA.

Configuring Nouveau to authenticate clients
-------------------------------------------

Nouveau is built on the dropwizard framework, which directly supports the
`HTTPS <https://www.dropwizard.io/en/stable/manual/configuration.html#http-2-over-tls>`_
transport.

Acquiring or generating client and server certificates are out of scope of this
documentation and we assume they have been created from here onward. We further
assume the user can construct a Java keystore.

in ``nouveau.yaml`` you should remove all connectors of type ``h2c`` and add new
ones using ``h2``;

  .. code-block:: yaml

    server:
      applicationConnectors:
        - type: h2
          port: 5987
          keyStorePath: <path to keystore>
          keyStorePassword: <password to keystore>
          needClientAuth: true
          validateCerts: true
          validatePeers: true

If you're using self-generated certificates you will also need to set the
``trustStorePath`` and ``trustStorePassword`` attributes.

Docker
======

There is a version of of the :ref:`semi-official CouchDB Docker image <install/docker>`
available under the ``*-nouveau`` tags (eg, ``3.4-nouveau``).

Compose
-------

A minimal CouchDB/Nouveau cluster can be create with this compose:

.. code-block:: yaml

    services:
      couchdb:
        image: couchdb:3
        environment:
          COUCHDB_USER: admin
          COUCHDB_PASSWORD: admin
        volumes:
          - couchdb:/opt/couchdb/data
        ports:
          - 5984:5984
        configs:
          - source: nouveau.ini
            target: /opt/couchdb/etc/local.d/nouveau.ini

      nouveau:
        image: couchdb:3-nouveau

    volumes:
      couchdb:

    configs:
      nouveau.ini:
        content: |
          [couchdb]
          single_node=true
          [nouveau]
          enable = true
          url = http://nouveau:5987

.. note::

    This is not production ready, but it is a quick way to get Nouveau running.
