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

.. _configuring:

===================
Configuring CouchDB
===================

.. todo:: Configuring CouchDB

CouchDB Configuration Files
===========================

.. todo:: CouchDB Configuration Files

Configuration File Locations
============================

CouchDB reads files from the following locations, in the following
order.

1. ``PREFIX/default.ini``

2. ``PREFIX/default.d/*``

3. ``PREFIX/local.ini``

4. ``PREFIX/local.d/*``

Settings in successive documents override the settings in earlier
entries. For example, setting the ``bind_address`` parameter in
``local.ini`` would override any setting in ``default.ini``.

.. warning::
   The ``default.ini`` file may be overwritten during an upgrade or
   re-installation, so localised changes should be made to the
   ``local.ini`` file or files within the ``local.d`` directory.

.. _update-notifications:

Update Notifications
====================

.. todo:: Update Notifications


MochiWeb Server Options
=======================

Server options for the MochiWeb component of CouchDB can be added to the
configuration files. Settings should be added to the ``server_options``
option of the ``[httpd]`` section of ``local.ini``. For example:

.. code-block:: ini

    [httpd]
    server_options = [{backlog, 128}, {acceptor_pool_size, 16}]

Socket Options Configuration Setting
====================================

The socket options for the listening socket in CouchDB can now be set
within the CouchDB configuration file. The setting should be added to
the ``[httpd]`` section of the file using the option name
``socket_options``. The specification is as a list of tuples. For
example:

.. code-block:: ini

    [httpd]
    socket_options = [{recbuf, 262144}, {sndbuf, 262144}, {nodelay, true}]

The options supported are a subset of full options supported by the
TCP/IP stack. A list of the supported options are provided in the
`Erlang inet`_ documentation.

.. _Erlang inet: http://www.erlang.org/doc/man/inet.html#setopts-2

``vhosts`` definitions
======================

Similar to the rewrites section of a ``_design`` document, the
``vhosts`` system uses variables in the form of ``:varname`` or wildcards in
the form of asterisks. The variable results can be output into the
resulting path as they are in the rewriter.


Configuring Server Administrators
=================================

A default CouchDB install provides admin-level access to all connecting users.
This configuration is known as ``Admin Party``, and is not recommended for
in-production usage. You can crash the party simply by creating the first
admin account. CouchDB server administrators and passwords are not stored
in the ``_users`` database, but in the ``local.ini`` file, which should be
appropriately secured and readable only by system administrators.

.. code-block:: ini

    [admins]
    ;admin = mysecretpassword
    admin = -hashed-6d3c30241ba0aaa4e16c6ea99224f915687ed8cd,7f4a3e05e0cbc6f48a0035e3508eef90
    architect = -pbkdf2-43ecbd256a70a3a2f7de40d2374b6c3002918834,921a12f74df0c1052b3e562a23cd227f,10000

Administrators can be added directly to the ``[admins]`` section, and when
CouchDB is restarted, the passwords will be salted and encrypted. By using
the HTTP, administrator accounts may be created immediately without needing
a restart, nor of storing the plaintext password temporarily. The HTTP
``_config/admins`` endpoint supports querying, deleting or creating new
administrator accounts:

.. code-block:: bash

    shell> GET /_config/admins HTTP/1.1
        Accept: application/json
        Host: localhost:5984

    HTTP/1.1 200 OK
        Cache-Control: must-revalidate
        Content-Length: 196
        Content-Type: application/json
        Date: Fri, 30 Nov 2012 11:37:18 GMT
        Server: CouchDB/1.3.0 (Erlang OTP/R15B02)

.. code-block:: json

        {
            "admin": "-hashed-6d3c30241ba0aaa4e16c6ea99224f915687ed8cd,7f4a3e05e0cbc6f48a0035e3508eef90",
            "architect": "-pbkdf2-43ecbd256a70a3a2f7de40d2374b6c3002918834,921a12f74df0c1052b3e562a23cd227f,10000"
        }

Further details are available in ``security_``, including configuring the
work factor for ``PBKDF2``, and the algorithm itself at
`PBKDF2 (RFC-2898) <http://tools.ietf.org/html/rfc2898>`_.

.. versionadded::
    1.3.0 ``PBKDF2`` server-side hashed salted password support added,
    now as a synchronous call for the ``_config/admins`` API.

OS Daemons
==========

CouchDB now supports starting external processes. The support is simple
and enables CouchDB to start each configured OS daemon. If the daemon
stops at any point, CouchDB will restart it (with protection to ensure
regularly failing daemons are not repeatedly restarted).

The daemon starting process is one-to-one; for each each configured
daemon in the configuration file, CouchDB will start exactly one
instance. If you need to run multiple instances, then you must create
separate individual configurations. Daemons are configured within the
``[os_daemons]`` section of your configuration file (``local.ini``). The
format of each configured daemon is:

.. code-block:: ini

    NAME = PATH ARGS

Where ``NAME`` is an arbitrary (and unique) name to identify the daemon;
``PATH`` is the full path to the daemon to be executed; ``ARGS`` are any
required arguments to the daemon.

For example:

.. code-block:: ini

    [os_daemons]
    basic_responder = /usr/local/bin/responder.js

There is no interactivity between CouchDB and the running process, but
you can use the OS Daemons service to create new HTTP servers and
responders and then use the new proxy service to redirect requests and
output to the CouchDB managed service. For more information on proxying,
see :ref:`http-proxying`. For further background on the OS Daemon service, see
`CouchDB Externals API`_.

.. _CouchDB Externals API: http://davispj.com/2010/09/26/new-couchdb-externals-api.html

Native SSL Support
==================

CouchDB |version| supports SSL natively. All your secure connection needs can
now be served without needing to setup and maintain a separate proxy server
that handles SSL.

SSL setup can be tricky, but the configuration in CouchDB was designed
to be as easy as possible. All you need is two files; a certificate and
a private key. If you bought an official SSL certificate from a
certificate authority, both should be in your possession already.

If you just want to try this out and don't want to pay anything upfront,
you can create a self-signed certificate. Everything will work the same,
but clients will get a warning about an insecure certificate.

You will need the OpenSSL command line tool installed. It probably
already is.

::

    shell> mkdir cert && cd cert
    shell> openssl genrsa > privkey.pem
    shell> openssl req -new -x509 -key privkey.pem -out mycert.pem -days 1095
    shell> ls
    mycert.pem privkey.pem

Now, you need to edit CouchDB's configuration, either by editing your
``local.ini`` file or using the ``/_config`` API calls or the
configuration screen in Futon. Here is what you need to do in
``local.ini``, you can infer what needs doing in the other places.

Be sure to make these edits. Under ``[daemons]`` you should see:

::

    ; enable SSL support by uncommenting the following line and supply the PEM's below.
    ; the default ssl port CouchDB listens on is 6984
    ;httpsd = {couch_httpd, start_link, [https]}

Here uncomment the last line:

::

    httpsd = {couch_httpd, start_link, [https]}

Next, under ``[ssl]`` you will see:

::

    ;cert_file = /full/path/to/server_cert.pem
    ;key_file = /full/path/to/server_key.pem

Uncomment and adjust the paths so it matches your system's paths:

::

    cert_file = /home/jan/cert/mycert.pem
    key_file = /home/jan/cert/privkey.pem

For more information please read
`http://www.openssl.org/docs/HOWTO/certificates.txt`_.

Now start (or restart) CouchDB. You should be able to connect to it
using HTTPS on port 6984:

::

    shell> curl https://127.0.0.1:6984/
    curl: (60) SSL certificate problem, verify that the CA cert is OK. Details:
    error:14090086:SSL routines:SSL3_GET_SERVER_CERTIFICATE:certificate verify failed
    More details here: http://curl.haxx.se/docs/sslcerts.html

    curl performs SSL certificate verification by default, using a "bundle"
    of Certificate Authority (CA) public keys (CA certs). If the default
    bundle file isn't adequate, you can specify an alternate file
    using the --cacert option.
    If this HTTPS server uses a certificate signed by a CA represented in
    the bundle, the certificate verification probably failed due to a
    problem with the certificate (it might be expired, or the name might
    not match the domain name in the URL).
    If you'd like to turn off curl's verification of the certificate, use
    the -k (or --insecure) option.

Oh no what happened?! — Remember, clients will notify their users that
your certificate is self signed. ``curl`` is the client in this case and
it notifies you. Luckily you trust yourself (don't you?) and you can
specify the ``-k`` option as the message reads:

::

    shell> curl -k https://127.0.0.1:6984/
    {"couchdb":"Welcome","version":"|version|"}

All done.

.. _`http://www.openssl.org/docs/HOWTO/certificates.txt`: http://www.openssl.org/docs/HOWTO/certificates.txt
