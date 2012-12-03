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
