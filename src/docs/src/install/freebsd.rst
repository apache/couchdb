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

.. _install/freebsd:

=======================
Installation on FreeBSD
=======================

Installation
============

Use the pre-built binary packages to install CouchDB::

    pkg install couchdb3

Alternatively, it is possible installing CouchDB from the Ports
Collection::

    cd /usr/ports/databases/couchdb3
    make install clean

.. note::
   Be sure to :ref:`create an admin user<config/admins>` before starting
   CouchDB for the first time!

Service Configuration
=====================

The port is shipped with a script that integrates CouchDB with
FreeBSD's `rc.d service framework`_.  The following options for
``/etc/rc.conf`` or ``/etc/rc.conf.local`` are supported (defaults
shown)::

    couchdb3_enable="NO"
    couchdb3_user="couchdb"
    couchdb3_erl_flags="-couch_ini /usr/local/libexec/couchdb3/etc/default.ini /usr/local/etc/couchdb3/local.ini"
    couchdb3_chdir="/var/db/couchdb3"

After enabling the ``couchdb3`` service (by setting
``couchdb3_enable`` to ``"YES"``), use the following command to start
CouchDB::

    service couchdb3 start

This script responds to the arguments ``start``, ``stop``, ``status``,
``rcvar`` etc.  If the service is not yet enabled in ``rc.conf``, use
``onestart`` to start it up ad-hoc.

The service will also use settings from the following config files:

- ``/usr/local/libexec/couchdb3/etc/default.ini``
- ``/usr/local/etc/couchdb3/local.ini``

The ``default.ini`` should be left read-only, and will be replaced on
upgrades and re-installs without warning.  Therefore administrators
should use ``default.ini`` as a reference and only modify the
``local.ini`` file.

Post Install
============

**The installation is not complete. Be sure to complete the**
:ref:`Setup <setup>` **steps for a single node or clustered
installation.**

Also note that the port will probably show some messages after the
installation happened.  Make note of these instructions, although they
can be found `in the ports tree`_ for later reference.

.. _rc.d service framework: https://man.freebsd.org/rc.d
.. _in the ports tree: https://cgit.freebsd.org/ports/tree/databases/couchdb3/files/pkg-message.in
