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

Installation from ports
=======================

.. code-block:: text

    cd /usr/ports/databases/couchdb
    make install clean

This will install CouchDB from the ports collection.

Start script
------------

The following options for ``/etc/rc.conf`` or ``/etc/rc.conf.local`` are
supported by the start script (defaults shown)::

    couchdb_enable="NO"
    couchdb_enablelogs="YES"
    couchdb_user="couchdb"

After enabling couchdb rc service use the following to start CouchDB::

    /usr/local/etc/rc.d/couchdb start

This script responds to the arguments `start`, `stop`, `status`, `rcvar` etc..

The start script will also use settings from the following config files:

- /usr/local/etc/couchdb/default.ini
- /usr/local/etc/couchdb/local.ini

Administrators should use ``default.ini`` as reference and only modify the
``local.ini`` file.

Post install
------------
In case the install script fails to install a noninteractive user "couchdb" to
be used for the database, the user needs to be created manually:

I used the ``pw`` command to add a user "couchdb" in group "couchdb":

.. code-block:: text

    pw user add couchdb
    pw user mod couchdb -c 'CouchDB, time to relax' -s /usr/sbin/nologin -d /var/lib/couchdb
    pw group add couchdb

The user is added to ``/etc/passwd`` and should look similar to the following:

.. code-block:: text

   shell#  grep couchdb /etc/passwd
   couchdb:*:1013:1013:Couchdb, time to relax:/var/lib/couchdb/:/usr/sbin/nologin

To change any of these settings, please refrain from editing `/etc/passwd` and
instead use ``pw user mod ...`` or ``vipw``. Make sure that the user has no
shell, but instead uses ``/usr/sbin/nologin``. The '*' in the second field means
that this user can not login via password authorization. For details use
`man 5 passwd`_.

.. _man 5 passwd: http://linux.die.net/man/5/passwd
