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

.. _install/snap:

=====================
Installation via Snap
=====================

.. highlight:: sh

Apache CouchDB provides 'convenience binary' Snap builds through the
Ubuntu snapcraft repository under the name `couchdb snap`_. These are
available in separate snap channels for each major/minor release stream,
e.g., ``2.x``, ``3.3``, and a ``latest`` stream.

Once you've completed `installing snapd`_, you can install the CouchDB snap via::

    $ sudo snap install couchdb

After installation, set up an admin password and a cookie using a snap hook.
Then, restart the snap for changes to take effect::

    $ sudo snap set couchdb admin=[your-password] setcookie=[your-cookie]
    $ sudo snap restart couchdb

CouchDB will be installed (read only) at ``/snap/couchdb/current/``.
Data files will be written to ``/var/snap/couchdb/common/data``, and
(writable) configuration files will be stored in ``/var/snap/couchdb/current/etc``.

.. note::

    Your installation is not complete. Follow the
    :ref:`Setup <setup>` steps for a single node or clustered installation.

Snaps use AppArmor and are closely tied to systemd. They enforce that
only writable files are housed under ``/var/snap``. Ensure that ``/var``
has sufficient space for your data requirements.

To view logs, access them via ``journalctl snap.couchdb`` or using the ``snap logs``
command::

    $ sudo snap logs couchdb -f

When installing from a specific channel, snaps are automatically refreshed with
new revisions. Revert to a previous installation with::

    $ sudo snap revert couchdb

After this, updates will no longer be received. View installed snaps and alternative
channels using the list and info commands::

    $ snap list
    $ snap info couchdb

As easily as they are installed, snaps can be removed::

    $ sudo snap remove couchdb
    $ sudo snap remove couchdb --purge

The first command stops the server, removes couchdb from the list, and the filesystem
(keeping a backup for about 30 days if space permits). If you reinstall couchdb, it
tries to restore the backup. The second command removes couchdb and purges any backups.

When troubleshooting couchdb snap, check the logs first. You'll likely need to inspect
``/var/snap/couchdb/current/etc/local.ini`` to verify the data directory or modify
admin settings, port, or address bindings. Also, anything related to Erlang runtime
check ``/var/snap/couchdb/current/etc/vm.args`` to view the erlang name.

The most common issue is couchdb not finding the database files. Ensure that
``local.ini`` includes the following stanza and points to your data files:

.. code-block:: ini

    [couchdb]
    ;max_document_size = 4294967296 ; bytes
    ;os_process_timeout = 5000
    database_dir = /var/snap/couchdb/common/data
    view_index_dir = /var/snap/couchdb/common/data

.. note::

    Remember, you cannot modify the ``/snap/couchdb/`` directory, even with sudo,
    as the filesystem is mounted read-only for security reasons.

For additional details on the snap build process, refer to our
`couchdb-pkg git repository`_. This includes instructions on setting up a cluster
using the command line.

.. _couchdb snap: https://snapcraft.io/couchdb
.. _installing snapd: https://snapcraft.io/docs/core/install
.. _couchdb-pkg git repository: https://github.com/apache/couchdb-pkg
