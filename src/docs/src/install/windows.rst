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

.. _install/windows:

=======================
Installation on Windows
=======================

There are two ways to install CouchDB on Windows.

Installation from binaries
==========================

This is the simplest way to go.

.. warning::
    Windows 8, 8.1, and 10 require the `.NET Framework v3.5`_ to be installed.

#. Get `the latest Windows binaries`_ from the `CouchDB web site`_.
   Old releases are available at `archive`_.

#. Follow the installation wizard steps. **Be sure to install CouchDB to a
   path with no spaces, such as** ``C:\CouchDB``.

#. **Your installation is not complete. Be sure to complete the**
   :ref:`Setup <setup>` **steps for a single node or clustered installation.**

#. `Open up Fauxton`_

#. It's time to Relax!

.. note::
    In some cases you might been asked to reboot Windows to complete
    installation process, because of using on different Microsoft Visual C++
    runtimes by CouchDB.

.. note::
    **Upgrading note**

    It's recommended to uninstall previous CouchDB version before upgrading,
    especially if the new one is built against different Erlang release.
    The reason is simple: there may be leftover libraries with alternative or
    incompatible versions from old Erlang release that may create conflicts,
    errors and weird crashes.

    In this case, make sure you backup of your `local.ini` config and CouchDB
    database/index files.

.. _Open up Fauxton: http://localhost:5984/_utils
.. _CouchDB web site: http://couchdb.apache.org/
.. _archive: http://archive.apache.org/dist/couchdb/binary/win/
.. _the latest Windows binaries: http://couchdb.apache.org/#download
.. _.NET Framework v3.5: https://docs.microsoft.com/en-us/dotnet/framework/install/dotnet-35-windows-10

.. _install/windows/silent:

Silent Install
--------------

The Windows installer supports silent installs. Here are some sample commands, supporting
the new features of the 3.0 installer.

Install CouchDB without a service, but with an admin user:password of ``admin:hunter2``:

.. code-block:: batch

    msiexec /i apache-couchdb-3.0.0.msi /quiet ADMINUSER=admin ADMINPASSWORD=hunter2 /norestart

The same as above, but also install and launch CouchDB as a service:

.. code-block:: batch

    msiexec /i apache-couchdb-3.0.0.msi /quiet INSTALLSERVICE=1 ADMINUSER=admin ADMINPASSWORD=hunter2 /norestart

Unattended uninstall of CouchDB to target directory `D:\CouchDB`:

.. code-block:: batch

    msiexec /x apache-couchdb-3.0.0.msi INSTALLSERVICE=1 APPLICATIONFOLDER="D:\CouchDB" ADMINUSER=admin ADMINPASSWORD=hunter2 /quiet /norestart

Unattended uninstall if the installer file is unavailable:

.. code-block:: batch

    msiexec /x {4CD776E0-FADF-4831-AF56-E80E39F34CFC} /quiet /norestart

Add ``/l* log.txt`` to any of the above to generate a useful logfile for debugging.

Installation from sources
=========================

.. seealso::
    `Glazier: Automate building of CouchDB from source on Windows
    <https://github.com/apache/couchdb-glazier>`_
