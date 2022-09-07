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

.. _install/mac:

=====================
Installation on macOS
=====================

.. _install/mac/binary:

Installation using the Apache CouchDB native application
========================================================

The easiest way to run CouchDB on macOS is through the native macOS
application. Just follow the below instructions:

#. `Download Apache CouchDB for macOS`_.
   Old releases are available at `archive`_.
#. Double click on the Zip file
#. Drag and drop the Apache CouchDB.app into Applications folder

.. _Download Apache CouchDB for macOS: http://couchdb.apache.org/#download
.. _archive: http://archive.apache.org/dist/couchdb/binary/mac/

That's all, now CouchDB is installed on your Mac:

#. Run Apache CouchDB application
#. `Open up Fauxton`_, the CouchDB admin interface
#. Verify the install by clicking on `Verify`, then `Verify Installation`.
#. **Your installation is not complete. Be sure to complete the**
   :ref:`Setup <setup>` **steps for a single node or clustered installation.**
#. Time to Relax!

.. _Open up Fauxton: http://localhost:5984/_utils

.. _install/mac/homebrew:

Installation with Homebrew
==========================

CouchDB can be installed via `Homebrew`_.  Fetch the newest version of Homebrew
and all formulae and install CouchDB with the following commands:

.. code:: bash

   brew update
   brew install couchdb

.. _Homebrew: http://brew.sh/

Installation from source
========================

Installation on macOS is possible from source. Download the `source tarball`_,
extract it, and follow the instructions in the ``INSTALL.Unix.md`` file.

.. _source tarball: http://couchdb.apache.org/#download

Running as a Daemon
-------------------

CouchDB itself no longer ships with any daemonization scripts.

The CouchDB team recommends `runit <http://smarden.org/runit/>`_ to
run CouchDB persistently and reliably. Configuration of runit is
straightforward; if you have questions, reach out to the CouchDB
user mailing list.

Naturally, you can configure launchd or other init daemons to launch CouchDB
and keep it running using standard configuration files.

Consult your system documentation for more information.
