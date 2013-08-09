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

========================
Installation on Mac OS X
========================


.. _install/mac/binary:

Installation using the Apache CouchDB native application
========================================================

The easiest way to run CouchDB on Mac OS X is through his native Mac OS X application.
Just follow the below instructions:

#. `Download Apache CouchDB for Mac OS X`_.
   Old releases are available at `archive`_.
#. Double click on the Zip file
#. Drag and drop the Apache CouchDB.app into Applications folder

.. _Download Apache CouchDB for Mac OS X: http://couchdb.org/#download
.. _archive: http://archive.apache.org/dist/couchdb/binary/mac/

That's all, now CouchDB is installed on your Mac:

#. Run Apache CouchDB application
#. `Open up Futon`_, the CouchDB admin interface
#. Time to Relax!

.. _Open up Futon: http://localhost:5984/_utils


.. _install/mac/macports:

Installation from MacPorts
==========================

To install CouchDB using MacPorts you have 2 package choices:

- ``couchdb`` - the latest release version
- ``couchdb-devel`` - updated every few weeks from the master branch

::

  $ sudo port install couchdb
  
or, if you want the last development version:

  $ sudo port install couchdb-devel

MacPorts takes care of installing all necessary dependencies.
It should now be up and accessible via Futon at http://127.0.0.1:5984/_utils.

If you want to run CouchDB as a service, load the launchd configuration which comes 
with the project, with this command::

  $ sudo port load couchdb

Now CouchDB should be started automatically after every reboot.


.. _install/mac/homebrew:

Installation with HomeBrew
==========================

You can install the build tools by running::

    open /Applications/Installers/Xcode\ Tools/XcodeTools.mpkg

You will need `Homebrew`_ installed to use the `brew` command. To install the
other :ref:`dependencies <install/unix/dependencies>` run next commands::

    brew install autoconf
    brew install autoconf-archive
    brew install automake
    brew install libtool
    brew install erlang
    brew install icu4c
    brew install spidermonkey
    brew install curl

You may want to link ICU so that CouchDB can find the header files
automatically::

    brew link icu4c

The same is true for recent versions of Erlang::

    brew link erlang

Now it's time to brew CouchDB::

    brew install couchdb


The above Erlang install will use the bottled (pre-compiled) version if you are:
using `/usr/local` for `homebrew`, and on 10.6 or 10.7. If you're not on one of
these, `homebrew` will build from source, so consider doing::

  brew install erlang --no-docs

to trim down compilation time.

If you're hacking on CouchDB, and we hope you will, you may try the current
git-based master (head) branch, or the next development release using this
``couchdb`` recipe, using either ``--head`` or ``--devel`` options respectively.
This will allow quick installation of the future release branch when it becomes
active. If you're not sure if you need this, then you probably don't.
In both cases we assume you are comfortable identifying bugs, and handling any
potential upgrades between commits to the codebase.

::

  brew install [--devel|--head] couchdb

.. note::

   OS X Lion might hang on the final brew.
   See the thread at https://github.com/mxcl/homebrew/issues/7024 it seems in
   most cases to be resolved by breaking out with ``CTRL-C`` and then repeating
   with ``brew install -v couchdb``.

If you wish to have CouchDB run as a daemon then, set up the account,
using the "User & Groups" preference pane:

- Create a standard user `couchdb` with home directory as
  `/usr/local/var/lib/couchdb`

- Create a group called `couchdb` and add yourself, the `couchdb` user, and any
  others you want to be able to edit config or db files directly to it.
  Use the `advanced` group options to ensure the internal name is also correctly
  called `couchdb`.

Some versions of Mac OS X ship a problematic OpenSSL library. If you're
experiencing troubles with CouchDB crashing intermittently with a segmentation
fault or a bus error, you will need to install your own version of OpenSSL.

.. _Homebrew: http://mxcl.github.com/homebrew/


Running as a Daemon
-------------------

You can use the `launchctl` command to control the CouchDB daemon.

You can load the configuration by running::

    sudo launchctl load \
         /usr/local/Library/LaunchDaemons/org.apache.couchdb.plist

You can stop the CouchDB daemon by running::

    sudo launchctl unload \
         /usr/local/Library/LaunchDaemons/org.apache.couchdb.plist

You can start CouchDB by running::

    sudo launchctl start org.apache.couchdb

You can restart CouchDB by running::

    sudo launchctl stop org.apache.couchdb

You can edit the launchd configuration by running::

    open /usr/local/Library/LaunchDaemons/org.apache.couchdb.plist

To start the daemon on boot, copy the configuration file to::

    /Library/LaunchDaemons

Consult your system documentation for more information.
