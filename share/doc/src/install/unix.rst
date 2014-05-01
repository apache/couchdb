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


.. _install/unix:

=================================
Installation on Unix-like systems
=================================

A high-level guide to Unix-like systems, inc. Mac OS X and Ubuntu.

This document is the canonical source of installation information. However, many
systems have gotchas that you need to be aware of. In addition, dependencies
frequently change as distributions update their archives. If you're running into
trouble, be sure to check out the wiki. If you have any tips to share, please
also update the wiki so that others can benefit from your experience.

.. seealso::

   `Community installation guides`_

.. _Community installation guides: http://wiki.apache.org/couchdb/Installation

Troubleshooting
---------------

* There is a `troubleshooting guide`_.
* There is a `wiki`_ for general documentation.
* There are collection of `friendly mailing lists`_.

Please work through these in order if you experience any problems.

.. _troubleshooting guide: http://wiki.apache.org/couchdb/Troubleshooting
.. _wiki: http://wiki.apache.org/couchdb
.. _friendly mailing lists: http://couchdb.apache.org/community/lists.html


.. _install/unix/dependencies:

Dependencies
------------

You should have the following installed:

* `Erlang OTP (>=R14B01, =<R17) <http://erlang.org/>`_
* `ICU                          <http://icu-project.org/>`_
* `OpenSSL                      <http://www.openssl.org/>`_
* `Mozilla SpiderMonkey (1.8.5) <http://www.mozilla.org/js/spidermonkey/>`_
* `GNU Make                     <http://www.gnu.org/software/make/>`_
* `GNU Compiler Collection      <http://gcc.gnu.org/>`_
* `libcurl                      <http://curl.haxx.se/libcurl/>`_
* `help2man                     <http://www.gnu.org/s/help2man/>`_
* `Python (>=2.7) for docs      <http://python.org/>`_
* `Python Sphinx (>=1.1.3)      <http://pypi.python.org/pypi/Sphinx>`_

It is recommended that you install Erlang OTP R13B-4 or above where possible.
You will only need libcurl if you plan to run the JavaScript test suite. And
help2man is only need if you plan on installing the CouchDB man pages.
Python and Sphinx are only required for building the online documentation.

Debian-based Systems
~~~~~~~~~~~~~~~~~~~~

You can install the dependencies by running::

    sudo apt-get install build-essential
    sudo apt-get install erlang-base-hipe
    sudo apt-get install erlang-dev
    sudo apt-get install erlang-manpages
    sudo apt-get install erlang-eunit
    sudo apt-get install erlang-nox
    sudo apt-get install libicu-dev
    sudo apt-get install libmozjs-dev
    sudo apt-get install libcurl4-openssl-dev

There are lots of Erlang packages. If there is a problem with your install, try
a different mix. There is more information on the wiki. Additionally, you might
want to install some of the optional Erlang tools which may also be useful.

Be sure to update the version numbers to match your system's available packages.

Unfortunately, it seems that installing dependencies on Ubuntu is troublesome.

.. seealso::

  * `Installing on Debian <http://wiki.apache.org/couchdb/Installing_on_Debian>`_
  * `Installing on Ubuntu <http://wiki.apache.org/couchdb/Installing_on_Ubuntu>`_


RedHat-based (Fedora, Centos, RHEL) Systems
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

You can install the dependencies by running::

    sudo yum install autoconf
    sudo yum install autoconf-archive
    sudo yum install automake
    sudo yum install curl-devel
    sudo yum install erlang-asn1
    sudo yum install erlang-erts
    sudo yum install erlang-eunit
    sudo yum install erlang-os_mon
    sudo yum install erlang-xmerl
    sudo yum install help2man
    sudo yum install js-devel
    sudo yum install libicu-devel
    sudo yum install libtool
    sudo yum install perl-Test-Harness

While CouchDB builds against the default js-devel-1.7.0 included in some
distributions, it's recommended to use a more recent js-devel-1.8.5.

Mac OS X
~~~~~~~~

Follow :ref:`install/mac/homebrew` reference till `brew install couchdb` step.


Installing
----------

Once you have satisfied the dependencies you should run::

    ./configure

This script will configure CouchDB to be installed into `/usr/local` by default.

If you wish to customise the installation, pass `--help` to this script.

If everything was successful you should see the following message::

    You have configured Apache CouchDB, time to relax.

Relax.

To install CouchDB you should run::

    make && sudo make install

You only need to use `sudo` if you're installing into a system directory.

Try `gmake` if `make` is giving you any problems.

If everything was successful you should see the following message::

    You have installed Apache CouchDB, time to relax.

Relax.

First Run
---------

You can start the CouchDB server by running::

    sudo -i -u couchdb couchdb

This uses the `sudo` command to run the `couchdb` command as the `couchdb` user.

When CouchDB starts it should eventually display the following message::

    Apache CouchDB has started, time to relax.

Relax.

To check that everything has worked, point your web browser to::

    http://127.0.0.1:5984/_utils/index.html

From here you should verify your installation by pointing your web browser to::

    http://localhost:5984/_utils/verify_install.html

Security Considerations
-----------------------

You should create a special `couchdb` user for CouchDB.

On many Unix-like systems you can run::

    adduser --system \
            --home /usr/local/var/lib/couchdb \
            --no-create-home \
            --shell /bin/bash \
            --group --gecos \
            "CouchDB Administrator" couchdb

On Mac OS X you can use the `Workgroup Manager`_ to create users.

You must make sure that:

* The user has a working POSIX shell
* The user's home directory is `/usr/local/var/lib/couchdb`

You can test this by:

* Trying to log in as the `couchdb` user
* Running `pwd` and checking the present working directory

Change the ownership of the CouchDB directories by running::

    chown -R couchdb:couchdb /usr/local/etc/couchdb
    chown -R couchdb:couchdb /usr/local/var/lib/couchdb
    chown -R couchdb:couchdb /usr/local/var/log/couchdb
    chown -R couchdb:couchdb /usr/local/var/run/couchdb

Change the permission of the CouchDB directories by running::

    chmod 0770 /usr/local/etc/couchdb
    chmod 0770 /usr/local/var/lib/couchdb
    chmod 0770 /usr/local/var/log/couchdb
    chmod 0770 /usr/local/var/run/couchdb

.. _Workgroup Manager: http://www.apple.com/support/downloads/serveradmintools1047.html


Running as a Daemon
-------------------

SysV/BSD-style Systems
~~~~~~~~~~~~~~~~~~~~~~

You can use the `couchdb` init script to control the CouchDB daemon.

On SysV-style systems, the init script will be installed into::

    /usr/local/etc/init.d

On BSD-style systems, the init script will be installed into::

    /usr/local/etc/rc.d

We use the `[init.d|rc.d]` notation to refer to both of these directories.

You can control the CouchDB daemon by running::

    /usr/local/etc/[init.d|rc.d]/couchdb [start|stop|restart|status]

If you wish to configure how the init script works, you can edit::

    /usr/local/etc/default/couchdb

Comment out the `COUCHDB_USER` setting if you're running as a non-superuser.

To start the daemon on boot, copy the init script to::

    /etc/[init.d|rc.d]

You should then configure your system to run the init script automatically.

You may be able to run::

    sudo update-rc.d couchdb defaults

If this fails, consult your system documentation for more information.

A `logrotate` configuration is installed into::

    /usr/local/etc/logrotate.d/couchdb

Consult your `logrotate` documentation for more information.

It is critical that the CouchDB logs are rotated so as not to fill your disk.
