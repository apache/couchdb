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

.. warning::
    CouchDB 3.0+ will not run without an admin user being created first.
    Be sure to :ref:`create an admin user<config/admins>` before starting
    CouchDB!

.. _install/unix/binary:

Installation using the Apache CouchDB convenience binary packages
=================================================================

If you are running one of the following operating systems, the easiest way
to install CouchDB is to use the convenience binary packages:

* CentOS/RHEL 7
* CentOS/RHEL 8
* Debian 10 (buster)
* Debian 11 (bullseye)
* Ubuntu 18.04 (bionic)
* Ubuntu 20.04 (focal)

These RedHat-style rpm packages and Debian-style deb packages will install CouchDB at
``/opt/couchdb`` and ensure CouchDB is run at system startup by the appropriate init
subsystem (SysV-style initd or systemd).

The Debian-style deb packages *also* pre-configure CouchDB as a standalone or clustered
node, prompt for the address to which it will bind, and a password for the admin user.
Responses to these prompts may be pre-seeded using standard ``debconf`` tools. Further
details are in the `README.Debian`_ file.

.. _README.Debian: https://github.com/apache/couchdb-pkg/blob/main/debian/README.Debian

For distributions lacking a compatible SpiderMonkey library, Apache CouchDB
also provides packages for the 1.8.5 version.

Enabling the Apache CouchDB package repository
----------------------------------------------

.. highlight:: sh

**Debian or Ubuntu**: Run the following commands::

    sudo apt update && sudo apt install -y curl apt-transport-https gnupg
    curl https://couchdb.apache.org/repo/keys.asc | gpg --dearmor | sudo tee /usr/share/keyrings/couchdb-archive-keyring.gpg >/dev/null 2>&1
    source /etc/os-release
    echo "deb [signed-by=/usr/share/keyrings/couchdb-archive-keyring.gpg] https://apache.jfrog.io/artifactory/couchdb-deb/ ${VERSION_CODENAME} main" \
        | sudo tee /etc/apt/sources.list.d/couchdb.list >/dev/null

**RedHat or CentOS**: Run the following commands::

    sudo yum install -y yum-utils
    sudo yum-config-manager --add-repo https://couchdb.apache.org/repo/couchdb.repo

Installing the Apache CouchDB packages
--------------------------------------

.. highlight:: sh

**Debian or Ubuntu**: Run the following commands::

    sudo apt update
    sudo apt install -y couchdb

Debian/Ubuntu installs from binaries can be pre-configured for single node or
clustered installations. For clusters, multiple nodes will still need to be
joined together and configured consistently across all machines; **follow the**
:ref:`Cluster Setup <setup/cluster>` **walkthrough** to complete the process.

**RedHat/CentOS**: Run the command::

    sudo yum install -y couchdb

Once installed, :ref:`create an admin user<config/admins>` by hand before
starting CouchDB, if your installer didn't do this for you already.

You can now start the service.

**Your installation is not complete. Be sure to complete the**
:ref:`Setup <setup>` **steps for a single node or clustered installation.**

**Relax!** CouchDB is installed and running.

GPG keys used for signing the CouchDB repositories
--------------------------------------------------

As of 2021.04.25, the *repository* signing key for both types of supported packages
is::

    pub   rsa8192 2015-01-19 [SC]
          390EF70BB1EA12B2773962950EE62FB37A00258D
    uid           The Apache Software Foundation (Package repository signing key) <root@apache.org>

As of 2021.04.25, the *package* signing key (only used for ``rpm`` packages) is::

    pub   rsa4096 2017-07-28 [SC] [expires: 2022-07-27]
          2EC788AE3F239FA13E82D215CDE711289384AE37
    uid           Joan Touzet (Apache Code Signing Key) <wohali@apache.org>

As of 2021.11.13, the *package* signing key (only used for ``rpm`` packages) is::

     pub   rsa4096 2019-09-05 [SC] [expires: 2039-01-02]
           0BD7A98499C4AB41C910EE65FC04DFBC9657A78E
     uid           Nicolae Vatamaniuc <vatamane@apache.org>
     uid           default <vatamane@gmail.com>

All are available from most popular GPG key servers. The ``rpm``
signing keys should be listed in the `KEYS
<https://downloads.apache.org/couchdb/KEYS>`_ list as well.

Installation from source
========================

The remainder of this document describes the steps required to install CouchDB
directly from source code.

This guide, as well as the INSTALL.Unix document in the official tarball
release are the canonical sources of installation information. However, many
systems have gotchas that you need to be aware of. In addition, dependencies
frequently change as distributions update their archives.

.. _install/unix/dependencies:

Dependencies
============

You should have the following installed:

* `Erlang OTP (20.x >= 20.3.8.11, 21.x >= 21.2.3, 22.x >= 22.0.5, 23.x, 24.x) <http://erlang.org/>`_
* `ICU                          <http://icu-project.org/>`_
* `OpenSSL                      <http://www.openssl.org/>`_
* `Mozilla SpiderMonkey (1.8.5, 60, 68, 78, 91) <https://spidermonkey.dev/>`_
* `GNU Make                     <http://www.gnu.org/software/make/>`_
* `GNU Compiler Collection      <http://gcc.gnu.org/>`_
* `libcurl                      <http://curl.haxx.se/libcurl/>`_
* `help2man                     <http://www.gnu.org/s/help2man/>`_
* `Python (>=3.6) for docs and tests      <http://python.org/>`_
* `Python Sphinx (>=1.1.3)      <http://pypi.python.org/pypi/Sphinx>`_

You will only need libcurl if you plan to run the JavaScript test suite. And
help2man is only need if you plan on installing the CouchDB man pages.
Sphinx is only required for building the online documentation.
Documentation build can be disabled by adding the ``--disable-docs`` flag to
the ``configure`` script.

Debian-based Systems
--------------------

You can install the dependencies by running::

    sudo apt-get --no-install-recommends -y install \
        build-essential pkg-config erlang \
        libicu-dev libmozjs185-dev libcurl4-openssl-dev

Be sure to update the version numbers to match your system's available
packages.

RedHat-based (Fedora, CentOS, RHEL) Systems
-------------------------------------------

You can install the dependencies by running::

    sudo yum install autoconf autoconf-archive automake \
        curl-devel erlang-asn1 erlang-erts erlang-eunit gcc-c++ \
        erlang-os_mon erlang-xmerl erlang-erl_interface help2man \
        libicu-devel libtool perl-Test-Harness

Warning: To build a release for CouchDB the erlang-reltool package is required,
yet on CentOS/RHEL this package depends on erlang-wx which pulls in wxGTK
and several X11 libraries. If CouchDB is being built on a console only
server it might be a good idea to install this in a separate step to the
rest of the dependencies, so that the package and all its dependencies
can be removed using the ``yum history`` tool after the release is built.
(reltool is needed only during release build but not for CouchDB functioning)

The package can be installed by running::

    sudo yum install erlang-reltool

Mac OS X
--------

Follow :ref:`install/mac/homebrew` reference for Mac App installation.

If you are installing from source, you will need to install the Command
Line Tools::

    xcode-select --install

You can then install the other dependencies by running::

    brew install autoconf autoconf-archive automake libtool \
        erlang icu4c spidermonkey curl pkg-config

You will need `Homebrew` installed to use the ``brew`` command.

Some versions of Mac OS X ship a problematic OpenSSL library. If
you're experiencing troubles with CouchDB crashing intermittently with
a segmentation fault or a bus error, you will need to install your own
version of OpenSSL. See the wiki, mentioned above, for more information.

.. seealso::

    * `Homebrew <http://mxcl.github.com/homebrew/>`_

FreeBSD
-------

FreeBSD requires the use of GNU Make. Where ``make`` is specified in this
documentation, substitute ``gmake``.

You can install this by running::

    pkg install gmake

Installing
==========

Once you have satisfied the dependencies you should run::

    ./configure

If you wish to customize the installation, pass ``--help`` to this script.

If everything was successful you should see the following message::

    You have configured Apache CouchDB, time to relax.

Relax.

To build CouchDB you should run::

    make release

Try ``gmake`` if ``make`` is giving you any problems.

If include paths or other compiler options must be specified, they can be passed to rebar, which compiles CouchDB, with the ERL_CFLAGS environment variable. Likewise, options may be passed to the linker with the ERL_LDFLAGS environment variable::

    make release ERL_CFLAGS="-I/usr/local/include/js -I/usr/local/lib/erlang/usr/include"

If everything was successful you should see the following message::

    ... done
    You can now copy the rel/couchdb directory anywhere on your system.
    Start CouchDB with ./bin/couchdb from within that directory.

Relax.

Note: a fully-fledged ``./configure`` with the usual GNU Autotools options
for package managers and a corresponding ``make install`` are in
development, but not part of the 2.0.0 release.

.. _install/unix/security:

User Registration and Security
==============================

For OS X, in the steps below, substitute ``/Users/couchdb`` for
``/home/couchdb``.

You should create a special ``couchdb`` user for CouchDB.

On many Unix-like systems you can run::

    adduser --system \
            --shell /bin/bash \
            --group --gecos \
            "CouchDB Administrator" couchdb

On Mac OS X you can use the Workgroup Manager to create users up to version
10.9, and dscl or sysadminctl after version 10.9. Search Apple's support
site to find the documentation appropriate for your system. As of recent
versions of OS X, this functionality is also included in Server.app,
available through the App Store only as part of OS X Server.

You must make sure that the user has a working POSIX shell and a writable
home directory.

You can test this by:

* Trying to log in as the ``couchdb`` user
* Running ``pwd`` and checking the present working directory

As a recommendation, copy the ``rel/couchdb`` directory into
``/home/couchdb`` or ``/Users/couchdb``.

Ex: copy the built couchdb release to the new user's home directory::

    cp -R /path/to/couchdb/rel/couchdb /home/couchdb

Change the ownership of the CouchDB directories by running::

    chown -R couchdb:couchdb /home/couchdb

Change the permission of the CouchDB directories by running::

    find /home/couchdb -type d -exec chmod 0770 {} \;

Update the permissions for your ini files::

    chmod 0644 /home/couchdb/etc/*

First Run
=========

.. note::
    Be sure to :ref:`create an admin user<config/admins>` before trying to
    start CouchDB!

You can start the CouchDB server by running::

    sudo -i -u couchdb /home/couchdb/bin/couchdb

This uses the ``sudo`` command to run the ``couchdb`` command as the
``couchdb`` user.

When CouchDB starts it should eventually display following messages::

    {database_does_not_exist,[{mem3_shards,load_shards_from_db,"_users" ...

Don't be afraid, we will fix this in a moment.

To check that everything has worked, point your web browser to::

    http://127.0.0.1:5984/_utils/index.html

From here you should verify your installation by pointing your web browser to::

    http://localhost:5984/_utils/index.html#verifyinstall

**Your installation is not complete. Be sure to complete the**
:ref:`Setup <setup>` **steps for a single node or clustered installation.**

Running as a Daemon
===================

CouchDB no longer ships with any daemonization scripts.

The CouchDB team recommends `runit <http://smarden.org/runit/>`_ to
run CouchDB persistently and reliably. According to official site:

    *runit* is a cross-platform Unix init scheme with service supervision,
    a replacement for sysvinit, and other init schemes. It runs on
    GNU/Linux, \*BSD, MacOSX, Solaris, and can easily be adapted to
    other Unix operating systems.

Configuration of runit is straightforward; if you have questions, contact
the CouchDB `user mailing list <http://mail-archives.apache.org/mod_mbox/couchdb-user/>`_
or `IRC-channel #couchdb <http://webchat.freenode.net/?channels=#couchdb>`_
in FreeNode network.

Let's consider configuring runit on Ubuntu 18.04. The following
steps should be considered only as an example. Details will vary
by operating system and distribution. Check your system's package
management tools for specifics.

Install runit::

    sudo apt-get install runit

Create a directory where logs will be written::

    sudo mkdir /var/log/couchdb
    sudo chown couchdb:couchdb /var/log/couchdb

Create directories that will contain runit configuration for CouchDB::

    sudo mkdir /etc/sv/couchdb
    sudo mkdir /etc/sv/couchdb/log

Create /etc/sv/couchdb/log/run script::

    #!/bin/sh
    exec svlogd -tt /var/log/couchdb

Basically it determines where and how exactly logs will be written.
See ``man svlogd`` for more details.

Create /etc/sv/couchdb/run::

    #!/bin/sh
    export HOME=/home/couchdb
    exec 2>&1
    exec chpst -u couchdb /home/couchdb/bin/couchdb

This script determines how exactly CouchDB will be launched.
Feel free to add any additional arguments and environment
variables here if necessary.

Make scripts executable::

    sudo chmod u+x /etc/sv/couchdb/log/run
    sudo chmod u+x /etc/sv/couchdb/run

Then run::

    sudo ln -s /etc/sv/couchdb/ /etc/service/couchdb

In a few seconds runit will discover a new symlink and start CouchDB.
You can control CouchDB service like this::

    sudo sv status couchdb
    sudo sv stop couchdb
    sudo sv start couchdb

Naturally now CouchDB will start automatically shortly after system starts.

You can also configure systemd, launchd or SysV-init daemons to launch
CouchDB and keep it running using standard configuration files. Consult
your system documentation for more information.
