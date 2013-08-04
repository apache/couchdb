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

This is the most simplest way to go.

#. Get `the latest Windows binaries`_ from `CouchDB web site`_.
   Old releases are available at `archive`_.

#. Follow the installation wizard steps:

   - Next on "Welcome" screen
   - Accept the License agreement
   - Select the installation directory
   - Specify "Start Menu" group name
   - Approve that you'd like to install CouchDB as service and let him been
     started automatically after installation (probably, you'd like so)
   - Verify installation settings
   - Install CouchDB

#. `Open up Futon`_ (if you hadn't selected autostart CouchDB after
   installation, you have to start it first manually)

#. It's time to Relax!

.. note::
   In some cases you might been asked to reboot Windows to complete
   installation process, because of using on different Microsoft Visual C++
   runtimes by CouchDB.

.. note:: **Upgrading note**

   It's very recommended to uninstall previous CouchDB version before upgrading,
   especially if the new one is built against different Erlang release.
   The reason is simple: there may left a some of libraries with alternative or
   incompatible versions from old Erlang release that may create conflicts,
   errors and weird crushes.

   In this case, make sure you'd backup of your `local.ini` config and CouchDB
   database/index files.

.. _Open up Futon: http://localhost:5984/_utils
.. _CouchDB web site: http://couchdb.org/
.. _archive: http://archive.apache.org/dist/couchdb/binary/win/
.. _the latest Windows binaries: http://couchdb.org/#download


Installation from sources
=========================

If you're Windows geek, this section is for you!

Troubleshooting
---------------

* There is a `troubleshooting guide`_.
* There is a `wiki`_ for general documentation.
* And some `Windows-specific tips`_.
* There are collection of `friendly mailing lists`_.

Please work through these in order if you experience any problems.

.. _troubleshooting guide: http://wiki.apache.org/couchdb/Troubleshooting
.. _wiki: http://wiki.apache.org/couchdb
.. _friendly mailing lists: http://couchdb.apache.org/community/lists.html
.. _Windows-specific tips: http://wiki.apache.org/couchdb/Quirks_on_Windows

Dependencies
------------

You should have the following installed:

* `Erlang OTP (>=14B01, <R17)    <http://erlang.org/>`_
* `ICU        (>=4.*)            <http://icu-project.org/>`_
* `OpenSSL    (>0.9.8r)          <http://www.openssl.org/>`_
* `Mozilla SpiderMonkey (=1.8.5) <http://www.mozilla.org/js/spidermonkey/>`_
* `Cygwin                        <http://www.cygwin.com/>`_
* `Microsoft SDK 7.0 or 7.1      <http://www.microsoft.com/en-us/download/details.aspx?id=8279>`_
* `libcurl    (>=7.20)           <http://curl.haxx.se/libcurl/>`_
* `help2man                      <http://www.gnu.org/s/help2man/>`_
* `Python (>=2.7) for docs       <http://python.org/>`_
* `Python Sphinx (>=1.1.3)       <http://pypi.python.org/pypi/Sphinx>`_

You will only need libcurl if you plan to run the JavaScript test suite. And
help2man is only need if you plan on installing the CouchDB man pages.
Python and Sphinx are only required for building the online documentation.

General Notes
-------------

* When installing Cygwin, be sure to select all the `development` tools.

* When installing Erlang, you must build it from source.

* The CouchDB build requires a number of the Erlang build scripts.

* All dependent libraries should be built with the same version of
  Microsoft SDK.

* Do not try to link against libraries built with, or included in,
  Cygwin or MingW. They are not compatible with the Erlang/OTP or CouchDB
  build scripts.

* ICU version 4.6 and later will build cleanly using MSBuild.

* Python and Sphinx are optional for building the online documentation.
  Use cygwin-provided Python and install Sphinx via easy_install or pip.
  Further information is here http://pypi.python.org/pypi/setuptools#id4

Setting Up Cygwin
-----------------

Before starting any Cygwin terminals, run::

    set CYGWIN=nontsec

To set up your environment, run::

    [VS_BIN]/vcvars32.bat

Replace ``[VS_BIN]`` with the path to your Visual Studio `bin` directory.

You must check that:

* The ``which link`` command points to the Microsoft linker.

* The ``which cl`` command points to the Microsoft compiler.

* The ``which mc`` command points to the Microsoft message compiler.

* The ``which mt`` command points to the Microsoft manifest tool.

* The ``which nmake`` command points to the Microsoft make tool.

If you do not do this, the build may fail due to Cygwin ones found in `/usr/bin`
being used instead.

Building Erlang
---------------

You must include Win32 OpenSSL, built statically from source. Use
exactly the same version as required by the Erlang/OTP build process.

However, you can skip the GUI tools by running::

   echo "skipping gs" > lib/gs/SKIP

   echo "skipping ic" > lib/ic/SKIP

   echo "skipping jinterface" > lib/jinterface/SKIP

Follow the rest of the Erlang instructions as described.

After running::

   ./otp_build release -a

You should run::

   ./release/win32/Install.exe -s

This will set up the release/win32/bin directory correctly. The CouchDB
installation scripts currently write their data directly into this
location.

To set up your environment for building CouchDB, run::

    eval `./otp_build env_win32`

To set up the `ERL_TOP` environment variable, run::

    export ERL_TOP=[ERL_TOP]

Replace ``[ERL_TOP]`` with the Erlang source directory name.

Remember to use `/cygdrive/c/` instead of `c:/` as the directory prefix.

To set up your path, run::

    export PATH=$ERL_TOP/release/win32/erts-5.8.5/bin:$PATH

If everything was successful, you should be ready to build CouchDB.

Relax.

Building CouchDB
----------------

Note that `win32-curl` is only required if you wish to run the developer
tests.

The documentation step may be skipped using ``--disable-docs`` if you wish.

Once you have satisfied the dependencies you should run::

    ./configure \
        --with-js-include=/cygdrive/c/path_to_spidermonkey_include \
        --with-js-lib=/cygdrive/c/path_to_spidermonkey_lib \
        --with-win32-icu-binaries=/cygdrive/c/path_to_icu_binaries_root \
        --with-erlang=$ERL_TOP/release/win32/usr/include \
        --with-win32-curl=/cygdrive/c/path/to/curl/root/directory \
        --with-openssl-bin-dir=/cygdrive/c/openssl/bin \
        --with-msvc-redist-dir=/cygdrive/c/dir/with/vcredist_platform_executable \
        --disable-init \
        --disable-launchd \
        --prefix=$ERL_TOP/release/win32

This command could take a while to complete.

If everything was successful you should see the following message::

    You have configured Apache CouchDB, time to relax.

Relax.

To install CouchDB you should run::

    make install

If everything was successful you should see the following message::

    You have installed Apache CouchDB, time to relax.

Relax.

To build the .exe installer package, you should run::

    make dist

Alternatively, you may run CouchDB directly from the build tree, but
to avoid any contamination do not run `make dist` after this.

First Run
---------

You can start the CouchDB server by running::

    $ERL_TOP/release/win32/bin/couchdb.bat

When CouchDB starts it should eventually display the following message::

    Apache CouchDB has started, time to relax.

Relax.

To check that everything has worked, point your web browser to::

    http://127.0.0.1:5984/_utils/index.html

From here you should run the verification tests in Firefox.


.. seealso::

   `Glazier: Automate building of CouchDB from source on Windows <https://github.com/dch/glazier>`_
