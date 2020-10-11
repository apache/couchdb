# Apache CouchDB INSTALL.Unix

A high-level guide to Unix-like systems, inc. Mac OS X and Ubuntu.

Community installation guides are available on the wiki:

    http://wiki.apache.org/couchdb/Installation

If you are trying to build CouchDB from a git checkout rather than
a .tar.gz, see the `DEVELOPERS` file.

This document is the canonical source of installation
information. However, many systems have gotchas that you need to be
aware of. In addition, dependencies frequently change as distributions
update their archives. If you're running into trouble, be sure to
check out the wiki. If you have any tips to share, please also update
the wiki so that others can benefit from your experience.

## Troubleshooting

There is a troubleshooting guide:

    http://wiki.apache.org/couchdb/Troubleshooting

There is a wiki for general documentation:

    http://wiki.apache.org/couchdb/

There are collection of friendly mailing lists:

    http://couchdb.apache.org/community/lists.html

Please work through these in order if you experience any problems.

## Dependencies

You should have the following installed:

 * Erlang OTP (>=R16B03-1, =<19.x) (http://erlang.org/)
 * ICU                          (http://icu-project.org/)
 * OpenSSL                      (http://www.openssl.org/)
 * Mozilla SpiderMonkey - either 1.8.5 or 60
   * 60 is not supported on ARM 64-bit (aarch64) at this time.
   * https://developer.mozilla.org/en/docs/Mozilla/Projects/SpiderMonkey/Releases/1.8.5
   * https://archive.mozilla.org/pub/firefox/releases/60.9.0esr/source/ (src/js)
 * GNU Make                     (http://www.gnu.org/software/make/)
 * GNU Compiler Collection      (http://gcc.gnu.org/)
 * libcurl                      (http://curl.haxx.se/libcurl/)
 * help2man                     (http://www.gnu.org/s/help2man/)
 * Python (>=2.7)               (http://python.org/)

To build Fauxton, you should have the following installed:
 * Node.JS (>=6.x)              (https://nodejs.org/)
   -- obtainable from NodeSource (https://github.com/nodesource/distributions)

To build the documentation, you should have the following installed:
 * Python Sphinx (>=1.5)        (http://pypi.python.org/pypi/Sphinx)

It is recommended that you install Erlang OTP R16B03-1 or above where
possible.  You will only need libcurl if you plan to run the
JavaScript test suite. And help2man is only need if you plan on
installing the CouchDB man pages.  Python and Sphinx are only required
for building the online documentation. Documentation build can be disabled
by adding the `--disable-docs` flag to the `configure` script.

### Debian-based Systems

You can install the dependencies by running:

    sudo apt-get --no-install-recommends -y install \
        build-essential pkg-config erlang erlang-reltool \
        libicu-dev libmozjs185-dev libcurl4-openssl-dev

You can install the Node.JS dependencies via [NodeSource](https://github.com/nodesource/distributions#installation-instructions).

You can install the documentation dependencies by running:

    sudo apt-get --no-install-recommends -y install \
        python-sphinx
        
    sudo pip install --upgrade sphinx_rtd_theme nose requests hypothesis


Be sure to update the version numbers to match your system's available
packages.

### RedHat-based (Fedora, Centos, RHEL) Systems

You can install the dependencies by running:

    sudo yum install autoconf autoconf-archive automake \
        curl-devel erlang-asn1 erlang-erts erlang-eunit \
        erlang-xmerl help2man \
        js-devel-1.8.5 libicu-devel libtool perl-Test-Harness

You can install the Node.JS dependencies via [NodeSource](https://github.com/nodesource/distributions#rpminstall).

The built-in packages for Sphinx in RHEL repositories are too old
to run the documentation build process. Instead, use pip:

    sudo yum install python-pip
    sudo pip install sphinx

### Mac OS X

To build CouchDB from source on Mac OS X, you will need to install
the Command Line Tools:

    xcode-select --install

You can then install the other dependencies by running:

    brew install autoconf autoconf-archive automake libtool \
        erlang icu4c spidermonkey curl pkg-config

You can install the Node.JS dependencies via the
[official Macintosh installer](https://nodejs.org/en/download/).

You can install the documentation dependencies by running:

    sudo easy_install pip
    sudo pip install sphinx

You will need Homebrew installed to use the `brew` command.

Learn more about Homebrew at:

    http://mxcl.github.com/homebrew/

Some versions of Mac OS X ship a problematic OpenSSL library. If
you're experiencing troubles with CouchDB crashing intermittently with
a segmentation fault or a bus error, you will need to install your own
version of OpenSSL. See the wiki, mentioned above, for more information.

### FreeBSD

FreeBSD requires the use of GNU Make. Where `make` is specified in this
documentation, substitute `gmake`.

You can install this by running:

    pkg install gmake

You can install the remaining dependencies by running:

    pkg install help2man openssl icu curl git bash \
        autoconf automake libtool node spidermonkey185 \
        erlang node8 npm-node8 lang/python py27-sphinx py27-pip
    pip install --upgrade sphinx_rtd_theme nose requests hypothesis

## Installing

Once you have satisfied the dependencies you should run:

    ./configure

If you wish to customize the installation, pass `--help` to this
script.

If everything was successful you should see the following message:

    You have configured Apache CouchDB, time to relax.

Relax.

To build CouchDB you should run:

    make release

Try `gmake` if `make` is giving you any problems.

If everything was successful you should see the following message:

    ... done
    You can now copy the rel/couchdb directory anywhere on your system.
    Start CouchDB with ./bin/couchdb from within that directory.

Relax.

## User Registration

For OS X, in the steps below, substitute `/Users/couchdb` for `/home/couchdb`.

You should create a special `couchdb` user for CouchDB.

On many Unix-like systems you can run:

    adduser --system \
            --home /opt/couchdb \
            --no-create-home \
            --shell /bin/bash \
            --group --gecos \
            "CouchDB Administrator" couchdb

On Mac OS X you can use the Workgroup Manager to create users up to version
10.9, and dscl or sysadminctl after version 10.9. Search Apple's support
site to find the documentation appropriate for your system. As of recent
versions of OS X, this functionality is also included in Server.app,
available through the App Store only as part of OS X Server.

You must make sure that the user has a working POSIX shell.

You can test this by:

    * Trying to log in as the `couchdb` user

    * Running `pwd` and checking the present working directory

Copy the built couchdb release to the new user's home directory:

    cp -R /path/to/couchdb/rel/couchdb /opt/couchdb

Change the ownership of the CouchDB directories by running:

    chown -R couchdb:couchdb /opt/couchdb

Change the permission of the CouchDB directories by running:

    find /opt/couchdb -type d -exec chmod 0770 {} \;

Update the permissions for your ini files:

    chmod 0644 /opt/couchdb/etc/*

## First Run

You can start the CouchDB server by running:

    sudo -i -u couchdb couchdb/bin/couchdb

This uses the `sudo` command to run the `couchdb` command as the
`couchdb` user.

When CouchDB starts it should eventually display the following
message:

    Apache CouchDB has started, time to relax.

Relax.

To check that everything has worked, point your web browser to:

    http://127.0.0.1:5984/_utils/

From here you should verify your installation by pointing your web browser to:

    http://localhost:5984/_utils/#/verifyinstall

## Running as a Daemon

The couchdb team recommends [runit](http://smarden.org/runit/) to
run CouchDB persistently and reliably. Configuration of runit is
straightforward; if you have questions, reach out to the CouchDB
user mailing list.

Naturally, you can configure systemd, launchd or SysV-init daemons to
launch CouchDB and keep it running using standard configuration files.
Sample scripts are in the couchdb-pkg repository:

* SysV-init (Debian-style): https://github.com/apache/couchdb-pkg/blob/master/debian/couchdb.init
* SysV-init (RHEL-style): https://github.com/apache/couchdb-pkg/blob/master/rpm/SOURCES/couchdb.init
* upstart: Use the Debian-style sysvinit script instead.
* systemd: https://github.com/apache/couchdb-pkg/blob/master/debian/couchdb.service

Consult your system documentation for more information.
