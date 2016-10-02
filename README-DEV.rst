Apache CouchDB DEVELOPERS
=========================

Before you start here, read `INSTALL.Unix` (or `INSTALL.Windows`) and
follow the setup instructions including the installation of all the
listed dependencies for your system.

Only follow these instructions if you are building from a source checkout.

If you're unsure what this means, ignore this document.

Dependencies
------------

You may need:

* `Sphinx                 <http://sphinx.pocoo.org/>`_
* `LaTex                  <http://www.latex-project.org/>`_
* `GNU Texinfo            <http://www.gnu.org/software/texinfo/>`_
* `GNU help2man           <http://www.gnu.org/software/help2man/>`_
* `GnuPG                  <http://www.gnupg.org/>`_
* `md5sum                 <http://www.microbrew.org/tools/md5sha1sum/>`_
* `sha1sum                <http://www.microbrew.org/tools/md5sha1sum/>`_

The first of these optional dependencies are required for building the
documentation. The last three are needed to build releases.

You will need these optional dependencies installed if:

* You are working on the documentation, or
* You are preparing a distribution archive

However, you do not need them if:

* You are building from a distribution archive, or
* You don't care about building the documentation

If you intend to build Fauxton, you will also need to install its
dependencies. After running ``./configure`` to download all of the
dependent repositories, you can read about required dependencies in
`src/fauxton/readme.md`. Typically, installing npm and node.js are
sufficient to enable a Fauxton build.

Here is a list of *optional* dependencies for various operating systems.
Installation will be easiest, when you install them all.

Debian-based (inc. Ubuntu) Systems
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

::

    sudo apt-get install help2man python-sphinx \
        texlive-latex-base texlive-latex-recommended \
        texlive-latex-extra texlive-fonts-recommended texinfo gnupg

Gentoo-based Systems
~~~~~~~~~~~~~~~~~~~~

::

    sudo emerge texinfo gnupg coreutils pkgconfig help2man
    sudo USE=latex emerge sphinx

RedHat-based (Fedora, Centos, RHEL) Systems
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

::

    sudo yum install help2man python-sphinx python-docutils \
        python-pygments texlive-latex texlive-latex-fonts texinfo gnupg

Mac OS X
~~~~~~~~

Install `Homebrew <https://github.com/mxcl/homebrew>`_, if you do not have 
it already.

Unless you want to install the optional dependencies, skip to the next section.

Install what else we can with Homebrew::

    brew install help2man gnupg md5sha1sum

If you don't already have pip installed, install it::

    sudo easy_install pip

Now, install the required Python packages::

    sudo pip install sphinx
    sudo pip install docutils
    sudo pip install pygments

Download `MacTeX <http://www.tug.org/mactex/>`_ and follow the instructions 
to get a working LaTeX install on your system.

FreeBSD
~~~~~~~

::

    pkg install help2man texinfo gnupg py27-sphinx texlive-full tex-formats

Windows
~~~~~~~

Follow the instructions in `INSTALL.Windows` and build all components from
source, using the same Visual C++ compiler and runtime.

Configuring
-----------

Configure the source by running::

    ./configure

If you intend to run the test suites::

    ./configure -c

If you want to build it into different destination than `/usr/local`.::

    ./configure --prefix=/<your directory path>

If you don't want to build Fauxton or documentation specify
``--disable-fauxton`` and/or ``--disable-docs`` arguments for ``configure`` to
ignore their build and avoid any issues with their dependencies.

See ``./configure --help`` for more information.

Testing
-------

To run all the tests use run::

    make check

You can also run each test suite individually via ``eunit`` and ``javascript``
targets::

    make eunit
    make javascript

If you need to run specific Erlang tests, you can pass special "options"
to make targets::

    # Run tests only for couch and chttpd apps
    make eunit apps=couch,chttpd

    # Run only tests from couch_btree_tests suite
    make eunit suites=couch_btree_tests

    # Run only only specific tests
    make eunit tests=btree_open_test,reductions_test

    # Ignore tests for specified apps
    make eunit skip_deps=couch_log,couch_epi

The ``apps``, ``suites``, ``tests`` and ``skip_deps`` could be combined in any 
way. These are mimics to ``rebar eunit`` arguments. If you're not satisfied by 
these, you can use EUNIT_OPT environment variable to specify exact `rebar eunit`
options::

    make eunit EUNIT_OPTS="apps=couch,chttpd"

JavaScript tests accepts only `suites` option, but in the same way::

    # Run all JavaScript tests
    make javascript

    # Run only basic and design_options tests
    make javascript suites="basic design_options"

Note that tests are delimited here by whitespace, not by comma. You can get list
of all possible test targets with the following command::

    make list-js-suites

Code analyzer could be run by::

    make dialyze

If you need to analyze only specific apps, you can specify them in familiar way
::

    make dialyze apps=couch,couch_epi

See ``make help`` for more info and useful commands.

Please report any problems to the developer's mailing list.

Testing a cluster
-----------------

We use `Docker <https://docker.io>`_ to safely run a local three node
cluster all inside a single docker container.

Assuming you have Docker installed and running::

    make docker-image

This will create a docker image (tagged 'couchdb/dev-cluster') capable
of running a joined three node cluster.

To start it up::

    make docker-start

A three node cluster should now be running (you can now use ``docker ps``
to find the exposed ports of the nodes).

To stop it::

    make docker-stop

Releasing
---------

The release procedure is documented here::

    https://cwiki.apache.org/confluence/display/COUCHDB/Release+Procedure

Unix-like Systems
~~~~~~~~~~~~~~~~~

A release tarball can be built by running::

    make dist

An Erlang CouchDB release includes the full Erlang Run Time System and
all dependent applications necessary to run CouchDB, standalone. The
release created is completely relocatable on the file system, and is
the recommended way to distribute binaries of CouchDB. A release can be
built by running::

    make release

The release can then be found in the rel/couchdb directory.

Microsoft Windows
~~~~~~~~~~~~~~~~~

The release tarball and Erlang CouchDB release commands work on
Microsoft Windows the same as they do on Unix-like systems. To create
a full installer, the separate couchdb-glazier repository is required.
Full instructions are available in that repository's README file.

