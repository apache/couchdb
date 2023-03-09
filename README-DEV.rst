Apache CouchDB DEVELOPERS
=========================

Before you start here, read `<INSTALL.Unix.md>`_ (or
`<INSTALL.Windows.md>`_) and follow the setup instructions including
the installation of all the listed dependencies for your system.

Only follow these instructions if you are building from a source checkout.

If you're unsure what this means, ignore this document.

Dependencies
------------

You need the following to run tests:

* `Python 3               <https://www.python.org/>`_
* `Elixir                 <https://elixir-lang.org/>`_

You need the following optionally to build documentation:

* `Sphinx                 <http://sphinx.pocoo.org/>`_
* `GNU help2man           <http://www.gnu.org/software/help2man/>`_
* `GnuPG                  <http://www.gnupg.org/>`_

You need the following optionally to build releases:

* `md5sum                 <http://www.microbrew.org/tools/md5sha1sum/>`_
* `sha1sum                <http://www.microbrew.org/tools/md5sha1sum/>`_

You need the following optionally to build Fauxton:

* `nodejs                 <http://nodejs.org/>`_
* `npm                    <https://www.npmjs.com/>`_

You will need these optional dependencies installed if:

* You are working on the documentation, or
* You are preparing a distribution archive

However, you do not need them if:

* You are building from a distribution archive, or
* You don't care about building the documentation

If you intend to build Fauxton, you will also need to install its
dependencies. After running ``./configure`` to download all of the
dependent repositories, you can read about required dependencies in
`its readme
<https://github.com/apache/couchdb-fauxton/blob/main/readme.md>`_. Typically,
installing ``npm`` and node.js are sufficient to enable a Fauxton
build.

Here is a list of *optional* dependencies for various operating systems.
Installation will be easiest, when you install them all.

Docker
~~~~~~

CouchDB maintains a ``Dockerfile`` based on Debian that includes all
the dependencies noted above in the `.devcontainer <https://github.com/apache/couchdb/tree/main/.devcontainer>`_
folder.

The ``Dockerfile`` can be used on its own, or together with the
associated ``devcontainer.json`` file to quickly provision a
development environment using `GitHub Codespaces <https://github.com/features/codespaces>`_
or `Visual Studio Code <https://code.visualstudio.com/docs/remote/containers>`_.


.. image:: https://img.shields.io/static/v1?label=Remote%20-%20Containers&message=Open&color=blue&logo=visualstudiocode
    :target: https://vscode.dev/redirect?url=vscode://ms-vscode-remote.remote-containers/cloneInVolume?url=https://github.com/apache/couchdb

If you already have VS Code and Docker installed, you can click the
badge above or `here
<https://vscode.dev/redirect?url=vscode://ms-vscode-remote.remote-containers/cloneInVolume?url=https://github.com/apache/couchdb>`_
to get started. Clicking these links will cause VS Code to
automatically install the Remote - Containers extension if needed,
clone the source code into a container volume, and spin up a dev
container for use.

This ``devcontainer`` will automatically run ``./configure && make``
the first time it is created.  While this may take some extra time to
spin up, this tradeoff means you will be able to run things like
``./dev/run``, ``./dev/run --admin=admin:admin``, ``./dev/run
--with-admin-party-please``, and ``make check`` straight away.
Subsequent startups should be quick.


Debian-based (inc. Ubuntu) Systems
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

::

    sudo apt-get install help2man python-sphinx gnupg nodejs npm \
         python3 python3-venv

Gentoo-based Systems
~~~~~~~~~~~~~~~~~~~~

::

    sudo emerge gnupg coreutils pkgconfig help2man sphinx python
    sudo pip install hypothesis requests nose

Centos 7 and RHEL 7
~~~~~~~~~~~~~~~~~~~

::

    sudo yum install help2man python-sphinx python-docutils \
        python-pygments gnupg nodejs npm


Mac OS X
~~~~~~~~

Install `Homebrew <https://brew.sh/>`_, if you do not have it already.

Unless you want to install the optional dependencies, skip to the next section.

Install what else we can with Homebrew::

    brew install help2man gnupg md5sha1sum node python

If you don't already have pip installed, install it::

    sudo easy_install pip

Now, install the required Python packages::

    sudo pip install sphinx docutils pygments sphinx_rtd_theme

FreeBSD
~~~~~~~

::

    pkg install help2man gnupg py27-sphinx node
    pip install nose requests hypothesis

Windows
~~~~~~~

Follow the instructions in `<INSTALL.Windows.md>`_ and build all
components from source, using the same Visual C++ compiler and
runtime.

Configuring
-----------

Configure the source by running::

    ./configure

If you intend to run the test suites::

    ./configure -c

If you don't want to build Fauxton or documentation specify
``--disable-fauxton`` and/or ``--disable-docs`` arguments for ``configure`` to
ignore their build and avoid any issues with their dependencies.

See ``./configure --help`` for more information.

Developing
----------

Formatting
~~~~~~~~~~

The ``erl`` files in ``src`` are formatted using erlfmt_. The checks are run
for every PR in the CI. To run the checks locally, run ``make erlfmt-check``.
To format the ``erl`` files in ``src``, run ``make erlfmt-format``.
To use ``erlfmt`` for specific files only, use the executable ``bin/erlfmt``
that is installed by ``configure``.

Python files throughout the repository should conform to (PEP
8-compliant) formatting rules as specified by black_.  Similarly to
``erlfmt``, the related checks are run for every PR in the CI.  The
same checks could also be run locally via ``make python-black``.
Files can be automatically formatted by ``make python-black-update``.

.. _erlfmt: https://github.com/WhatsApp/erlfmt
.. _black: https://github.com/psf/black

Testing
-------

To run all the tests use run::

    make check

You can also run each test suite individually via the ``eunit``, ``mango-test``,
``elixir-suite``, and ``weatherreport-test`` targets::

    make eunit
    make mango-test
    make elixir-suite
    make weatherreport-test

Erlang Unit Tests
~~~~~~~~~~~~~~~~~

If you need to run specific Erlang tests, you can pass special "options"
to make targets::

    # Run tests only for couch and chttpd apps
    make eunit apps=couch,chttpd

    # Run only tests from couch_btree_tests suite
    make eunit apps=couch suites=couch_btree

    # Run only only specific tests
    make eunit tests=btree_open_test,reductions_test

    # Ignore tests for specified apps
    make eunit skip_deps=couch_log,couch_epi

The ``apps``, ``suites``, ``tests`` and ``skip_deps`` could be
combined in any way. These are mimics to ``rebar eunit`` arguments. If
you're not satisfied by these, you can use the ``EUNIT_OPTS`` variable
to specify exact ``rebar eunit`` options::

    make eunit EUNIT_OPTS="apps=couch,chttpd"

Elixir Integration Tests
~~~~~~~~~~~~~~~~~~~~~~~~

All the Elixir-based integration tests could be by the `elixir-suite`
target::

    make elixir-suite

There is an additional suite for Dreyfus, which is not run
automatically by either the ``elixir-suite`` or the ``check`` target
but it could be done manually via the corresponding target::

    make elixir-search

Note that this requires a running Clouseau instance with the name
``clouseau@127.0.0.1``.  The easiest way to get it is to clone the
`cloudant-labs/clouseau <https://github.com/cloudant-labs/clouseau>`_
repository and launch it run there once all the prerequisites (JDK,
Scala, and Maven) have been installed successfully, e.g.::

    git clone https://github.com/cloudant-labs/clouseau
    mvn -f clouseau/pom.xml scala:run

Mango Integration Tests
~~~~~~~~~~~~~~~~~~~~~~~

Tests for the Mango interface can be run individually with the help of
the ``mango-test`` target and they can be narrowed down to specific
test suites via the ``MANGO_TEST_OPTS`` variable::

    make mango-test \
      MANGO_TEST_OPTS="--pretty-assert --verbose 03-operator-test"

The value of the ``MANGO_TEST_OPTS`` variable will be passed down to
the `Nose 2 <https://nose2.io/>`_ testing framework which is used for
the implementation.  Consult its documentation for more information.

Tests that rely on text indexes are run only if the ``search`` feature
is reported to be available (i.e. a working Clouseau instance is
connected), otherwise they will be skipped.

Note that the databases that are created during the tests will be all
removed after each of the suites completed.  However, with the help of
the ``MANGO_TESTS_KEEP_DBS`` environment variable, it can be requested
to keep those databases around for further investigation::

    MANGO_TESTS_KEEP_DBS=please \
      make mango-test MANGO_TEST_OPTS='03-operator-test'

Static Code Analysis
~~~~~~~~~~~~~~~~~~~~

Code analyzer could be run by::

    make dialyze

If you need to analyze only specific apps, you can specify them in familiar way
::

    make dialyze apps=couch,couch_epi

See ``make help`` for more info and useful commands.

Please report any problems to the `developer's mailing list
<https://lists.apache.org/list.html?dev@couchdb.apache.org>`_.

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

The release can then be found in the ``rel/couchdb`` directory.

Microsoft Windows
~~~~~~~~~~~~~~~~~

The release tarball and Erlang CouchDB release commands work on
Microsoft Windows the same as they do on Unix-like systems. To create
a full installer, the separate `couchdb-glazier repository
<https://github.com/apache/couchdb-glazier>`_ is required.  Full
instructions are available in that repository's ``README`` file.
