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

* `Sphinx                 <https://www.sphinx-doc.org/>`_
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

If you intend to run the test suites with Clouseau::

    ./configure --enable-clouseau

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
``elixir``, and ``weatherreport-test`` targets::

    make eunit
    make mango-test
    make elixir
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

All the Elixir-based integration tests could be by the `elixir`
target::

    make elixir

There is an additional suite for Dreyfus, which is not run
automatically by either the ``elixir`` or the ``check`` target
but it could be done manually via the corresponding target::

    make elixir-search

Note that this requires Clouseau to be configured for running, see
above.

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
configured and working), otherwise they will be skipped.

Note that the databases that are created during the tests will be all
removed after each of the suites completed.  However, with the help of
the ``MANGO_TESTS_KEEP_DBS`` environment variable, it can be requested
to keep those databases around for further investigation::

    MANGO_TESTS_KEEP_DBS=please \
      make mango-test MANGO_TEST_OPTS='03-operator-test'

Running Clouseau
~~~~~~~~~~~~~~~~

When configured with the ``./configure`` script, the ``./dev/run``
script is capable of launching Clouseau instances alongside the
CouchDB nodes and hooking them up.  This is what the ``mango-test``
and ``elixir-search`` targets also use to run their respective test
suites, and let Clouseau automatically manage them.

Although the ``./configure`` and the ``./dev/run`` scripts try to take
care of the details of the Clouseau deployment, it is still the
responsibility of the user to provide a suitable Java environment for
running.  Clouseau can run with JRE 1.7 and 1.8 only.  Also, when
Nouveau is in use, which uses a more recent Java environment, the old
JDK has to be installed separately and the ``CLOUSEAU_JAVA_HOME``
environment variable has to be set to point its location.

Fortunately, the ```asdf`` tool <https://asdf-vm.com/>` provides a
convenient way to install old versions of JDK through its ```java``
plugin <https://github.com/halcyon/asdf-java>`::

    asdf plugin add java

Then use ``asdf`` to install it::

    asdf install java zulu-jre-8.74.0.17

Finally, use ``asdf`` to set the ``CLOUSEAU_JAVA_HOME`` environment
variable::

    export CLOUSEAU_JAVA_HOME=$(asdf where java zulu-jre-8.74.0.17)

If the use of ``asdf`` is not an option, `the Zulu site
<https://cdn.azul.com/zulu/bin/>` could be used directly to get the
distribution package for the appropriate JRE version.  But this is
just one of the possibilities to access installers for old Java
environments.

Once both Clouseau and the corresponding Java environment are set,
they are not put in use automatically.  In order to do so, the
``./dev/run`` script needs to be run with Clouseau enabled as
follows::

    dev/run --with-clouseau

When a specific Erlang cookie string is set in
``rel/overlay/etc/vm.args``, the ``--erlang-cookie`` flag could be
used to configure Clouseau to work with that::

    dev/run --with-clouseau --erlang-cookie=brumbrum

It is possible to override Clouseau's location per invocation of
``./dev/run`` in case some other version needs to be exercised for the
moment.  This can be done with the help of the ``--clouseau-dir``
flag.  The specified location could be either an unpacked bundle of
JAR files or a git clone of the Clouseau source code repository::

    dev/run --with-clouseau --clouseau-dir $HOME/git/clouseau.wip

Through the ``CLOUSEAU_DIR`` variable the same could be forwarded to the
respective test targets, e.g. ``mango-test``::

    make mango-test CLOUSEAU_DIR=$HOME/git/clouseau.wip

This can even be done if there was no local Clouseau deployment
configured previously.  Mind that this will require building Clouseau
from source, which causes the nodes start up somewhat slower.  It also
requires JDK 1.7 and Apache Maven 3.8 to be present, that is why it is
important the set the ``CLOUSEAU_JAVA_HOME`` and the
``CLOUSEAU_M2_HOME`` environment variables accordingly, for instance::

    asdf install java zulu-7.56.0.11
    asdf plugin add maven
    asdf install maven 3.8.8
    export CLOUSEAU_JAVA_HOME=$(asdf where java zulu-7.56.0.11)
    export CLOUSEAU_M2_HOME=$(asdf where maven 3.8.8)

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
