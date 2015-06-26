Apache CouchDB DEVELOPERS
=========================

Before you start here, read `INSTALL.Unix` (or `INSTALL.Windows`) and
follow the setup instructions including the installation of all the
listed dependencies for your system.

Only follow these instructions if you are building from a source checkout.

If you're unsure what this means, ignore this document.

Dependencies
------------

You will need the following installed:

 * Rebar (>=2.5.0)        (https://github.com/rebar/rebar)

You may also need:

 * Sphinx                 (http://sphinx.pocoo.org/)
 * LaTex                  (http://www.latex-project.org/)
 * GNU Texinfo            (http://www.gnu.org/software/texinfo/)
 * GNU help2man           (http://www.gnu.org/software/help2man/)
 * GnuPG                  (http://www.gnupg.org/)
 * md5sum                 (http://www.microbrew.org/tools/md5sha1sum/)
 * sha1sum                (http://www.microbrew.org/tools/md5sha1sum/)

The first of these optional dependencies are required for building the
documentation. The last three are needed to build releases.

You will need these optional dependencies installed if:

 * You are working on the documentation, or
 * You are preparing a distribution archive

However, you do not need them if:

 * You are building from a distribution archive, or
 * You don't care about building the documentation

If you intend to build Fauxton, you will also need to install its
dependencies. After running ./configure to download all of the
dependent repositories, you can read about required dependencies in
`src/fauxton/readme.md`. Typically, installing npm and node.js are
sufficient to enable a Fauxton build.

Here is a list of *optional* dependencies for various operating systems.
Installation will be easiest, when you install them all.

Debian-based (inc. Ubuntu) Systems
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    sudo apt-get install help2man python-sphinx \
        texlive-latex-base texlive-latex-recommended \
        texlive-latex-extra texlive-fonts-recommended texinfo gnupg

Gentoo-based Systems
~~~~~~~~~~~~~~~~~~~~

    sudo emerge texinfo gnupg coreutils pkgconfig help2man
    sudo USE=latex emerge sphinx

RedHat-based (Fedora, Centos, RHEL) Systems
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    sudo yum install help2man python-sphinx python-docutils \
        python-pygments texlive-latex texlive-latex-fonts texinfo gnupg

Mac OS X
~~~~~~~~

Install Homebrew, if you do not have it already:

    https://github.com/mxcl/homebrew

Unless you want to install the optional dependencies, skip to the next section.

Install what else we can with Homebrew:

    brew install help2man gnupg md5sha1sum

If you don't already have pip installed, install it:

    sudo easy_install pip

Now, install the required Python packages:

    sudo pip install sphinx
    sudo pip install docutils
    sudo pip install pygments

Download MaxTeX from here:

    http://www.tug.org/mactex/

Follow the instructions to get a working LaTeX install on your system.

FreeBSD
-------


    pkg install help2man texinfo gnupg py27-sphinx texlive-full tex-formats

Windows
~~~~~~~

Follow the instructions in INSTALL.Windows and build all components from
source, using the same Visual C++ compiler and runtime.

Configuring
-----------

Configure the source by running:

    ./configure

If you intend to run the test suites:

    ./configure -c

If you want to build it into different destination than `/usr/local`.

    ./configure --prefix=/<your directory path>

Testing
-------

Check the test suite by running:

    make check

Generate a coverage report by running:

    make cover

Please report any problems to the developer's mailing list.

Testing a cluster
-----------------

We use Docker (https://docker.io) to safely run a local three node
cluster all inside a single docker container.

Assuming you have Docker installed and running:

    make docker-image

This will create a docker image (tagged 'couchdb/dev-cluster') capable
of running a joined three node cluster.

To start it up:

    make docker-start

A three node cluster should now be running (you can now use `docker ps`
to find the exposed ports of the nodes).

To stop it:

    make docker-stop

Releasing
---------

The release procedure is documented here:

    https://wiki.apache.org/couchdb/Release_Procedure

Unix-like Systems
~~~~~~~~~~~~~~~~~

Prepare the release artefacts by running:

    make distcheck

You can prepare signed release artefacts by running:

    make distsign

The release artefacts can be found in the root source directory.

Microsoft Windows
~~~~~~~~~~~~~~~~~

Prepare the release artefacts by running:

    make dist

The release artefacts can be found in the `etc/windows` directory.

Until the build system has been improved, you must make sure that you run this
command from a clean source checkout. If you do not, your test database and log
files will be bundled up in the release artefact.
