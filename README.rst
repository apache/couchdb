Apache CouchDB README
=====================

Installation
------------

For a high-level guide to Unix-like systems, inc. Mac OS X and Ubuntu, see:

    INSTALL.Unix

For a high-level guide to Microsoft Windows, see:

    INSTALL.Windows

Follow the proper instructions to get CouchDB installed on your system.

If you're having problems, skip to the next section.

Documentation
-------------

We have documentation:

    http://docs.couchdb.org/

They include a changelog:

    http://docs.couchdb.org/en/latest/changelog.html

For troubleshooting, see:

    http://wiki.apache.org/couchdb/Troubleshooting

If you're getting a cryptic error message, see:

    http://wiki.apache.org/couchdb/Error_messages

For general help, see:

     http://couchdb.apache.org/#mailing-list
     
We also have an IRC channel:

    http://webchat.freenode.net/?channels=couchdb

The mailing lists provide a wealth of support and knowledge for you to tap into.
Feel free to drop by with your questions or discussion. See the official CouchDB
website for more information about our community resources.

Running the Testsuite
---------------------

Run the testsuite for couch.js and jquery.couch.js by browsing here:

    http://127.0.0.1:5984/_utils/spec/run.html

It should work in at least Firefox >= 3.6 with Private Browsing mode enabled.

Read more about JSpec here:

    https://github.com/liblime/jspec

When you change the specs, but your changes have no effect, manually reload
the changed spec file in the browser. When the spec that tests Erlang views
fails, make sure you have enabled Erlang views as described here:

       http://wiki.apache.org/couchdb/EnableErlangViews

Getting started with developing
-------------------------------

Basically you just have to install the needed dependencies which are
documented in the install docs and then run ``./configure && make``.

You don't need to run ``make install`` after compiling, just use
``./dev/run`` to spin up three nodes. There is a haproxy config file in
``./rel``.
Just run: ``cd rel && haproxy -f haproxy.cfg``. You will now have a local
cluster listening on port 5984.

For Fauxton developers fixing the admin-party does not work via the button in
Fauxton. To fix the admin party you have to run ``./dev/run`` with the ``admin``
flag, e.g. ``./dev/run --admin=username:password``. If you want to have an
admin-party, just omit the flag.

Cryptographic Software Notice
-----------------------------

This distribution includes cryptographic software. The country in which you
currently reside may have restrictions on the import, possession, use, and/or
re-export to another country, of encryption software. BEFORE using any
encryption software, please check your country's laws, regulations and policies
concerning the import, possession, or use, and re-export of encryption software,
to see if this is permitted. See <http://www.wassenaar.org/> for more
information.

The U.S. Government Department of Commerce, Bureau of Industry and Security
(BIS), has classified this software as Export Commodity Control Number (ECCN)
5D002.C.1, which includes information security software using or performing
cryptographic functions with asymmetric algorithms. The form and manner of this
Apache Software Foundation distribution makes it eligible for export under the
License Exception ENC Technology Software Unrestricted (TSU) exception (see the
BIS Export Administration Regulations, Section 740.13) for both object code and
source code.

The following provides more details on the included cryptographic software:

CouchDB includes a HTTP client (ibrowse) with SSL functionality.
