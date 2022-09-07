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

.. _install/troubleshooting:

===============================
Troubleshooting an Installation
===============================

First Install
=============

If your CouchDB doesn't start after you've just installed, check the following
things:

- On UNIX-like systems, this is usually this is a permissions issue. Ensure
  that you've followed the :ref:`install/unix/security`
  ``chown``/``chmod`` commands. This problem is indicated by the presence of
  the keyword ``eacces`` somewhere in the error output from CouchDB itself.
- Some Linux distributions split up Erlang into multiple packages. For your
  distribution, check that you **really** installed all the required Erlang
  modules. This varies from platform to platform, so you'll just have to
  work it out for yourself. For example, on recent versions of Ubuntu/Debian,
  the ``erlang`` package includes all Erlang modules.
- Confirm that Erlang itself starts up with crypto (SSL) support:

.. code-block:: text

    ## what version of erlang are you running? Ensure it is supported
    erl -noshell -eval 'io:put_chars(erlang:system_info(otp_release)).' -s erlang halt
    ## are the erlang crypto (SSL) libraries working?
    erl -noshell -eval 'case application:load(crypto) of ok -> io:put_chars("yay_crypto\n") ; _ -> exit(no_crypto) end.' -s init stop

- Next, identify where your Erlang CouchDB libraries are installed. This will
  typically be the lib/ subdirectory of the release that you have installed.
- Use this to start up Erlang with the CouchDB libraries in its path:

.. code-block:: text

    erl -env ERL_LIBS $ERL_LIBS:/path/to/couchdb/lib -couch_ini -s crypto

- In that Erlang shell, let's check that the key libraries are running. The
  ``%%`` lines are comments, so you can skip them:

.. code-block:: text

    %% test SSL support. If this fails, ensure you have the OTP erlang-crypto library installed
    crypto:md5_init().

    %% test Snappy compression. If this fails, check your CouchDB configure script output or alternatively
    %% if your distro comes with erlang-snappy make sure you're using only the CouchDB supplied version
    snappy:compress("gogogogogogogogogogogogogogo").

    %% test the CouchDB JSON encoder. CouchDB uses different encoders in each release, this one matches
    %% what is used in 2.0.x.
    jiffy:decode(jiffy:encode(<<"[1,2,3,4,5]">>)).

    %% this is how you quit the erlang shell.
    q().

- The output should resemble this, or an error will be thrown:

.. code-block:: text

    Erlang/OTP 17 [erts-6.2] [source] [64-bit] [smp:2:2] [async-threads:10] [kernel-poll:false]

    Eshell V6.2  (abort with ^G)
    1> crypto:md5_init().
    <<1,35,69,103,137,171,205,239,254,220,186,152,118,84,50,
      16,0,0,0,0,0,0,0,0,0,0,0,0,0,...>>
    2> snappy:compress("gogogogogogogogogogogogogogo").
    {ok,<<28,4,103,111,102,2,0>>}
    3> jiffy:decode(jiffy:encode(<<"[1,2,3,4,5]">>)).
    <<"[1,2,3,4,5]">>
    4> q().

- At this point the only remaining dependency is your system's Unicode support
  library, ICU, and the Spidermonkey Javascript VM from Mozilla. Make sure that
  your ``LD_LIBRARY_PATH`` or equivalent for non-Linux systems
  (``DYLD_LIBRARY_PATH`` on macOS) makes these available to CouchDB.
  Linux example running as normal user:

.. code-block:: text

    LD_LIBRARY_PATH=/usr/local/lib:/usr/local/spidermonkey/lib couchdb

  Linux example running as couchdb user:

.. code-block:: text

    echo LD_LIBRARY_PATH=/usr/local/lib:/usr/local/spidermonkey/lib couchdb | sudo -u couchdb sh

- If you receive an error message including the key word ``eaddrinuse``,
  such as this:

.. code-block:: text

    Failure to start Mochiweb: eaddrinuse

  edit your ``etc/default.ini`` or ``etc/local.ini`` file and change the
  ``[chttpd] port = 5984`` line to an available port.

- If you receive an error including the string:

.. code-block:: text

    … OS Process Error … {os_process_error,{exit_status,127}}

then it is likely your SpiderMonkey JavaScript VM installation is not
correct. Please recheck your build dependencies and try again.

- If you receive an error including the string:

.. code-block:: text

    … OS Process Error … {os_process_error,{exit_status,139}}

this is caused by the fact that SELinux blocks access to certain areas of
the file system. You must re-configure SELinux, or you can fully disable
SELinux using the command:

.. code-block:: text

    setenforce 0

- If you are still not able to get CouchDB to start at this point, keep
  reading.

Quick Build
===========

Having problems getting CouchDB to run for the first time? Follow this simple
procedure and report back to the user mailing list or IRC with the output
of each step. Please put the output of these steps into a paste service (such
as https://paste.ee/) rather than including the output of your entire
run in IRC or the mailing list directly.

1. Note down the name and version of your operating system and your processor
   architecture.

2. Note down the installed versions of CouchDB's dependencies.

3. Follow the checkout instructions to get a fresh copy of CouchDB's trunk.

4. Configure from the couchdb directory:

.. code-block:: text

    ./configure

5. Build the release:

.. code-block:: text

    make release

6. Run the couchdb command and log the output:

.. code-block:: text

    cd rel/couchdb
    bin/couchdb

7. Use your system's kernel trace tool and log the output of the above command.

   a) For example, linux systems should use ``strace``:

.. code-block:: text

    strace bin/couchdb 2> strace.out

8. Report back to the mailing list (or IRC) with the output of each step.

Upgrading
=========

Are you upgrading from CouchDB 1.x? Install CouchDB into a fresh directory.
CouchDB's directory layout has changed and may be confused by libraries
present from previous releases.

Runtime Errors
==============

Erlang stack trace contains ``system_limit``, ``open_port``, or ``emfile``
--------------------------------------------------------------------------
Modern Erlang has a default limit of 65536 ports (8196 on Windows), where each
open file handle, tcp connection, and linked-in driver uses one port. OSes have
different soft and hard limits on the number of open handles per process, often
as low as 1024 or 4096 files. You've probably exceeded this.

There are two settings that need changing to increase this value. Consult your
OS documentation for how to increase the limit for your process. Under Linux
and systemd, this setting can be adjusted via ``systemctl edit couchdb`` and
adding the lines:

.. code-block:: ini

    [Service]
    LimitNOFILE=65536

to the file in the editor.

To increase this value higher than 65536, you must also add the Erlang ``+Q``
parameter to your ``etc/vm.args`` file by adding the line:

.. code-block:: text

    +Q 102400

The old ``ERL_MAX_PORTS`` environment variable is ignored by the version of
Erlang supplied with CouchDB.

Lots of memory being used on startup
------------------------------------
Is your CouchDB using a lot of memory (several hundred MB) on startup? This one
seems to especially affect Dreamhost installs. It's really an issue with the
Erlang VM pre-allocating data structures when ulimit is very large or
unlimited. A detailed discussion can be found on the erlang-questions list,
but the short answer is that you should decrease ``ulimit -n`` or lower the
``vm.args`` parameter ``+Q`` to something reasonable like 1024.

function raised exception (Cannot encode 'undefined' value as JSON)
-------------------------------------------------------------------
If you see this in the CouchDB error logs, the JavaScript code you are using
for either a map or reduce function is referencing an object member that is
not defined in at least one document in your database. Consider this
document:

.. code-block:: text

    {
      "_id":"XYZ123",
      "_rev":"1BB2BB",
      "field":"value"
    }

and this map function:

.. code-block:: javascript

    function(doc) {
      emit(doc.name, doc.address);
    }

This will fail on the above document, as it does not contain a ``name`` or
``address`` member. Instead, use guarding to make sure the function only
accesses members when they exist in a document:

.. code-block:: javascript

    function(doc) {
      if(doc.name && doc.address) {
        emit(doc.name, doc.address);
      }
    }

While the above guard will work in most cases, it's worth bearing JavaScript's
understanding of 'false' values in mind. Testing against a property with a
value of 0 (zero), ``''`` (empty String), ``false`` or ``null`` will return
false. If this is undesired, a guard of the form ``if (doc.foo !== undefined)``
should do the trick.

This error can also be caused if a reduce function does not return a value. For
example, this reduce function will cause an error:

.. code-block:: javascript

    function(key, values) {
      sum(values);
    }

The function needs to return a value:

.. code-block:: javascript

    function(key, values) {
      return sum(values);
    }

erlang stack trace contains ``bad_utf8_character_code``
-------------------------------------------------------

CouchDB 1.1.1 and later contain stricter handling of UTF8 encoding. If you are
replicating from older versions to newer versions, then this error may occur
during replication.

A number of work-arounds exist; the simplest is to do an in-place upgrade of
the relevant CouchDB and then compact prior to replicating.

Alternatively, if the number of documents impacted is small, use filtered
replication to exclude only those documents.

FIPS mode
---------

Operating systems can be configured to disallow the use of OpenSSL MD5 hash
functions in order to prevent use of MD5 for cryptographic purposes. CouchDB
makes use of MD5 hashes for verifying the integrity of data (and not for
cryptography) and will not run without the ability to use MD5 hashes.

The message below indicates that the operating system is running in "FIPS mode,"
which, among other restrictions, does not allow the use of OpenSSL's
MD5 functions:

.. code-block:: text

    md5_dgst.c(82): OpenSSL internal error, assertion failed: Digest MD5 forbidden in FIPS mode!
    [os_mon] memory supervisor port (memsup): Erlang has closed
    [os_mon] cpu supervisor port (cpu_sup): Erlang has closed
    Aborted

A workaround for this is provided with the ``--erlang-md5`` compile flag. Use of
the flag results in CouchDB substituting the OpenSSL MD5 function calls with
equivalent calls to Erlang's built-in library ``erlang:md5.`` NOTE: there may be
a performance penalty associated with this workaround.

Because CouchDB does not make use of MD5 hashes for cryptographic purposes, this
workaround does not defeat the purpose of "FIPS mode," provided that the system
owner is aware of and consents to its use.

Debugging startup
-----------------
If you've compiled from scratch and are having problems getting CouchDB to even
start up, you may want to see more detail. Start by enabling logging at the debug
level:

.. code-block:: ini

    [log]
    level = debug

You can then pass the ``-init_debug +W i +v +V -emu_args`` flags in the ``ERL_FLAGS``
environment variable to turn on additional debugging information that CouchDB
developers can use to help you.

Then, reach out to the CouchDB development team using the links provided on the
`CouchDB home page <https://couchdb.apache.org/>`_ for assistance.

macOS Known Issues
====================
undefined error, exit_status 134
--------------------------------

Sometimes the ``Verify Installation`` fails with an ``undefined`` error.
This could be due to a missing dependency with Mac.
In the logs, you will find ``couchdb exit_status,134``.

Installing the missing ``nspr`` via ``brew install nspr`` resolves the issue.
(see: https://github.com/apache/couchdb/issues/979)
