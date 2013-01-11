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

Apache CouchDB CHANGES
======================

Version 1.3.0
-------------

Database core
^^^^^^^^^^^^^

* :issue:`1512`: Validate bind address before assignment. :commit:`09ead8a0`
* Restore ``max_document_size`` protection. :commit:`bf1eb135`

Documentation
^^^^^^^^^^^^^

* :issue:`1523`: Import CouchBase documentation and convert them into
  `Sphinx docs <http://sphinx.pocoo.org/>`_

Futon
^^^^^

* :issue:`1470`: Futon raises popup on attempt to navigate to missed/deleted
  document. :commit:`5da40eef`
* :issue:`1383`: Futon view editor won't allow you to save original view after
  saving a revision. :commit:`ce48342`
* :issue:`627`: Support all timezones. :commit:`b1a049bb`
* :issue:`509`: Added view request duration to Futon. :commit:`2d2c7d1e`
* :issue:`1473`, :issue:`1472`: Disable buttons for actions that the user
  doesn't have permissions to. :commit:`7156254d`

HTTP Interface
^^^^^^^^^^^^^^^^^

* :issue:`1537`: Include user name in show/list `ETags`. :commit:`ac320479`
* :issue:`1511`: CouchDB checks `roles` field for `_users` database documents
  with more care. :commit:`41205000`
* :issue:`1502`: Allow users to delete own _users doc. :commit:`f0d6f19bc8`
* :issue:`1501`: :ref:`Changes feed <changes>` now can take special parameter
  ``since=now`` to emit changes since current point of time. :commit:`3bbb2612`
* :issue:`1442`: No longer rewrites the `X-CouchDB-Requested-Path` during
  recursive calls to the rewriter. :commit:`56744f2f`
* :issue:`1441`: Limit recursion depth in the URL rewriter.
  Defaults to a maximum of 100 invocations but is configurable.
  :commit:`d076976c`
* :issue:`1381`: Add jquery.couch support for Windows 8 Metro apps.
  :commit:`dfc5d37c`
* :issue:`1337`: Use MD5 for attachment ETag header value. :commit:`6d912c9f`
* :issue:`1321`: Variables in rewrite rules breaks OAuth authentication.
  :commit:`c307ba95`
* :issue:`1285`: Allow configuration of vendor and modules version in CouchDB
  welcome message. :commit:`3c24a94d`
* :issue:`1277`: Better query parameter support and code clarity:
  :commit:`7e3c69ba`

  * Responses to documents created/modified via form data `POST` to /db/doc or
    copied with `COPY` should now include `Location` header.
  * Form data POST to /db/doc now includes an `ETag` response header.
  * ``?batch=ok`` is now supported for `COPY` and `POST` /db/doc updates.
  * ``?new_edits=false`` is now supported for more operations.

* :issue:`1210`: Files starting with underscore can be attached and updated now.
  :commit:`05858792`
* :issue:`1097`: Allow `OPTIONS` request to shows and lists functions.
  :commit:`9f53704a`
* :issue:`1026`: Database names are encoded with respect of special characters
  in the rewriter now. :commit:`272d6415`
* :issue:`986`: Added Server-Sent Events protocol to db changes API.
  See http://www.w3.org/TR/eventsource/ for details. :commit:`093d2aa6`
* :issue:`887`: Fix ``bytes`` and ``offset`` parameters semantic for `_log`
  resource (`explanation <https://git-wip-us.apache.org/repos/asf?p=couchdb.git;a=blobdiff;f=src/couchdb/couch_log.erl;h=1b05f4db2;hp=0befe7aab;hb=ad700014;hpb=7809f3ca>`_)
  :commit:`ad700014`
* :issue:`764`, :issue:`514`, :issue:`430`: Fix sending HTTP headers from
  ``_list`` function, :commit:`2a74f88375`
* Send a 202 response for `_restart`. :commit:`b213e16f`
* Make password hashing synchronous when using the /_config/admins API.
  :commit:`08071a80`
* Fix `_session` for IE7.
* Return ``X-Couch-Id`` header if doc is created, :commit:`98515bf0b9`
* Allow any 2xx code to indicate success, :commit:`0d50103cfd`
* Restore 400 error for empty PUT, :commit:`2057b895`
* Add support to serve single file with CouchDB, :commit:`2774531ff2`
* Support auth cookies with ``:`` characters, :commit:`d9566c831d`

Log System
^^^^^^^^^^

* :issue:`1380`: Minor fixes for logrotate support.
* Improve file I/O error logging and handling, :commit:`4b6475da`
* Module Level Logging, :commit:`b58f069167`
* Log 5xx responses at error level, :commit:`e896b0b7`
* Log problems opening database at ERROR level except for auto-created
  system dbs, :commit:`41667642f7`

Replicator
^^^^^^^^^^

* :issue:`1557`: Upgrade some code to use BIFs bring good improvements for
  replication.
* :issue:`1363`: Fix rarely occurred, but still race condition in changes feed
  if a quick burst of changes happens while replication is starting the
  replication can go stale. :commit:`573a7bb9`
* :issue:`1323`: Replicator now acts as standalone application.
  :commit:`f913ca6e`
* :issue:`1259`: Stabilize replication id, :commit:`c6252d6d7f`
* :issue:`1248`: `HTTP 500` error now doesn't occurs when replicating with
  ``?doc_ids=null``. :commit:`bea76dbf`

Security
^^^^^^^^

* :issue:`1060`: Passwords are now hashed using the PBKDF2 algorithm with a
  configurable work factor. :commit:`7d418134`

Source Repository
^^^^^^^^^^^^^^^^^

* The source repository was migrated from `SVN`_ to `Git`_.

.. _SVN: https://svn.apache.org/repos/asf/couchdb
.. _Git: https://git-wip-us.apache.org/repos/asf/couchdb.git

Storage System
^^^^^^^^^^^^^^

* Fixed unnecessary conflict when deleting and creating a
  document in the same batch.

Test Suite
^^^^^^^^^^

* :issue:`1563`: Ensures urlPrefix is set in all ajax requests.
  :commit:`07a6af222`
* :issue:`1389`: Improved tracebacks printed by the JS CLI tests.
* :issue:`1339`: Use shell trap to catch dying beam processes during test runs.
  :commit:`2921c78`
* :issue:`1338`: Start CouchDB with ``port=0``. While CouchDB might be already
  running on the default port 5984, port number 0 let the TCP stack figure out a
  free port to run. :commit:`127cbe3`
* :issue:`1321`: Moved the JS test suite to the CLI.
* Improved the reliability of a number of tests.
* Fix race condition for test running on faster hardware.

URL Rewriter & Vhosts
^^^^^^^^^^^^^^^^^^^^^

* :issue:`1026`: Database name is encoded during rewriting
  (allowing embedded /'s, etc). :commit:`272d6415`

UUID Algorithms
^^^^^^^^^^^^^^^

* :issue:`1373`: Added the utc_id algorithm :commit:`5ab712a2`

Query and View Server
^^^^^^^^^^^^^^^^^^^^^

* :issue:`1491`: Clenaup view tables. :commit:`c37204b7`
* :issue:`1483`: Update handlers requires valid doc ids. :commit:`72ea7e38`
* :issue:`1445`: CouchDB tries no more to delete view file if it couldn't open
  it, even if the error is `emfile`.
* :issue:`1444`: Fix missed_named_view error that occurs on existed design
  documents and views. :commit:`b59ac98b`
* :issue:`1372`: `_stats` builtin reduce function no longer produces error for
  empty view result.
* :issue:`410`: More graceful error handling for JavaScript validate_doc_update
  functions.
* :issue:`111`: Improve the errors reported by the javascript view server
  to provide a more friendly error report when something goes wrong.
  :commit:`0c619ed`
* Deprecate E4X support, :commit:`cdfdda2314`

Windows
^^^^^^^

* :issue:`1482`: Use correct linker flang to build `snappy_nif.dll` on Windows.
  :commit:`a6eaf9f1`
* Allows building cleanly on Windows without cURL, :commit:`fb670f5712`


Version 1.2.0
-------------

Authentication
^^^^^^^^^^^^^^

* Fix use of OAuth with VHosts and URL rewriting.
* OAuth secrets can now be stored in the users system database
  as an alternative to key value pairs in the .ini configuration.
  By default this is disabled (secrets are stored in the .ini)
  but can be enabled via the .ini configuration key `use_users_db`
  in the `couch_httpd_oauth` section.
* Documents in the _users database are no longer publicly
  readable.
* Confidential information in the _replication database is no
  longer publicly readable.
* Password hashes are now calculated by CouchDB. Clients are no
  longer required to do this manually.
* Cookies used for authentication can be made persistent by enabling
  the .ini configuration key `allow_persistent_cookies` in the
  `couch_httpd_auth` section.

Build System
^^^^^^^^^^^^

* cURL is no longer required to build CouchDB as it is only
  used by the command line JS test runner. If cURL is available
  when building CouchJS you can enable the HTTP bindings by
  passing -H on the command line.
* Temporarily made `make check` pass with R15B. A more thorough
  fix is in the works (:issue:`1424`).
* Fixed --with-js-include and --with-js-lib options.
* Added --with-js-lib-name option.

Futon
^^^^^

* The `Status` screen (active tasks) now displays two new task status
  fields: `Started on` and `Updated on`.
* Futon remembers view code every time it is saved, allowing to save an
  edit that amounts to a revert.

HTTP Interface
^^^^^^^^^^^^^^

* Added a native JSON parser.
* The _active_tasks API now offers more granular fields. Each
  task type is now able to expose different properties.
* Added built-in changes feed filter `_view`.
* Fixes to the `_changes` feed heartbeat option which caused
  heartbeats to be missed when used with a filter. This caused
  timeouts of continuous pull replications with a filter.
* Properly restart the SSL socket on configuration changes.

Replicator
^^^^^^^^^^

* A new replicator implementation. It offers more performance and
  configuration options.
* Passing non-string values to query_params is now a 400 bad
  request. This is to reduce the surprise that all parameters
  are converted to strings internally.
* Added optional field `since_seq` to replication objects/documents.
  It allows to bootstrap a replication from a specific source sequence
  number.
* Simpler replication cancellation. In addition to the current method,
  replications can now be canceled by specifying the replication ID
  instead of the original replication object/document.

Storage System
^^^^^^^^^^^^^^

* Added optional database and view index file compression (using Google's
  snappy or zlib's deflate). This feature is enabled by default, but it
  can be disabled by adapting local.ini accordingly. The on-disk format
  is upgraded on compaction and new DB/view creation to support this.
* Several performance improvements, most notably regarding database writes
  and view indexing.
* Computation of the size of the latest MVCC snapshot data and all its
  supporting metadata, both for database and view index files. This
  information is exposed as the `data_size` attribute in the database and
  view group information URIs.
* The size of the buffers used for database and view compaction is now
  configurable.
* Added support for automatic database and view compaction. This feature
  is disabled by default, but it can be enabled via the .ini configuration.
* Performance improvements for the built-in changes feed filters `_doc_ids`
  and `_design'.

View Server
^^^^^^^^^^^

* Add CoffeeScript (http://coffeescript.org/) as a first class view server
  language.
* Fixed old index file descriptor leaks after a view cleanup.
* The requested_path property keeps the pre-rewrite path even when no VHost
  configuration is matched.
* Fixed incorrect reduce query results when using pagination parameters.
* Made icu_driver work with Erlang R15B and later.

OAuth
^^^^^

* Updated bundled erlang_oauth library to the latest version.


Version 1.1.1
-------------

* Support SpiderMonkey 1.8.5
* Add configurable maximum to the number of bytes returned by _log.
* Allow CommonJS modules to be an empty string.
* Bump minimum Erlang version to R13B02.
* Do not run deleted validate_doc_update functions.
* ETags for views include current sequence if include_docs=true.
* Fix bug where duplicates can appear in _changes feed.
* Fix bug where update handlers break after conflict resolution.
* Fix bug with _replicator where include "filter" could crash couch.
* Fix crashes when compacting large views.
* Fix file descriptor leak in _log
* Fix missing revisions in _changes?style=all_docs.
* Improve handling of compaction at max_dbs_open limit.
* JSONP responses now send "text/javascript" for Content-Type.
* Link to ICU 4.2 on Windows.
* Permit forward slashes in path to update functions.
* Reap couchjs processes that hit reduce_overflow error.
* Status code can be specified in update handlers.
* Support provides() in show functions.
* _view_cleanup when ddoc has no views now removes all index files.
* max_replication_retry_count now supports "infinity".
* Fix replication crash when source database has a document with empty ID.
* Fix deadlock when assigning couchjs processes to serve requests.
* Fixes to the document multipart PUT API.
* Fixes regarding file descriptor leaks for databases with views.


Version 1.1.0
-------------

.. note:: All CHANGES for 1.0.2 and 1.0.3 also apply to 1.1.0.

Externals
^^^^^^^^^

* Added OS Process module to manage daemons outside of CouchDB.
* Added HTTP Proxy handler for more scalable externals.

Futon
^^^^^

* Added a "change password"-feature to Futon.

HTTP Interface
^^^^^^^^^^^^^^

* Native SSL support.
* Added support for HTTP range requests for attachments.
* Added built-in filters for `_changes`: `_doc_ids` and `_design`.
* Added configuration option for TCP_NODELAY aka "Nagle".
* Allow POSTing arguments to `_changes`.
* Allow `keys` parameter for GET requests to views.
* Allow wildcards in vhosts definitions.
* More granular ETag support for views.
* More flexible URL rewriter.
* Added support for recognizing "Q values" and media parameters in
  HTTP Accept headers.
* Validate doc ids that come from a PUT to a URL.

Replicator
^^^^^^^^^^

* Added `_replicator` database to manage replications.
* Fixed issues when an endpoint is a remote database accessible via SSL.
* Added support for continuous by-doc-IDs replication.
* Fix issue where revision info was omitted when replicating attachments.
* Integrity of attachment replication is now verified by MD5.

Storage System
^^^^^^^^^^^^^^

* Multiple micro-optimizations when reading data.

URL Rewriter & Vhosts
^^^^^^^^^^^^^^^^^^^^^

* Fix for variable substituion

View Server
^^^^^^^^^^^

* Added CommonJS support to map functions.
* Added `stale=update_after` query option that triggers a view update after
  returning a `stale=ok` response.
* Warn about empty result caused by `startkey` and `endkey` limiting.
* Built-in reduce function `_sum` now accepts lists of integers as input.
* Added view query aliases start_key, end_key, start_key_doc_id and
  end_key_doc_id.


Version 1.0.3
-------------

General
^^^^^^^

* Fixed compatibility issues with Erlang R14B02.

Etap Test Suite
^^^^^^^^^^^^^^^

* Etap tests no longer require use of port 5984. They now use a randomly
  selected port so they won't clash with a running CouchDB.

Futon
^^^^^

* Made compatible with jQuery 1.5.x.

HTTP Interface
^^^^^^^^^^^^^^

* Fix bug that allows invalid UTF-8 after valid escapes.
* The query parameter `include_docs` now honors the parameter `conflicts`.
  This applies to queries against map views, _all_docs and _changes.
* Added support for inclusive_end with reduce views.

Replicator
^^^^^^^^^^

* Enabled replication over IPv6.
* Fixed for crashes in continuous and filtered changes feeds.
* Fixed error when restarting replications in OTP R14B02.
* Upgrade ibrowse to version 2.2.0.
* Fixed bug when using a filter and a limit of 1.

Security
^^^^^^^^

* Fixed OAuth signature computation in OTP R14B02.
* Handle passwords with : in them.

Storage System
^^^^^^^^^^^^^^

* More performant queries against _changes and _all_docs when using the
  `include_docs` parameter.

Windows
^^^^^^^

* Windows builds now require ICU >= 4.4.0 and Erlang >= R14B03. See
  :issue:`1152`, and :issue:`963` + OTP-9139 for more information.


Version 1.0.2
-------------

Futon
^^^^^

* Make test suite work with Safari and Chrome.
* Fixed animated progress spinner.
* Fix raw view document link due to overzealous URI encoding.
* Spell javascript correctly in loadScript(uri).

HTTP Interface
^^^^^^^^^^^^^^

* Allow reduce=false parameter in map-only views.
* Fix parsing of Accept headers.
* Fix for multipart GET APIs when an attachment was created during a
  local-local replication. See :issue:`1022` for details.

Log System
^^^^^^^^^^

* Reduce lengthy stack traces.
* Allow logging of native <xml> types.

Replicator
^^^^^^^^^^

* Updated ibrowse library to 2.1.2 fixing numerous replication issues.
* Make sure that the replicator respects HTTP settings defined in the config.
* Fix error when the ibrowse connection closes unexpectedly.
* Fix authenticated replication (with HTTP basic auth) of design documents
  with attachments.
* Various fixes to make replication more resilient for edge-cases.

Storage System
^^^^^^^^^^^^^^

* Fix leaking file handles after compacting databases and views.
* Fix databases forgetting their validation function after compaction.
* Fix occasional timeout errors after successfully compacting large databases.
* Fix ocassional error when writing to a database that has just been compacted.
* Fix occasional timeout errors on systems with slow or heavily loaded IO.
* Fix for OOME when compactions include documents with many conflicts.
* Fix for missing attachment compression when MIME types included parameters.
* Preserve purge metadata during compaction to avoid spurious view rebuilds.
* Fix spurious conflicts introduced when uploading an attachment after
  a doc has been in a conflict. See :issue:`902` for details.
* Fix for frequently edited documents in multi-master deployments being
  duplicated in _changes and _all_docs.  See :issue:`968` for details on how
  to repair.
* Significantly higher read and write throughput against database and
  view index files.

View Server
^^^^^^^^^^^

* Don't trigger view updates when requesting `_design/doc/_info`.
* Fix for circular references in CommonJS requires.
* Made isArray() function available to functions executed in the query server.
* Documents are now sealed before being passed to map functions.
* Force view compaction failure when duplicated document data exists. When
  this error is seen in the logs users should rebuild their views from
  scratch to fix the issue. See :issue:`999` for details.


Version 1.0.1
-------------

Authentication
^^^^^^^^^^^^^^

* Enable basic-auth popup when required to access the server, to prevent
   people from getting locked out.

Build and System Integration
^^^^^^^^^^^^^^^^^^^^^^^^^^^^

* Included additional source files for distribution.

Futon
^^^^^

* User interface element for querying stale (cached) views.

HTTP Interface
^^^^^^^^^^^^^^

* Expose `committed_update_seq` for monitoring purposes.
* Show fields saved along with _deleted=true. Allows for auditing of deletes.
* More robust Accept-header detection.

Replicator
^^^^^^^^^^

* Added support for replication via an HTTP/HTTPS proxy.
* Fix pull replication of attachments from 0.11 to 1.0.x.
* Make the _changes feed work with non-integer seqnums.

Storage System
^^^^^^^^^^^^^^

* Fix data corruption bug :issue:`844`. Please see
  http://couchdb.apache.org/notice/1.0.1.html for details.


Version 1.0.0
-------------

Security
^^^^^^^^

* Added authentication caching, to avoid repeated opening and closing of the
  users database for each request requiring authentication.

Storage System
^^^^^^^^^^^^^^

* Small optimization for reordering result lists.
* More efficient header commits.
* Use O_APPEND to save lseeks.
* Faster implementation of pread_iolist(). Further improves performance on
  concurrent reads.

View Server
^^^^^^^^^^^

* Faster default view collation.
* Added option to include update_seq in view responses.


Version 0.11.2
--------------

Authentication
^^^^^^^^^^^^^^

* User documents can now be deleted by admins or the user.

Futon
^^^^^

* Add some Futon files that were missing from the Makefile.

HTTP Interface
^^^^^^^^^^^^^^

* Better error messages on invalid URL requests.

Replicator
^^^^^^^^^^

* Fix bug when pushing design docs by non-admins, which was hanging the
   replicator for no good reason.
* Fix bug when pulling design documents from a source that requires
   basic-auth.

Security
^^^^^^^^

* Avoid potential DOS attack by guarding all creation of atoms.


Version 0.11.1
--------------

Build and System Integration
^^^^^^^^^^^^^^^^^^^^^^^^^^^^

* Output of `couchdb --help` has been improved.
* Fixed compatibility with the Erlang R14 series.
* Fixed warnings on Linux builds.
* Fixed build error when aclocal needs to be called during the build.
* Require ICU 4.3.1.
* Fixed compatibility with Solaris.

Configuration System
^^^^^^^^^^^^^^^^^^^^

* Fixed timeout with large .ini files.

Futon
^^^^^

* Use "expando links" for over-long document values in Futon.
* Added continuous replication option.
* Added option to replicating test results anonymously to a community
  CouchDB instance.
* Allow creation and deletion of config entries.
* Fixed display issues with doc ids that have escaped characters.
* Fixed various UI issues.

HTTP Interface
^^^^^^^^^^^^^^

* Mask passwords in active tasks and logging.
* Update mochijson2 to allow output of BigNums not in float form.
* Added support for X-HTTP-METHOD-OVERRIDE.
* Better error message for database names.
* Disable jsonp by default.
* Accept gzip encoded standalone attachments.
* Made max_concurrent_connections configurable.
* Made changes API more robust.
* Send newly generated document rev to callers of an update function.

JavaScript Clients
^^^^^^^^^^^^^^^^^^

* Added tests for couch.js and jquery.couch.js
* Added changes handler to jquery.couch.js.
* Added cache busting to jquery.couch.js if the user agent is msie.
* Added support for multi-document-fetch (via _all_docs) to jquery.couch.js.
* Added attachment versioning to jquery.couch.js.
* Added option to control ensure_full_commit to jquery.couch.js.
* Added list functionality to jquery.couch.js.
* Fixed issues where bulkSave() wasn't sending a POST body.

Log System
^^^^^^^^^^

* Log HEAD requests as HEAD, not GET.
* Keep massive JSON blobs out of the error log.
* Fixed a timeout issue.

Replication System
^^^^^^^^^^^^^^^^^^

* Refactored various internal APIs related to attachment streaming.
* Fixed hanging replication.
* Fixed keepalive issue.

Security
^^^^^^^^

* Added authentication redirect URL to log in clients.
* Fixed query parameter encoding issue in oauth.js.
* Made authentication timeout configurable.
* Temporary views are now admin-only resources.

Storage System
^^^^^^^^^^^^^^

* Don't require a revpos for attachment stubs.
* Added checking to ensure when a revpos is sent with an attachment stub,
  it's correct.
* Make file deletions async to avoid pauses during compaction and db
  deletion.
* Fixed for wrong offset when writing headers and converting them to blocks,
  only triggered when header is larger than 4k.
* Preserve _revs_limit and instance_start_time after compaction.

Test Suite
^^^^^^^^^^

* Made the test suite overall more reliable.

View Server
^^^^^^^^^^^

* Provide a UUID to update functions (and all other functions) that they can
  use to create new docs.
* Upgrade CommonJS modules support to 1.1.1.
* Fixed erlang filter funs and normalize filter fun API.
* Fixed hang in view shutdown.

URL Rewriter & Vhosts
^^^^^^^^^^^^^^^^^^^^^

* Allow more complex keys in rewriter.
* Allow global rewrites so system defaults are available in vhosts.
* Allow isolation of databases with vhosts.
* Fix issue with passing variables to query parameters.


Version 0.11.0
--------------

Build and System Integration
^^^^^^^^^^^^^^^^^^^^^^^^^^^^

* Updated and improved source documentation.
* Fixed distribution preparation for building on Mac OS X.
* Added support for building a Windows installer as part of 'make dist'.
* Bug fix for building couch.app's module list.
* ETap tests are now run during make distcheck. This included a number of
  updates to the build system to properly support VPATH builds.
* Gavin McDonald setup a build-bot instance. More info can be found at
  http://ci.apache.org/buildbot.html

Futon
^^^^^

* Added a button for view compaction.
* JSON strings are now displayed as-is in the document view, without the
  escaping of new-lines and quotes. That dramatically improves readability of
  multi-line strings.
* Same goes for editing of JSON string values. When a change to a field value is
  submitted, and the value is not valid JSON it is assumed to be a string. This
  improves editing of multi-line strings a lot.
* Hitting tab in textareas no longer moves focus to the next form field, but
  simply inserts a tab character at the current caret position.
* Fixed some font declarations.

HTTP Interface
^^^^^^^^^^^^^^

* Provide Content-MD5 header support for attachments.
* Added URL Rewriter handler.
* Added virtual host handling.

Replication
^^^^^^^^^^^

* Added option to implicitly create replication target databases.
* Avoid leaking file descriptors on automatic replication restarts.
* Added option to replicate a list of documents by id.
* Allow continuous replication to be cancelled.

Runtime Statistics
^^^^^^^^^^^^^^^^^^

* Statistics are now calculated for a moving window instead of non-overlapping
  timeframes.
* Fixed a problem with statistics timers and system sleep.
* Moved statistic names to a term file in the priv directory.

Security
^^^^^^^^

* Fixed CVE-2010-0009: Apache CouchDB Timing Attack Vulnerability.
* Added default cookie-authentication and users database.
* Added Futon user interface for user signup and login.
* Added per-database reader access control lists.
* Added per-database security object for configuration data in validation
  functions.
* Added proxy authentication handler

Storage System
^^^^^^^^^^^^^^

* Adds batching of multiple updating requests, to improve throughput with many
  writers. Removed the now redundant couch_batch_save module.
* Adds configurable compression of attachments.

View Server
^^^^^^^^^^^

* Added optional 'raw' binary collation for faster view builds where Unicode
  collation is not important.
* Improved view index build time by reducing ICU collation callouts.
* Improved view information objects.
* Bug fix for partial updates during view builds.
* Move query server to a design-doc based protocol.
* Use json2.js for JSON serialization for compatiblity with native JSON.
* Major refactoring of couchjs to lay the groundwork for disabling cURL
  support. The new HTTP interaction acts like a synchronous XHR. Example usage
  of the new system is in the JavaScript CLI test runner.




Version 0.10.1
--------------

Build and System Integration
^^^^^^^^^^^^^^^^^^^^^^^^^^^^

* Test suite now works with the distcheck target.

Replicator
^^^^^^^^^^

* Stability enhancements regarding redirects, timeouts, OAuth.

Query Server
^^^^^^^^^^^^

* Avoid process leaks
* Allow list and view to span languages

Stats
^^^^^

* Eliminate new process flood on system wake


Version 0.10.0
--------------

Build and System Integration
^^^^^^^^^^^^^^^^^^^^^^^^^^^^

* Changed `couchdb` script configuration options.
* Added default.d and local.d configuration directories to load sequence.

HTTP Interface
^^^^^^^^^^^^^^

* Added optional cookie-based authentication handler.
* Added optional two-legged OAuth authentication handler.

Storage Format
^^^^^^^^^^^^^^

* Add move headers with checksums to the end of database files for extra robust
  storage and faster storage.

View Server
^^^^^^^^^^^

* Added native Erlang views for high-performance applications.


Version 0.9.2
-------------

Build and System Integration
^^^^^^^^^^^^^^^^^^^^^^^^^^^^

* Remove branch callbacks to allow building couchjs against newer versions of
  Spidermonkey.

Replication
^^^^^^^^^^^

* Fix replication with 0.10 servers initiated by an 0.9 server (:issue:`559`).


Version 0.9.1
-------------

Build and System Integration
^^^^^^^^^^^^^^^^^^^^^^^^^^^^

* PID file directory is now created by the SysV/BSD daemon scripts.
* Fixed the environment variables shown by the configure script.
* Fixed the build instructions shown by the configure script.
* Updated ownership and permission advice in `README` for better security.

Configuration and stats system
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

* Corrected missing configuration file error message.
* Fixed incorrect recording of request time.

Database Core
^^^^^^^^^^^^^

* Document validation for underscore prefixed variables.
* Made attachment storage less sparse.
* Fixed problems when a database with delayed commits pending is considered
  idle, and subject to losing changes when shutdown. (:issue:`334`)

External Handlers
^^^^^^^^^^^^^^^^^

* Fix POST requests.

Futon
^^^^^

* Redirect when loading a deleted view URI from the cookie.

HTTP Interface
^^^^^^^^^^^^^^

* Attachment requests respect the "rev" query-string parameter.

JavaScript View Server
^^^^^^^^^^^^^^^^^^^^^^

* Useful JavaScript Error messages.

Replication
^^^^^^^^^^^

* Added support for Unicode characters transmitted as UTF-16 surrogate pairs.
* URL-encode attachment names when necessary.
* Pull specific revisions of an attachment, instead of just the latest one.
* Work around a rare chunk-merging problem in ibrowse.
* Work with documents containing Unicode characters outside the Basic
  Multilingual Plane.


Version 0.9.0
-------------

Build and System Integration
^^^^^^^^^^^^^^^^^^^^^^^^^^^^

* The `couchdb` script now supports system chainable configuration files.
* The Mac OS X daemon script now redirects STDOUT and STDERR like SysV/BSD.
* The build and system integration have been improved for portability.
* Added COUCHDB_OPTIONS to etc/default/couchdb file.
* Remove COUCHDB_INI_FILE and COUCHDB_PID_FILE from etc/default/couchdb file.
* Updated `configure.ac` to manually link `libm` for portability.
* Updated `configure.ac` to extended default library paths.
* Removed inets configuration files.
* Added command line test runner.
* Created dev target for make.

Configuration and stats system
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

* Separate default and local configuration files.
* HTTP interface for configuration changes.
* Statistics framework with HTTP query API.

Database Core
^^^^^^^^^^^^^

* Faster B-tree implementation.
* Changed internal JSON term format.
* Improvements to Erlang VM interactions under heavy load.
* User context and administrator role.
* Update validations with design document validation functions.
* Document purge functionality.
* Ref-counting for database file handles.

Design Document Resource Paths
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

* Added httpd_design_handlers config section.
* Moved _view to httpd_design_handlers.
* Added ability to render documents as non-JSON content-types with _show and
  _list functions, which are also httpd_design_handlers.

Futon Utility Client
^^^^^^^^^^^^^^^^^^^^

* Added pagination to the database listing page.
* Implemented attachment uploading from the document page.
* Added page that shows the current configuration, and allows modification of
  option values.
* Added a JSON "source view" for document display.
* JSON data in view rows is now syntax highlighted.
* Removed the use of an iframe for better integration with browser history and
  bookmarking.
* Full database listing in the sidebar has been replaced by a short list of
  recent databases.
* The view editor now allows selection of the view language if there is more
  than one configured.
* Added links to go to the raw view or document URI.
* Added status page to display currently running tasks in CouchDB.
* JavaScript test suite split into multiple files.
* Pagination for reduce views.

HTTP Interface
^^^^^^^^^^^^^^

* Added client side UUIDs for idempotent document creation
* HTTP COPY for documents
* Streaming of chunked attachment PUTs to disk
* Remove negative count feature
* Add include_docs option for view queries
* Add multi-key view post for views
* Query parameter validation
* Use stale=ok to request potentially cached view index
* External query handler module for full-text or other indexers.
* Etags for attachments, views, shows and lists
* Show and list functions for rendering documents and views as developer
  controlled content-types.
* Attachment names may use slashes to allow uploading of nested directories
  (useful for static web hosting).
* Option for a view to run over design documents.
* Added newline to JSON responses. Closes bike-shed.

Replication
^^^^^^^^^^^

* Using ibrowse.
* Checkpoint replications so failures are less expensive.
* Automatically retry of failed replications.
* Stream attachments in pull-replication.


Version 0.8.1-incubating
------------------------

Build and System Integration
^^^^^^^^^^^^^^^^^^^^^^^^^^^^

* The `couchdb` script no longer uses `awk` for configuration checks as this
  was causing portability problems.
* Updated `sudo` example in `README` to use the `-i` option, this fixes
  problems when invoking from a directory the `couchdb` user cannot access.

Database Core
^^^^^^^^^^^^^

* Fix for replication problems where the write queues can get backed up if the
  writes aren't happening fast enough to keep up with the reads. For a large
  replication, this can exhaust memory and crash, or slow down the machine
  dramatically. The fix keeps only one document in the write queue at a time.
* Fix for databases sometimes incorrectly reporting that they contain 0
  documents after compaction.
* CouchDB now uses ibrowse instead of inets for its internal HTTP client
  implementation. This means better replication stability.

Futon
^^^^^

* The view selector dropdown should now work in Opera and Internet Explorer
  even when it includes optgroups for design documents. (:issue:`81`)

JavaScript View Server
^^^^^^^^^^^^^^^^^^^^^^

* Sealing of documents has been disabled due to an incompatibility with
  SpiderMonkey 1.9.
* Improve error handling for undefined values emitted by map functions.
  (:issue:`83`)

HTTP Interface
^^^^^^^^^^^^^^

* Fix for chunked responses where chunks were always being split into multiple
  TCP packets, which caused problems with the test suite under Safari, and in
  some other cases.
* Fix for an invalid JSON response body being returned for some kinds of
  views. (:issue:`84`)
* Fix for connections not getting closed after rejecting a chunked request.
  (:issue:`55`)
* CouchDB can now be bound to IPv6 addresses.
* The HTTP `Server` header now contains the versions of CouchDB and Erlang.


Version 0.8.0-incubating
------------------------

Build and System Integration
^^^^^^^^^^^^^^^^^^^^^^^^^^^^

* CouchDB can automatically respawn following a server crash.
* Database server no longer refuses to start with a stale PID file.
* System logrotate configuration provided.
* Improved handling of ICU shared libraries.
* The `couchdb` script now automatically enables SMP support in Erlang.
* The `couchdb` and `couchjs` scripts have been improved for portability.
* The build and system integration have been improved for portability.

Database Core
^^^^^^^^^^^^^

* The view engine has been completely decoupled from the storage engine. Index
  data is now stored in separate files, and the format of the main database
  file has changed.
* Databases can now be compacted to reclaim space used for deleted documents
  and old document revisions.
* Support for incremental map/reduce views has been added.
* To support map/reduce, the structure of design documents has changed. View
  values are now JSON objects containing at least a `map` member, and
  optionally a `reduce` member.
* View servers are now identified by name (for example `javascript`) instead of
  by media type.
* Automatically generated document IDs are now based on proper UUID generation
  using the crypto module.
* The field `content-type` in the JSON representation of attachments has been
  renamed to `content_type` (underscore).

Futon
^^^^^

* When adding a field to a document, Futon now just adds a field with an
  autogenerated name instead of prompting for the name with a dialog. The name
  is automatically put into edit mode so that it can be changed immediately.
* Fields are now sorted alphabetically by name when a document is displayed.
* Futon can be used to create and update permanent views.
* The maximum number of rows to display per page on the database page can now
  be adjusted.
* Futon now uses the XMLHTTPRequest API asynchronously to communicate with the
  CouchDB HTTP server, so that most operations no longer block the browser.
* View results sorting can now be switched between ascending and descending by
  clicking on the `Key` column header.
* Fixed a bug where documents that contained a `@` character could not be
  viewed. (:issue:`12`)
* The database page now provides a `Compact` button to trigger database
  compaction. (:issue:`38`)
* Fixed portential double encoding of document IDs and other URI segments in
  many instances. (:issue:`39`)
* Improved display of attachments.
* The JavaScript Shell has been removed due to unresolved licensing issues.

JavaScript View Server
^^^^^^^^^^^^^^^^^^^^^^

* SpiderMonkey is no longer included with CouchDB, but rather treated as a
  normal external dependency. A simple C program (`_couchjs`) is provided that
  links against an existing SpiderMonkey installation and uses the interpreter
  embedding API.
* View functions using the default JavaScript view server can now do logging
  using the global `log(message)` function. Log messages are directed into the
  CouchDB log at `INFO` level. (:issue:`59`)
* The global `map(key, value)` function made available to view code has been
  renamed to `emit(key, value)`.
* Fixed handling of exceptions raised by view functions.

HTTP Interface
^^^^^^^^^^^^^^

* CouchDB now uses MochiWeb instead of inets for the HTTP server
  implementation. Among other things, this means that the extra configuration
  files needed for inets (such as `couch_httpd.conf`) are no longer used.
* The HTTP interface now completely supports the `HEAD` method. (:issue:`3`)
* Improved compliance of `Etag` handling with the HTTP specification.
  (:issue:`13`)
* Etags are no longer included in responses to document `GET` requests that
  include query string parameters causing the JSON response to change without
  the revision or the URI having changed.
* The bulk document update API has changed slightly on both the request and the
  response side. In addition, bulk updates are now atomic.
* CouchDB now uses `TCP_NODELAY` to fix performance problems with persistent
  connections on some platforms due to nagling.
* Including a `?descending=false` query string parameter in requests to views
  no longer raises an error.
* Requests to unknown top-level reserved URLs (anything with a leading
  underscore) now return a `unknown_private_path` error instead of the
  confusing `illegal_database_name`.
* The Temporary view handling now expects a JSON request body, where the JSON
  is an object with at least a `map` member, and optional `reduce` and
  `language` members.
* Temporary views no longer determine the view server based on the Content-Type
  header of the `POST` request, but rather by looking for a `language` member
  in the JSON body of the request.
* The status code of responses to `DELETE` requests is now 200 to reflect that
  that the deletion is performed synchronously.
