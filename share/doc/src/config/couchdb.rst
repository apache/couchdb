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

.. highlight:: ini

.. _config/couchdb:

``[couchdb]`` :: Base CouchDB Options
=====================================

These options are under ``[couchdb]`` section.


.. _config/couchdb/attachment_stream_buffer_size:

``attachment_stream_buffer_size`` :: Attachment streaming buffer
----------------------------------------------------------------

Higher values may give better read performance due to less read operations
and/or more OS page cache hits, but they can also increase overall response
time for writes when there are many attachment write requests in parallel::

  [couchdb]
  attachment_stream_buffer_size = 4096


.. _config/couchdb/database_dir:

``database_dir`` :: Databases location directory
------------------------------------------------

Specifies location of CouchDB databases files (``*.couch`` named). This location
should be writable and readable for user that runs CouchDB service (``couchdb``
by default)::

  [couchdb]
  database_dir = /var/lib/couchdb


.. _config/couchdb/delayed_commits:

``delayed_commits`` :: Delayed commits
--------------------------------------

When this config value as ``false`` the CouchDB provides guaranty of fsync call
before return ``HTTP 201 Created`` response on each document saving. Setting
this config value as ``true`` may raise some overall performance with cost of
losing durability - it's strongly not recommended to do such in production::

  [couchdb]
  delayed_commits = false

.. warning::

   Delayed commits are a feature of CouchDB that allows it to achieve better
   write performance for some workloads while sacrificing a small amount of
   durability. The setting causes CouchDB to wait up to a full second before
   committing new data after an update. If the server crashes before the header
   is written then any writes since the last commit are lost.


.. _config/couchdb/file_compression:

``file_compression`` :: Compression method for documents
-----------------------------------------------------------

.. versionchanged:: 1.2 Added `Google Snappy`_ compression algorithm

Method used to compress everything that is appended to database and view index
files, except for attachments (see the :ref:`[attachments] <config/attachments>`
section). Available methods are:

* ``none``: no compression
* ``snappy``: use Google Snappy, a very fast compressor/decompressor.
* ``deflate_N``: use zlib's deflate, ``N`` is the compression level which ranges
  from 1 (fastest, lowest compression ratio) to 9 (slowest, highest compression
  ratio)

::

  [couchdb]
  file_compression = snappy

.. _Google Snappy: http://code.google.com/p/snappy/


.. _config/couchdb/max_dbs_open:

``max_dbs_open`` :: Limit of simultaneously opened databases
------------------------------------------------------------

This option places an upper bound on the number of databases that can be open at
one time. CouchDB reference counts database accesses internally and will close
idle databases when it must. Sometimes it is necessary to keep more than the
default open at once, such as in deployments where many databases will be
continuously replicating::

  [couchdb]
  max_dbs_open = 100


.. _config/couchdb/max_document_size:

``max_document_size`` :: Maximum document size
----------------------------------------------

.. versionchanged:: 1.3 This option now actually works.

Defines limit of size in bytes that document may has in JSON form. Doesn't
applies for attachments since they are been transferred as stream of chunks.
Do not set this value too small since you want be able to modify config options,
database security and other options until you restore this value by edit config
file::

  [couchdb]
  max_document_size = 4294967296 ; 4 GB


.. _config/couchdb/os_process_timeout:

``os_process_timeout`` :: External processes time limit
-------------------------------------------------------

Number of microseconds that external processes such as `query server` and
`externals` may process CouchDB commands before return any result. Keeping
this value smaller you'll be ensured that your services works fast, but you may
tweak it depending on your needs::

  [couchdb]
  os_process_timeout = 5000 ; 5 sec


.. _config/couchdb/uri_file:

``uri_file`` :: Discovery CouchDB help file
-------------------------------------------

This file contains full `URI`_ that runs CouchDB. It's used to help discover
CouchDB served port if it was set to ``0`` (e.g. automatically assigned any
free one). This file should be writable and readable for user that runs CouchDB
service (``couchdb`` by default)::

  [couchdb]
  uri_file = /var/run/couchdb/couchdb.uri

.. _URI: http://en.wikipedia.org/wiki/URI


.. _config/couchdb/util_driver_dir:

``util_driver_dir`` :: CouchDB binary utility drivers
-----------------------------------------------------

Specified location of binary drivers (`icu`, `ejson`, etc.). This location and
his content should be readable for user that runs CouchDB service::

  [couchdb]
  util_driver_dir = /usr/lib/couchdb/erlang/lib/couch-1.3.0/priv/lib


.. _config/couchdb/view_index_dir:

``view_index_dir`` :: View indexes location directory
-----------------------------------------------------

Specifies location of CouchDB view index files. This location should be writable
and readable for user that runs CouchDB service (``couchdb`` by default)::

  [couchdb]
  view_index_dir = /var/lib/couchdb

