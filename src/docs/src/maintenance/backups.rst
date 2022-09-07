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

.. _backups:

==================
Backing up CouchDB
==================

CouchDB has three different types of files it can create during runtime:

* Database files (including secondary indexes)
* Configuration files (``*.ini``)
* Log files (if configured to log to disk)

Below are strategies for ensuring consistent backups of all of these files.

Database Backups
================

The simplest and easiest approach for CouchDB backup is to use :ref:`CouchDB
replication <replication>` to another CouchDB installation.  You can choose
between :ref:`normal (one-shot) or continuous replications <Normal vs Continuous
Replications>` depending on your need.

However, you can also copy the actual ``.couch`` files from the CouchDB data
directory (by default, ``data/``) at any time, without problem. CouchDB's
append-only storage format for both databases and secondary indexes ensures that
this will work without issue.

To ensure reliability of backups, it is recommended that you *back up secondary
indexes* (stored under ``data/.shards``) *prior to backing up the main database
files* (stored under ``data/shards`` as well as the system-level databases at the
parent ``data/`` directory). This is because CouchDB will automatically handle
views/secondary indexes that are slightly out of date by updating them on the
next read access, but views or secondary indexes that are *newer* than their
associated databases will trigger a *full rebuild of the index*. This can be a
very costly and time-consuming operation, and can impact your ability to
recover quickly in a disaster situation.

On supported operating systems/storage environments, you can also make use of
`storage snapshots <https://en.wikipedia.org/wiki/Snapshot_(computer_storage)>`_.
These have the advantage of being near-instantaneous when working with block
storage systems such as `ZFS <https://en.wikipedia.org/wiki/ZFS>`_ or `LVM
<https://en.wikipedia.org/wiki/Logical_Volume_Manager_(Linux)>`_ or `Amazon EBS
<https://en.wikipedia.org/wiki/Amazon_Elastic_Block_Store>`_. When using
snapshots at the block-storage level, be sure to quiesce the file system with an
OS-level utility such as Linux's `fsfreeze
<https://linux.die.net/man/8/fsfreeze>`_ if necessary. If unsure, consult your
operating system's or cloud provider's documentation for more detail.

Configuration Backups
=====================

CouchDB's :ref:`configuration system <config/intro>` stores data in ``.ini`` files
under the configuration directory (by default, ``etc/``). If changes are made
to the configuration at runtime, the very last file in the configuration chain
will be updated with the changes.

Simple back up the entire ``etc/`` directory to ensure a consistent configuration
after restoring from backup.

If no changes to the configuration are made at runtime through the HTTP API,
and all configuration files are managed by a configuration management system
(such as `Ansible <https://en.wikipedia.org/wiki/Ansible_(software)>`_ or
`Chef <https://en.wikipedia.org/wiki/Chef_(software)>`_), there is no need to
backup the configuration directory.

Log Backups
===========

If :ref:`configured to log to a file <config/log>`, you may want to back up the
log files written by CouchDB. Any backup solution for these files works.

Under UNIX-like systems, if using log rotation software, a copy-then-truncate
approach is necessary. This will truncate the original log file to zero size in
place after creating a copy. CouchDB does not recognize any signal to be told to
close its log file and create a new one. Because of this, and because of
differences in how file handles function, there is no straightforward log
rotation solution under Microsoft Windows other than periodic restarts of the
CouchDB process.
