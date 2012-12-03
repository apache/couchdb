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

=======================================
CouchDB Release |version| Feature Guide
=======================================

Upgrading to CouchDB |version|
==============================

You can upgrade your existing CouchDB 1.0.x installation to CouchDB |version|
without any specific steps or migration. When you run CouchDB |version| the
existing data and index files will be opened and used as normal.

The first time you run a compaction routine on your database within
CouchDB |version|, the data structure and indexes will be updated to the new
version of the CouchDB database format that can only be read by CouchDB
|version| and later. This step is not reversible. Once the data files have
been updated and migrated to the new version the data files will no
longer work with a CouchDB 1.0.x release.

.. warning::
   If you want to retain support for opening the data files in
   CouchDB 1.0.x you must back up your data files before performing the
   upgrade and compaction process.

New features in CouchDB |version|
=================================

.. toctree::
..   :maxdepth: 2
..   
..   replicator
..   ssl
..   range
..   proxy
..   commonjs
..   other
