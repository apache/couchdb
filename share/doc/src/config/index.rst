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

.. _config:

===================
Configuring CouchDB
===================

CouchDB reads files from the following locations, in the following
order.

1. ``PREFIX/default.ini``

2. ``PREFIX/default.d/*``

3. ``PREFIX/local.ini``

4. ``PREFIX/local.d/*``

Settings in successive documents override the settings in earlier
entries. For example, setting the
:ref:`bind_address <config/httpd/bind_address>` parameter in ``local.ini``
would override any setting in ``default.ini``.

.. warning::
   The ``default.ini`` file may be overwritten during an upgrade or
   re-installation, so localised changes should be made to the
   ``local.ini`` file or files within the ``local.d`` directory.

Content:

.. CouchDB configuration sections goes first.
   Please keep them sorted to simplify eye searching.
   After them - custom configuration tips and tricks.

.. toctree::
   :maxdepth: 2
   :glob:

   admins
   attachments
   compaction
   compaction-daemon
   cors
   couch-httpd-auth
   couchdb
   daemons
   httpd
   native-query-servers
   os-daemons
   query-servers
   replicator
   ssl
   stats
   update-notification
   uuids
   vendor
   vhosts

   proxying

