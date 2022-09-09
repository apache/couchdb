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

.. _install/upgrading:

=====================================
Upgrading from prior CouchDB releases
=====================================

Important Notes
===============

* **Always back up your** ``data/`` **and** ``etc/`` **directories prior to
  upgrading CouchDB.**
* We recommend that you overwrite your ``etc/default.ini`` file with the
  version provided by the new release. New defaults sometimes contain
  mandatory changes to enable default functionality. Always places your
  customizations in ``etc/local.ini`` or any ``etc/local.d/*.ini`` file.

Upgrading from CouchDB 2.x
==========================

If you are coming from a prior release of CouchDB 2.x, upgrading is simple.

Standalone (single) node upgrades
---------------------------------

If you are running a standalone (single) CouchDB node:

#. Plan for downtime.
#. Backup everything.
#. Check for new recommended settings in the shipped ``etc/local.ini`` file,
   and merge any changes desired into your own local settings file(s).
#. Stop CouchDB.
#. Upgrade CouchDB in place.
#. Be sure to :ref:`create an admin user<config/admins>` if you do not have
   one. CouchDB 3.0+ **require** an admin user to start (the admin party has
   ended).
#. Start CouchDB.
#. Relax! You're done.

Cluster upgrades
----------------

CouchDB 2.x and 3.x are explicitly designed to allow "mixed clusters" during
the upgrade process. This allows you to perform a rolling restart across
a cluster, upgrading one node at a time, for a *zero downtime upgrade*.
The process is also entirely scriptable within your configuration
management tool of choice.

We're proud of this feature, and you should be, too!

If you are running a CouchDB cluster:

#. Backup everything.
#. Check for new recommended settings in the shipped ``etc/local.ini`` file,
   and merge any changes desired into your own local settings file(s),
   staging these changes to occur as you upgrade the node.
#. Stop CouchDB on a single node.
#. Upgrade that CouchDB install in place.
#. Start CouchDB.
#. Double-check that the node has re-joined the cluster through the
   `/_membership <api/server/membership>` endpoint. If your load balancer has
   health check functionality driven by the `/_up <api/server/up>` endpoint,
   check whether it thinks the node is healthy as well.
#. Repeat the last 4 steps on the remaining nodes in the cluster.
#. Relax! You're done.

Upgrading from CouchDB 1.x
==========================

To upgrade from CouchDB 1.x, first upgrade to a version of CouchDB 2.x.  You
will need to convert all databases to CouchDB 2.x format first; see the Upgrade
Notes there for instructions. Then, upgrade to CouchDB 3.x.
