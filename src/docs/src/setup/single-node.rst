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

.. _setup/single-node:

=================
Single Node Setup
=================

Many users simply need a single-node CouchDB 2.x installation. Operationally,
it is roughly equivalent to the CouchDB 1.x series. Note that a single-node
setup obviously doesn't take any advantage of the new scaling and
fault-tolerance features in CouchDB 2.x.

After installation and initial startup, visit Fauxton at
``http://127.0.0.1:5984/_utils#setup``. You will be asked to set up
CouchDB as a single-node instance or set up a cluster. When you click
“Single-Node-Setup”, you will get asked for an admin username and
password. Choose them well and remember them.

You can also bind CouchDB to a public address, so it is accessible within your
LAN or the public, if you are doing this on a public VM. Or, you can keep the
installation private by binding only to 127.0.0.1 (localhost). Binding to
0.0.0.0 will bind to all addresses. The wizard then configures your admin
username and password and creates the three system databases ``_users``,
``_replicator`` and ``_global_changes`` for you.

Another option is to set the configuration parameter ``[couchdb] single_node=true``
in your ``local.ini`` file. When doing this, CouchDB will create the system
database for you on restart.

Alternatively, if you don't want to use the Setup Wizard or set that value, and
run 3.x as a single node with a server administrator already configured via
:ref:`config file<config/admins>`, make sure to create the three system
databases manually on startup:

.. code-block:: sh

    curl -X PUT http://127.0.0.1:5984/_users

    curl -X PUT http://127.0.0.1:5984/_replicator

    curl -X PUT http://127.0.0.1:5984/_global_changes

Note that the last of these is not necessary if you do not expect to be
using the global changes feed. Feel free to delete this database if you
have created it, it has grown in size, and you do not need the function
(and do not wish to waste system resources on compacting it regularly.)
