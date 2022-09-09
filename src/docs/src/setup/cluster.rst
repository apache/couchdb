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

.. _setup/cluster:

==============
Cluster Set Up
==============

This section describes everything you need to know to prepare, install, and
set up your first CouchDB 2.x/3.x cluster.

Ports and Firewalls
===================

CouchDB uses the following ports:

+-------------+----------+--------------------------+----------------------+
| Port Number | Protocol | Recommended binding      | Usage                |
+=============+==========+==========================+======================+
| 5984        | tcp      | As desired, by           | Standard clustered   |
|             |          | default ``localhost``    | port for all HTTP    |
|             |          |                          | API requests         |
+-------------+----------+--------------------------+----------------------+
| 4369        | tcp      | ``localhost`` for single | Erlang port mapper   |
|             |          | node installs. Private   | daemon (epmd)        |
|             |          | interface if clustered   |                      |
+-------------+----------+--------------------------+----------------------+
| Random      | tcp      | Private interface        | Communication with   |
| above 1024  |          |                          | other CouchDB nodes  |
| (see below) |          |                          | in the cluster       |
+-------------+----------+--------------------------+----------------------+

CouchDB in clustered mode uses the port ``5984``, just as in a standalone
configuration. Port ``5986``, previously used in CouchDB 2.x, has been removed
in CouchDB 3.x. All endpoints previously accessible at that port are now
available under the ``/_node/{node-name}/...`` hierarchy via the primary ``5984``
port.

CouchDB uses Erlang-native clustering functionality to achieve a clustered
installation.  Erlang uses TCP port ``4369`` (EPMD) to find other nodes, so all
servers must be able to speak to each other on this port. In an Erlang cluster,
all nodes are connected to all other nodes, in a mesh network configuration.

Every Erlang application running on that machine (such as CouchDB) then uses
automatically assigned ports for communication with other nodes. Yes, this
means random ports. This will obviously not work with a firewall, but it is
possible to force an Erlang application to use a specific port range.

This documentation will use the range TCP ``9100-9200``, but this range is
unnecessarily broad. If you only have a single Erlang application running on a
machine, the range can be limited to a single port: ``9100-9100``, since the
ports erlang assigns are for *inbound connections* only. Three CouchDB nodes
running on a single machine, as in a development cluster scenario, would need
three ports in this range.

.. warning::
    If you expose the distribution port to the Internet or any other untrusted
    network, then the only thing protecting you is the Erlang
    `cookie`_.

.. _cookie: http://erlang.org/doc/reference_manual/distributed.html

Configure and Test the Communication with Erlang
================================================

Make CouchDB use correct IP|FQDN and the open ports
----------------------------------------------------

In file ``etc/vm.args`` change the line ``-name couchdb@127.0.0.1`` to
``-name couchdb@<reachable-ip-address|fully-qualified-domain-name>`` which defines
the name of the node. Each node must have an identifier that allows remote
systems to talk to it. The node name is of the form
``<name>@<reachable-ip-address|fully-qualified-domain-name>``.

The name portion can be couchdb on all nodes, unless you are running more than
1 CouchDB node on the same server with the same IP address or domain name. In
that case, we recommend names of ``couchdb1``, ``couchdb2``, etc.

The second portion of the node name must be an identifier by which other nodes
can access this node -- either the node's fully qualified domain name (FQDN) or
the node's IP address. The FQDN is preferred so that you can renumber the node's
IP address without disruption to the cluster. (This is common in cloud-hosted
environments.)

.. warning::

    Tricks with ``/etc/hosts`` and ``libresolv`` don't work with Erlang.
    Either properly set up DNS and use fully-qualified domain names, or
    use IP addresses. DNS and FQDNs are preferred.

    Changing the name later is somewhat cumbersome (i.e. moving shards), which
    is why you will want to set it once and not have to change it.

Open ``etc/vm.args``, on all nodes, and add ``-kernel inet_dist_listen_min 9100``
and ``-kernel inet_dist_listen_max 9200`` like below:

.. code-block:: erlang

    -name ...
    -setcookie ...
    ...
    -kernel inet_dist_listen_min 9100
    -kernel inet_dist_listen_max 9200

Again, a small range is fine, down to a single port (set both to ``9100``) if you
only ever run a single CouchDB node on each machine.

Confirming connectivity between nodes
-------------------------------------

For this test, you need 2 servers with working hostnames. Let us call them
server1.test.com and server2.test.com. They reside at ``192.168.0.1`` and
``192.168.0.2``, respectively.

On server1.test.com:

.. code-block:: bash

    erl -name bus@192.168.0.1 -setcookie 'brumbrum' -kernel inet_dist_listen_min 9100 -kernel inet_dist_listen_max 9200

Then on server2.test.com:

.. code-block:: bash

    erl -name car@192.168.0.2 -setcookie 'brumbrum' -kernel inet_dist_listen_min 9100 -kernel inet_dist_listen_max 9200

An explanation to the commands:
    * ``erl`` the Erlang shell.
    * ``-name bus@192.168.0.1`` the name of the Erlang node and its IP address or FQDN.
    * ``-setcookie 'brumbrum'`` the "password" used when nodes connect to each
      other.
    * ``-kernel inet_dist_listen_min 9100`` the lowest port in the range.
    * ``-kernel inet_dist_listen_max 9200`` the highest port in the range.

This gives us 2 Erlang shells. shell1 on server1, shell2 on server2.
Time to connect them. Enter the following, being sure to end the line with a
period (``.``):

In shell1:

.. code-block:: erlang

    net_kernel:connect_node('car@192.168.0.2').

This will connect to the node called ``car`` on the server called
``192.168.0.2``.

If that returns true, then you have an Erlang cluster, and the firewalls are
open. This means that 2 CouchDB nodes on these two servers will be able to
communicate with each other successfully. If you get false or nothing at all,
then you have a problem with the firewall, DNS, or your settings. Try again.

If you're concerned about firewall issues, or having trouble connecting all
nodes of your cluster later on, repeat the above test between all pairs of
servers to confirm connectivity and system configuration is correct.

.. _cluster/setup/prepare:

Preparing CouchDB nodes to be joined into a cluster
===================================================

Before you can add nodes to form a cluster, you must have them listening on an
IP address accessible from the other nodes in the cluster. You should also ensure
that a few critical settings are identical across all nodes before joining them.

The settings we recommend you set now, before joining the nodes into a cluster,
are:

1. ``etc/vm.args`` settings as described in the
   :ref:`previous two sections<setup/cluster>`
2. At least one :ref:`server administrator<config/admins>`
   user (and password)
3. Bind the node's clustered interface (port ``5984``) to a reachable IP address
4. A consistent :config:option:`UUID <couchdb/uuid>`. The UUID is used in identifying
   the cluster when replicating. If this value is not consistent across all nodes
   in the cluster, replications may be forced to rewind the changes feed to zero,
   leading to excessive memory, CPU and network use.
5. A consistent :config:option:`httpd secret <chttpd_auth/secret>`. The secret
   is used in calculating and evaluating cookie and proxy authentication, and should
   be set consistently to avoid unnecessary repeated session cookie requests.

As of CouchDB 3.0, steps 4 and 5 above are automatically performed for you when
using the setup API endpoints described below.

If you use a configuration management tool, such as Chef, Ansible, Puppet, etc.,
then you can place these settings in a ``.ini`` file and distribute them to all
nodes ahead of time. Be sure to pre-encrypt the password (cutting and pasting
from a test instance is easiest) if you use this route to avoid CouchDB rewriting
the file.

If you do not use configuration management, or are just experimenting with
CouchDB for the first time, use these commands *once per server* to perform
steps 2-4 above. Be sure to change the ``password`` to something secure, and
again, use the same password on all nodes. You may have to run these commands
locally on each node; if so, replace ``<server-IP|FQDN>`` below with ``127.0.0.1``.

.. code-block:: bash

    # First, get two UUIDs to use later on. Be sure to use the SAME UUIDs on all nodes.
    curl http://<server-IP|FQDN>:5984/_uuids?count=2

    # CouchDB will respond with something like:
    #   {"uuids":["60c9e8234dfba3e2fdab04bf92001142","60c9e8234dfba3e2fdab04bf92001cc2"]}
    # Copy the provided UUIDs into your clipboard or a text editor for later use.
    # Use the first UUID as the cluster UUID.
    # Use the second UUID as the cluster shared http secret.

    # Create the admin user and password:
    curl -X PUT http://<server-IP|FQDN>:5984/_node/_local/_config/admins/admin -d '"password"'

    # Now, bind the clustered interface to all IP addresses available on this machine
    curl -X PUT http://<server-IP|FQDN>:5984/_node/_local/_config/chttpd/bind_address -d '"0.0.0.0"'

    # If not using the setup wizard / API endpoint, the following 2 steps are required:
    # Set the UUID of the node to the first UUID you previously obtained:
    curl -X PUT http://<server-IP|FQDN>:5984/_node/_local/_config/couchdb/uuid -d '"FIRST-UUID-GOES-HERE"'

    # Finally, set the shared http secret for cookie creation to the second UUID:
    curl -X PUT http://<server-IP|FQDN>:5984/_node/_local/_config/chttpd_auth/secret -d '"SECOND-UUID-GOES-HERE"'

.. _cluster/setup/wizard:

The Cluster Setup Wizard
========================

CouchDB 2.x/3.x comes with a convenient Cluster Setup Wizard as part of the Fauxton
web administration interface. For first-time cluster setup, and for
experimentation, this is your best option.

It is **strongly recommended** that the minimum number of nodes in a cluster is
3. For more explanation, see the :ref:`Cluster Theory <cluster/theory>` section
of this documentation.

After installation and initial start-up of all nodes in your cluster, ensuring
all nodes are reachable, and the pre-configuration steps listed above, visit
Fauxton at ``http://<server1>:5984/_utils#setup``. You will be asked to set up
CouchDB as a single-node instance or set up a cluster.

When you click "Setup Cluster" you are asked for admin credentials again, and
then to add nodes by IP address. To get more nodes, go through the same install
procedure for each node, using the same machien to perform the setup process.
Be sure to specify the total number of nodes you expect to add to the cluster
before adding nodes.

Now enter each node's IP address or FQDN in the setup wizard, ensuring you also
enter the previously set server admin username and password.

Once you have added all nodes, click "Setup" and Fauxton will finish the
cluster configuration for you.

To check that all nodes have been joined correctly, visit
``http://<server-IP|FQDN>:5984/_membership`` on each node. The returned list
should show all of the nodes in your cluster:

.. code-block:: javascript

    {
      "all_nodes": [
        "couchdb@server1.test.com",
        "couchdb@server2.test.com",
        "couchdb@server3.test.com"
      ],
      "cluster_nodes": [
        "couchdb@server1.test.com",
        "couchdb@server2.test.com",
        "couchdb@server3.test.com"
      ]
    }

The ``all_nodes`` section is the list of *expected* nodes; the ``cluster_nodes``
section is the list of *actually connected* nodes. Be sure the two lists match.

Now your cluster is ready and available! You can send requests to any one of
the nodes, and all three will respond as if you are working with a single
CouchDB cluster.

For a proper production setup, you'd now set up an HTTP reverse proxy in front
of the cluster, for load balancing and SSL termination. We recommend
`HAProxy`_, but others can be used. Sample configurations are available in the
:ref:`best-practices` section.

.. _cluster/setup/api:

The Cluster Setup API
=====================

If you would prefer to manually configure your CouchDB cluster, CouchDB exposes
the ``_cluster_setup`` endpoint for that purpose. After installation and
initial setup/config, we can set up the cluster. On each node we need to run
the following command to set up the node:

.. code-block:: bash

     curl -X POST -H "Content-Type: application/json" http://admin:password@127.0.0.1:5984/_cluster_setup -d '{"action": "enable_cluster", "bind_address":"0.0.0.0", "username": "admin", "password":"password", "node_count":"3"}'

After that we can join all the nodes together. Choose one node as the "setup
coordination node" to run all these commands on.  This "setup coordination
node" only manages the setup and requires all other nodes to be able to see it
and vice versa. *It has no special purpose beyond the setup process; CouchDB
does not have the concept of a "master" node in a cluster.*

Setup will not work with unavailable nodes. All nodes must be online and properly
preconfigured before the cluster setup process can begin.

To join a node to the cluster, run these commands for each node you want to add:

.. code-block:: bash

    curl -X POST -H "Content-Type: application/json" http://admin:password@<setup-coordination-node>:5984/_cluster_setup -d '{"action": "enable_cluster", "bind_address":"0.0.0.0", "username": "admin", "password":"password", "port": 5984, "node_count": "3", "remote_node": "<remote-node-ip>", "remote_current_user": "<remote-node-username>", "remote_current_password": "<remote-node-password>" }'
    curl -X POST -H "Content-Type: application/json" http://admin:password@<setup-coordination-node>:5984/_cluster_setup -d '{"action": "add_node", "host":"<remote-node-ip>", "port": <remote-node-port>, "username": "admin", "password":"password"}'

This will join the two nodes together. Keep running the above commands for each
node you want to add to the cluster. Once this is done run the following
command to complete the cluster setup and add the system databases:

.. code-block:: bash

    curl -X POST -H "Content-Type: application/json" http://admin:password@<setup-coordination-node>:5984/_cluster_setup -d '{"action": "finish_cluster"}'

Verify install:

.. code-block:: bash

    curl http://admin:password@<setup-coordination-node>:5984/_cluster_setup

Response:

.. code-block:: bash

    {"state":"cluster_finished"}

Verify all cluster nodes are connected:

.. code-block:: bash

    curl http://admin:password@<setup-coordination-node>:5984/_membership

Response:

.. code-block:: bash

    {
        "all_nodes": [
            "couchdb@couch1.test.com",
            "couchdb@couch2.test.com",
            "couchdb@couch3.test.com",
        ],
        "cluster_nodes": [
            "couchdb@couch1.test.com",
            "couchdb@couch2.test.com",
            "couchdb@couch3.test.com",
        ]
    }

Ensure the ``all_nodes`` and ``cluster_nodes`` lists match.

You CouchDB cluster is now set up.

.. _HAProxy: http://haproxy.org/
.. _example configuration for HAProxy: https://github.com/apache/couchdb/blob/main/rel/haproxy.cfg
