## cloudant-dbcore

### Overview

Cloudant-dbcore is a highly available, fault-tolerant, clustered, fully api-compliant version of [Apache CouchDB][1].  While it appears to the end-user as one CouchDB instance, it is in fact one or more nodes in an elastic cluster, acting in concert to store and retrieve documents, index and serve views, and serve CouchApps.  Dbcore has been developed and is continually maintained by [Cloudant][2] who offer hosted CouchDB as a service.

Clusters behave according to concepts outlined in [Amazon's Dynamo paper][4], namely that each node can accept requests, data is placed on partitions based on a consistent hashing algorithm, and quorum protocols are for read/write operations.

### Contents

 * README.md   this file
 * INSTALL     instructions on how to install dbcore
 * LICENSE     open-source license governing dbcore

### Getting Started

For detailed installation instructions, please consult the INSTALL file

#### Prerequisites

 * Erlang R13B03 or higher
 * ICU (4.2 is preferable)
 * Spidermonkey (1.9.2 preferable, [https://launchpad.net/~commonjs/+archive/ppa/][6]) and symlink /usr/lib/libmozjs-1.9.2.so to /usr/lib/libmozjs.so to make things easier
 * OpenSSL
 * make
 * Python 2.4 or higher

#### Building and installing dbcore

`$CLOUDANT_SRC` is the directory holding your downloaded source files, while `$CLOUDANT_PREFIX` is the prefix to which the software is installed (defaults to `/opt`):

    $ cd $CLOUDANT_SRC
    $ ./configure -p $CLOUDANT_PREFIX
    $ make
    $ sudo make install

#### Starting dbcore

    $ $CLOUDANT_PREFIX/dbcore/bin/dbcore

Now, visit http://localhost:5984/_utils in a browser to verify the CouchDB node is operational.

Note: see the rel/sv/README file for information on using runit to stop/start dbcore.

#### Joining a new node to the cluster

First, dbcore listens on two ports.  Defaults and explanations:

 * 5984 - front door, cluster-aware port, appears as a standalone CouchDB.
 * 5986 - back door, single-node port, used for admin functions

Next, once the first node is started, and assuming its hostname is 'node1_host' start dbcore on another node, with hostname 'node2_host'.  Once it's started successfully, on node1_host, join the new node:
    $ curl -X PUT http://localhost:5986/nodes/dbcore@node2_host -d {}

To verify the two-node cluster has been linked properly, on either node, try:
    $ curl http://locahost:5984/_membership

You should see something similar to this:
    {"all_nodes":["dbcore@node1_host","dbcore@node2_host"],"cluster_nodes":["dbcore@node1_host","dbcore@node2_host"]}

#### Now What?

If the above steps were successful, you should have a running dbcore cluster that looks just like a standalone CouchDB.  You may interact with it the same way you would a standalone CouchDB, via the HTTP REST interface.

Because every node can handle requests equally, you may want to put a load balancer in front of the cluster and set up a round-robin strategy for distributing incoming requests across all of your cluster's nodes.

##### Create a database:

    curl -X PUT http://loadbalancer:5984/test_db

Also note that 'q' and 'n' are query string arguments that may be specified.  These are Cloudant-specific options, and their values and defaults are discussed in the Configuration section below. Ex: &q=12 or &n=4.

##### Create a document:

    curl -X PUT http://loadbalancer:5984/test_db/doc_1 -d '{"a":1,"b":2}'

You may also provide 'r' and 'w' on the GET and PUT/POST document operations respectively.  Their values and defaults are discussed in the Configuration section below.  Ex: &r=3 for high consistency reads, or &w=1 for higher throughput writes.

##### Check out Futon, CouchDB's web UI:

 * [http://loadbalancer:5984/_utils][5]

### Configuration

#### Cluster constants

_Q_ - number of partitions per database.  Q is specified in the default.ini file, but may be provided as a URL parameter when creating a database.  Default value is 8, query parameter is &q=12 or similar.

_N_ - replication constant.  N defaults to 3, but can vary by database just as Q can vary.  N copies of each document will be written to the data store, on N different nodes.

_R_ - read quorum constant.  N writes have occurred for each document, as noted above.  When reads are requested, N reads are sent to the N nodes that store the particular document.  The system will return to the requesting client with the document when R successful reads have returned, and agree on versioning.  R defaults to 2.  Lower R values often result in faster reads at the expense of consistency.  Higher R values usually result in slower reads, but more consistent, or agreed-upon data values returning.

_W_ - write quorum constant.  When writing the N copies, the data store will respond to the write client after W successful writes have completed.  The remaining N-W writes are still being attempted in the background, but the client receives a 201 Created status and can resume execution.  W defaults to 2.  Lower W values mean more write throughput, and higher W values mean more data durability.

### Contact

Cloudant folks are usually hanging out in IRC.  Freenode, channel #cloudant.  We may also be reached:

 * [http://cloudant.com][2]
 * [info@cloudant.com][3]

----

[1]: http://couchdb.apache.org
[2]: http://cloudant.com
[3]: mailto:info@cloudant.com
[4]: http://www.allthingsdistributed.com/2007/10/amazons_dynamo.html
[5]: http://loadbalancer:5984/_utils
[6]: https://launchpad.net/~commonjs/+archive/ppa/
