### Overview

BigCouch is a highly available, fault-tolerant, clustered, mostly api-compliant version of [Apache CouchDB][1].  While it appears to the end-user as one CouchDB instance, it is in fact one or more BigCouch nodes in an elastic cluster, acting in concert to store and retrieve documents, index and serve views, and serve CouchApps.  BigCouch has been developed and is continually maintained by [Cloudant][2] who offer hosted CouchDB as a service.

Clusters behave according to concepts outlined in [Amazon's Dynamo paper][4], namely that each BigCouch node can accept requests, data is placed on partitions based on a consistent hashing algorithm, and quorum protocols are for read/write operations.

### Contents

 * README.md   this file
 * LICENSE     open-source license governing BigCouch

### Getting Started

#### Prerequisites

BigCouch has the same dependencies as CouchDB:

 * Erlang (R13B03 or higher) (R14B01 or higher to build a release)
 * ICU (4.2 is preferable)
 * Spidermonkey (1.9.2 preferable, [https://launchpad.net/~commonjs/+archive/ppa/][6])
 * LibCurl (7.18 or higher)
 * OpenSSL
 * make
 * Python (2.4 or higher)

#### Installing prerequisites on Ubuntu

    sudo apt-get install erlang libicu42 libicu-dev libcurl4-openssl-dev

To install Spidermonkey 1.9.2 from PPA:

    sudo apt-key adv --keyserver keyserver.ubuntu.com --recv-keys 74EE6429
    sudo bash -c 'echo "deb http://ppa.launchpad.net/commonjs/ppa/ubuntu karmic main" >> /etc/apt/sources.list.d/commonjs.list'
    sudo apt-get update
    sudo apt-get install libmozjs-1.9.2 libmozjs-1.9.2-dev
    sudo ln -s /usr/lib/libmozjs-1.9.2.so /usr/lib/libmozjs.so

#### Installing prerequisites on Mac OS X with [Homebrew][7]

    brew install erlang icu4c spidermonkey
    brew ln icu4c

#### Installing prerequisites on RedHat/Centos

    yum install js-devel libicu libicu-devel openssl openssl-devel python python-devel

    Erlang and LibCurl need to be installed from source to meet version requirements

#### Building and installing BigCouch

`$CLOUDANT_SRC` is the directory holding your downloaded source files, while `$PREFIX` is the prefix to which the software is installed (defaults to `/opt/bigcouch`):

    cd $CLOUDANT_SRC
    ./configure -p $PREFIX
    make
    sudo make install

`sudo` is only necessary when installing to a prefix which is not user-writeable.  In any case, the installer tries to chown the database directory and logfile to the user who configured BigCouchbigcouch.

Run into any issues? Check out our wiki, [https://github.com/cloudant/bigcouch/wiki/Troubleshooting][8]

#### Starting BigCouch

    $PREFIX/bin/bigcouch

Now, visit http://localhost:5984/_utils in a browser to verify the BigCouch node is operational.

BigCouch listens on two ports.  Defaults and explanations:

 * 5984 - front door, cluster-aware port, appears as a standalone CouchDB.
 * 5986 - back door, single-node port, used for admin functions

Note: see the `rel/sv/README` file for information on using `runit` to stop/start BigCouch.

#### Joining a new node to the cluster

Each BigCouch node has a local `nodes` database, accessible through the backend interface on port 5986.  Documents in the `nodes` DB name nodes in the cluster.  To add a new node, create a document with that node's name as the ID.  For example

    curl -X PUT http://foo.example.com:5986/nodes/bigcouch@bar.example.com -d {}

Everything else should be automatic, provided the machines can ping each other and the nodes set the same magic cookie.  You are advised to change the magic cookie from the default in `rel/etc/vm.args` when on a public network.

#### Local development cluster

The `make dev` target will build a three-node cluster under the rel/ directory.  Get the nodes running, like above, by doing the following (in separate terminals):

    ./rel/dev1/bin/bigcouch
    ./rel/dev2/bin/bigcouch
    ./rel/dev3/bin/bigcouch

These development nodes listen on ports 15984/15986 (dev1), 25984/25986 (dev2), and 35984/35986 (dev3).  Now, once the nodes are started, join the dev2 node by sending this PUT to dev1's listening backend port:

    curl -X PUT http://127.0.0.1:15986/nodes/dev2@127.0.0.1 -d {}

To verify the two-node cluster has been linked properly, on either node (via proper frontend port), try:

    curl http://127.0.0.1:15984/_membership

You should see something similar to this:

    {"all_nodes":["dev1@127.0.0.1","dev2@127.0.0.1"],"cluster_nodes":["dev1@127.0.0.1","dev2@127.0.0.1"]}

Add node 3 to the cluster by sending a similar PUT to either of the first two nodes.

#### Now What?

If the above steps were successful, you should have a running BigCouch cluster that looks just like a standalone CouchDB.  You may interact with it the same way you would a standalone CouchDB, via the HTTP REST interface.

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

### Troubleshooting

Please see [http://github.com/cloudant/bigcouch/wiki/troubleshooting][8]

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
[7]: http://mxcl.github.com/homebrew/
[8]: http://github.com/cloudant/bigcouch/wiki/troubleshooting
