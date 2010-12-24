## mem3

Mem3 is the node membership application for clustered [CouchDB][1].  It is used in [BigCouch][2] and tracks two very important things for the cluster:

 1. member nodes
 2. node/partition mappings for each database

Both the nodes and partitions are tracked in node-local couch databases.  Partitions are heavily used, so an ETS cache is also maintained for low-latency lookups.  The nodes and partitions are synchronized via continuous CouchDB replication, which serves as 'gossip' in Dynamo parlance.  The partitions ETS cache is kept in sync based on membership and database event listeners.

A very important point to make here is that BigCouch does not necessarily divide up each database into equal partitions across the nodes of a cluster.  For instance, in a 20-node cluster, you may have the need to create a small database with very few documents.  For efficiency reasons, you may create your database with Q=4 and keep the default of N=3.  This means you only have 12 partitions total, so 8 nodes will hold none of the data for this database.  Given this feature, we even partition use out across the cluster by altering the 'start' node for the database's partitions.

Splitting and merging partitions is an immature feature of the system, and will require attention in the near-term.  We believe we can implement both functions and perform them while the database remains online.

### Getting Started

Mem3 requires R13B03 or higher and can be built with [rebar][6], which comes bundled in the repository.  Rebar needs to be able to find the `couch_db.hrl` header file; one way to accomplish this is to set ERL_LIBS to point to the apps
subdirectory of a bigcouch checkout, e.g.

    ERL_LIBS="/usr/local/src/bigcouch/apps" ./rebar compile

### License
[Apache 2.0][3]

### Contact
 * [http://cloudant.com][4]
 * [info@cloudant.com][5]

[1]: http://couchdb.apache.org
[2]: http://github.com/cloudant/bigcouch
[3]: http://www.apache.org/licenses/LICENSE-2.0.html
[4]: http://cloudant.com
[5]: mailto:info@cloudant.com
[6]: http://github.com/basho/rebar
