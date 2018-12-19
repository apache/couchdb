## mem3

Mem3 is the node membership application for clustered [CouchDB][1]. It is used
in CouchDB since version 2.0 and tracks two very important things for the
cluster:

 1. member nodes
 2. node/shards mappings for each database

Both the nodes and shards are tracked in node-local couch databases.  Shards
are heavily used, so an ETS cache is also maintained for low-latency lookups.
The nodes and shards are synchronized via continuous CouchDB replication,
which serves as 'gossip' in Dynamo parlance. The shards ETS cache is kept in
sync based on membership and database event listeners.

A very important point to make here is that CouchDB does not necessarily
divide up each database into equal shards across the nodes of a cluster.  For
instance, in a 20-node cluster, you may have the need to create a small
database with very few documents. For efficiency reasons, you may create your
database with Q=4 and keep the default of N=3. This means you only have 12
shards total, so 8 nodes will hold none of the data for this database.  Given
this feature, we even shard use out across the cluster by altering the 'start'
node for the database's shards.

Splitting and merging shards is an immature feature of the system, and will
require attention in the near-term. We believe we can implement both
functions and perform them while the database remains online.

### Getting Started

Mem3 requires R13B03 or higher and can be built with [rebar][2], which comes
bundled in the repository. Rebar needs to be able to find the `couch_db.hrl`
header file; one way to accomplish this is to set ERL_LIBS to point to the
apps subdirectory of a CouchDB checkout, e.g.

    ERL_LIBS="/usr/local/src/couchdb/apps" ./rebar compile

### License
[Apache 2.0][3]

[1]: http://couchdb.apache.org
[2]: http://github.com/rebar/rebar
[3]: http://www.apache.org/licenses/LICENSE-2.0.html
