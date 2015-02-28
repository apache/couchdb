## fabric

Fabric is a collection of proxy functions for [CouchDB][1] operations in a cluster. These functions are used in CouchDB as the remote procedure endpoints on each of the cluster nodes.

For example, creating a database is a straightforward task in CouchDB 1.x, but for a clustered CouchDB, each node that will store a shard for the database needs to receive and execute a fabric function. The node handling the request also needs to compile the results from each of the nodes and respond accordingly to the client.

Fabric is used in conjunction with 'Rexi' which is also an application within CouchDB.

### Getting Started
Fabric requires R13B03 or higher and can be built with [rebar][3].

### License
[Apache 2.0][2]


[1]: http://couchdb.apache.org
[2]: http://www.apache.org/licenses/LICENSE-2.0.html
[3]: http://github.com/basho/rebar
