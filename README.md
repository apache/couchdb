## fabric

Fabric is a collection of proxy functions for [CouchDB][1] operations in a cluster.  These functions are used in [BigCouch][2] as the remote procedure endpoints on each of the cluster nodes.

For example, creating a database is a straightforward task in standalone CouchDB, but for BigCouch, each node that will store a shard for the database needs to receive and execute a fabric function.  The node handling the request also needs to compile the results from each of the nodes and respond accordingly to the client.

Fabric is used in conjunction with 'Rexi' which is also an application within BigCouch.

### Getting Started
Fabric requires R13B03 or higher and can be built with [rebar][6], which comes bundled in the repository.

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
