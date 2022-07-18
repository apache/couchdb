Rexi is a tailor-made RPC server application for sending [CouchDB][1] operations to nodes in a cluster.  It is used in [BigCouch][2] as the remote procedure vehicle to get [fabric][6] functions to execute on remote cluster nodes.

Rexi better fits the needs of the BigCouch distributed data store by dropping some unneeded overhead in rex, the RPC server that ships with Erlang/OTP.  Rexi is optimized for the case when you need to spawn a bunch of remote processes.  Cast messages are sent from the origin to the remote rexi server, and local processes are spawned from there, which is vastly more efficient than spawning remote processes from the origin.  You still get monitoring of the remote processes, but the request-handling process doesn't get stuck trying to connect to an overloaded/dead node.  'rexi_DOWN' messages will arrive at the client eventually.  This has been an extremely advantageous mix of latency and failure detection, vastly improving the performance of BigCouch.

Rexi is used in conjunction with 'Fabric' which is also an application within BigCouch, but can be used on a stand-alone basis.

### Getting Started
Rexi requires R13B03 or higher and can be built with [rebar][7], which comes bundled in the repository.

### License
[Apache 2.0][3]

### Contact
 * [http://cloudant.com][4]
 * [info@cloudant.com][5]

[1]: http://couchdb.apache.org
[2]: http://github.com/cloudant/BigCouch
[3]: http://www.apache.org/licenses/LICENSE-2.0.html
[4]: http://cloudant.com
[5]: mailto:info@cloudant.com
[6]: http://github.com/cloudant/fabric
[7]: http://github.com/basho/rebar
