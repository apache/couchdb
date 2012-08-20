## Replicator

### Overview

Replication is a process that synchronizes two databases. All changes
in one database (the source), inserts, updates, and deletes, are made
to the second database (the target). The replication can be run on
either the node containing the source (a *push* replication), or on the
node containing the target database (a *pull* replication) or on a
third node different from either the source or the target.

#### Background

This current version is based on the latest from CouchDB as partially
documented on the CouchDB  [WIKI][1]. The first solid version that worked
was implemented by [Adam Kocoloski][4] and subsequently picked up by [Filipe
Manana][3], another CouchDB committer, who did two major reworkings. The
first added the [replicator][2] db and the second some performance
improvements and configurability to adjust factors that affect
performance.
 
This was then ported and integrated with dbcore. The changes required
were modest but there are notable differences in the use of the
replicator db as noted below. These notes are intended to be the
definitive resource for Cloudant users and will be kept in sync with
code changes

### Usage

The following examples all assume the use of the `_replicate2` end
point. Eventualy this will be replaced by `_replicate` but this allows
us to support both the new and old replicator during the testing
phase, .eg.:

    curl -X POST -H 'content-type:application/json'
    http://bitdiddle.cloudant.com/_replicate2 -d
    '{"source":http://cust1.cloudant.com/foo","target":"http://cust2.cloudant.com/bar"}' 

In the remaining examples we'll just note the JSON
bodies. `_replicate2` is really all there is to calling the
replicator, all the parameters are in the JSON body. For example a
simple pull replication (run from the target machine):

    {"source":"http://bitdiddle.cloudant.com/foo",
     "target":"http://nordier.cloudant.com/foo"}
To create the target db also:

    {"source":"http://mazincas.cloudant.com/foo",
     "target":"http://nordier.cloudant.com/foo",
     "create_target": true}

A continuous replication will stay running and as changes occur in the
source, replicate them to the target. Under the covers it makes use of
a continuous `_changes` feed:

    {"source":"http://bitdiddle.cloudant.com/foo",
     "target":"http://nordier.cloudant.com/foo",
     "continuous":true}

A replication can be stopped by posting the same body but with a
cancel property added:

    {"source":"http://bitdiddle.cloudant.com/foo",
     "target":"http://nordier.cloudant.com/foo",
     "continuous":true,
     "cancel":true}

When a continuous replication is run a replication id is returned,
that provides an aditional method for cancelling the replication:

    {"source":"http://bitdiddle.cloudant.com/foo",
     "target":"http://nordier.cloudant.com/foo",
     "continuous":true}

    {"ok":true,"_local_id":"0a81b645497e6270611ec3419767a584+continuous+create_target"}

To cancel:

    {"replication_id":
    "0a81b645497e6270611ec3419767a584+continuous+create_target",
    "cancel": true}

These are the main use cases. Additionally there are filter functions
supported, JS functions that control whether a doc is replicated. They
work similar to views, .eg.:

    {
    "_id":"_design/foo",
    "filters": {
      "foo": function(doc, req) {
                   if (doc.name == req.query.key) {
                      return true;
                   } else {
                      return false;
                   }
                }
       }
    }

It's use is specified in the replication as follows:

    {"source":" "http://nordier.cloudant.com/foo", 
     "target":"http://beezlechaus.cloudant.com/foo",
     "filter":"foo/foo",
     "query_params": {"key":"value"}
    }

Some applications might need to just replicate a few docs in which
case `doc_ids` can be used:

    {"source":"http://nordier.cloudant.com/foo",
     "target":"http://mazincas.cloudant.com/foo",
     "doc_ids":["foo","bar","baz"]
    }

### Replicator DB

A replicator db is a new way to manage replications better by storing
them in a database. One just puts a JSON doc to the replicator db with
a body the same as if is were posted to _replicate2

    curl -X PUT 
    http://bitdiddle.cloudant.com/_replicator/repl1 -d
    '{"source":http://cust1.cloudant.com/foo",
      "target":"http://cust2.cloudant.com/bar"}'

The doc might look something like:

<pre>
{
    "_id": "repl1",
    "source":  "http://cust1.cloudant.com/foo",
    "target":  "http://cust1.cloudant.com/foo", 
    "create_target":  true,
    "_replication_id":  "c0ebe9256695ff083347cbf95f93e280",
    "_replication_state":  "triggered",
    "_replication_state_time":  "2011-06-07T16:54:35+01:00"
}
</pre>

This replication now persists across server restarts and the
replication can be cancelled by simply deleting the doc. The
`_replication_state` will change to `completed` when a replication
finishes, which may not happen in the case of a continuous
replication. It can also change to `error` if it fails.

### Advanced Configurations

There are a few additional arguments that can be passed to a
replication that govern it's performance. These are to be used with
care as they can have signifificant impact on the system in a
multi-tenant environment.

 * worker_processes - The default is 4. This controls how many
   separate processes will read from the changes manager and write to
   the target. A higher number can improve throughput.

 * worker_batch_size - The default is 500. This controls how many
   documents are processed. After each batch a checkpoint is written
   so this controls how frequently checkpointing occurs.

 * http_connections - The maximum number of http connections used per
   replication. The default is 20.

 * connection_timeout - How long a connection can remain idle, the
   default is 30000 (30s)

 * retries_per_request - When requests fail the number of times to
   retry, the default is 10. There is a wait period between each
   attempt, that begins with 0.25s and doubles on each iteration, with
   a cap at 5 minutes.

 * socket_options - a list of options that can be used with the socket
   connections. See the [erlang documentation][6] for details.


### Differences from CouchDB

CouchDB allows the replicator db to be shared by multiple users,
controlling access via update handlers. It also allows the replicator
db to be changed on the fly by setting a value in the config file. We
support neither of these in dbcore. Each account has it's own
replicator db, named _replicator, and that's it.

Another difference to note is in specifying urls. Using the name of a
local db works fine in a single CouchDB instance but makes less sense
in a clustered environment. Though we try to determine a full url to
use this doesn't work well in practice so it's a good rule of thumb to
always use the full urls in both the source and target.

### Overview of Code and Design

----

[1]: http://wiki.apache.org/couchdb/Replication
[2]: https://gist.github.com/832610
[3]: https://github.com/fdmanana
[4]: https://github.com/kocolosk
[5]: http://guide.couchdb.org/draft/replication.html
[6]: http://www.erlang.org/doc/man/inet.html#setopts-2
