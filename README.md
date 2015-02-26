This module implements /_cluster_setup and manages the setting up, duh, of a CouchDB cluster.

### Testing

```
git clone https://git-wip-us.apache.org/repos/asf/couchdb.git
cd couchdb
git checkout setup
./configure
make

# in dev/run comment out the line `connect_nodes("127.0.0.1", 15984)`

dev/run --admin a:b

# in a new terminal
src/setup/test/t.sh

```

The Plan:

N. End User Action
- What happens behind the scenes.


1. Launch CouchDB with `$ couchdb`, or init.d, or any other way, exactly
like it is done in 1.x.x.
- CouchDB launches and listens on 127.0.0.1:5984

From here on, there are two paths, one is via Fauxton (a) the other is
using a HTTP endpoint (b). Fauxton just uses the HTTP endpoint in (b).
(b) can be used to set up a cluster programmatically.


2.a. Go to Fauxton. There is a “Cluster Setup” tab in the sidebar. Go
to the tab and get presented with a form that asks you to enter an admin
username, admin password and optionally a bind_address and port to bind
to publicly. Submit the form with the [Enable Cluster] button.

If this is a single node install that already has an admin set up, there
is no need to ask for admin credentials here. If the bind_address is !=
127.0.0.1, we can skip this entirely and Fauxton can show the add_node
UI right away.

- POST to /_setup with
```
  {
    "action": "enable_cluster",
    "admin": {
      "user": "username",
      "pass": "password"
    },
    ["bind_address": "xxxx",]
    ["port": yyyy]
  }
```

This sets up the admin user on the current node and binds to 0.0.0.0:5984
or the specified ip:port. Logs admin user into Fauxton automatically.

2.b. POST to /_setup as shown above.

Repeat on all nodes.
- keep the same username/password everywhere.


3. Pick any one node, for simplicity use the first one, to be the
“setup coordination node”.
- this is a “master” node that manages the setup and requires all
  other nodes to be able to see it and vice versa. Setup won’t work
  with unavailable nodes (duh). The notion of “master” will be gone
  once the setup is finished. At that point, the system has no
  master node. Ignore I ever said “master”.

a. Go to Fauxton / Cluster Setup, once we have enabled the cluster, the
UI shows an “Add Node” interface with the fields admin, and node:
- POST to /_setup with
```
  {
    "action": "add_node",
    "admin": { // should be auto-filled from Fauxton, store plaintext PW in
               // localStorage until we finish_cluster or timeout.
      "user": "username",
      "pass": "password"
    },
    "node": {
      "host": "hostname",
      ["port": 5984]
    }
  }
```

b. as in a, but without the Fauxton bits, just POST to /_setup
- this request will do this:
 - on the “setup coordination node”:
  - check if we have an Erlang Cookie Secret. If not, generate
    a UUID and set the erlang cookie to to that UUID.
    - store the cookie in config.ini, re-set_cookie() on startup.
  - make a POST request to the node specified in the body above
    using the admin credentials in the body above:
    POST to http://username:password@node_b:5984/_setup with:
```
    {
      "action": "receive_cookie",
      "cookie": "<secretcookie>",
    }
```

  - when the request to node B returns, we know the Erlang-level
    inter-cluster communication is enabled and we can start adding
    the node on the CouchDB level. To do that, the “setup
    coordination node” does this to it’s own HTTP endpoint:
    PUT /nodes/node_b:5984 or the same thing with internal APIs.

- Repeat for all nodes.
- Fauxton keeps a list of all set up nodes for users to see.


4.a. When all nodes are added, click the [Finish Cluster Setup] button
in Fauxton.
- this does POST /_setup
```
  {
    "action": "finish_setup"
  }
```

b. Same as in a.

- this manages the final setup bits, like creating the _users,
  _replicator and _metadata, _db_updates endpoints and
  whatever else is needed. // TBD: collect what else is needed.


## The Setup Endpoint

This is not a REST-y endpoint, it is a simple state machine operated
by HTTP POST with JSON bodies that have an `action` field.

### State 1: No Cluster Enabled

This is right after starting a node for the first time, and any time
before the cluster is enabled as outlined above.

```
GET /_setup
{"state": "cluster_disabled"}

POST /_setup {"action":"enable_cluster"...} -> Transition to State 2
POST /_setup {"action":"enable_cluster"...} with empty admin user/pass or invalid host/post or host/port not available -> Error
POST /_setup {"action":"anything_but_enable_cluster"...} -> Error
```

### State 2: Cluster enabled, admin user set, waiting for nodes to be added.

```
GET /_setup
{"state":"cluster_enabled","nodes":[]}

POST /_setup {"action":"enable_cluster"...} -> Error
POST /_setup {"action":"add_node"...} -> Stay in State 2, but return "nodes":["node B"}] on GET
POST /_setup {"action":"add_node"...} -> if target node not available, Error
POST /_setup {"action":"finish_cluster"} with no nodes set up -> Error
POST /_setup {"action":"finish_cluster"} -> Transition to State 3
POST /_setup {"action":"delete_node"...} -> Stay in State 2, but delete node from /nodes, reflect the change in GET /_setup
POST /_setup {"action":"delete_node","node":"unknown"} -> Error Unknown Node
```

### State 3: Cluster set up, all nodes operational

```
GET /_setup
{"state":"cluster_finished","nodes":["node a", "node b", ...]}

POST /_setup {"action":"enable_cluster"...} -> Error
POST /_setup {"action":"finish_cluster"...} -> Stay in State 3, do nothing
POST /_setup {"action":"add_node"...} -> Error
POST /_setup?i_know_what_i_am_doing=true {"action":"add_node"...} -> Add node, stay in State 3.
POST /_setup {"action":"delete_node"...} -> Stay in State 3, but delete node from /nodes, reflect the change in GET /_setup
POST /_setup {"action":"delete_node","node":"unknown"} -> Error Unknown Node
```

// TBD: we need to persist the setup state somewhere.
