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

.. _replicator:

Replicator Database
===================

The ``_replicator`` database works like any other in CouchDB, but documents
added to it will trigger replications. Creating (``PUT`` or ``POST``) a
document to start a replication. ``DELETE`` a replicaiton document to
cancel an ongoing replication.

These documents have exactly the same content as the JSON objects we use to
``POST`` to ``_replicate`` (fields ``source``, ``target``, ``create_target``,
``continuous``, ``doc_ids``, ``filter``, ``query_params``, ``use_checkpoints``,
``checkpoint_interval``).

Replication documents can have a user defined ``_id`` (handy for finding a
specific replication request later). Design Documents
(and ``_local`` documents) added to the replicator database are ignored.

The default name of this database is ``_replicator``. The name can be
changed in the ``local.ini`` configuration, section ``[replicator]``,
parameter ``db``.

Basics
------

Let's say you POST the following document into ``_replicator``:

.. code-block:: javascript

    {
        "_id": "my_rep",
        "source":  "http://myserver.com:5984/foo",
        "target":  "bar",
        "create_target":  true
    }

In the couch log you'll see 2 entries like these:

.. code-block:: text

    [Thu, 17 Feb 2011 19:43:59 GMT] [info] [<0.291.0>] Document `my_rep` triggered replication `c0ebe9256695ff083347cbf95f93e280+create_target`
    [Thu, 17 Feb 2011 19:44:37 GMT] [info] [<0.124.0>] Replication `c0ebe9256695ff083347cbf95f93e280+create_target` finished (triggered by document `my_rep`)

As soon as the replication is triggered, the document will be updated by
CouchDB with 3 new fields:

.. code-block:: javascript

    {
        "_id": "my_rep",
        "source":  "http://myserver.com:5984/foo",
        "target":  "bar",
        "create_target":  true,
        "_replication_id":  "c0ebe9256695ff083347cbf95f93e280",
        "_replication_state":  "triggered",
        "_replication_state_time":  1297974122
    }

Special fields set by the replicator start with the prefix
``_replication_``.

-  ``_replication_id``

   The ID internally assigned to the replication. This is also the ID
   exposed by ``/_active_tasks``.

-  ``_replication_state``

   The current state of the replication.

-  ``_replication_state_time``

   A Unix timestamp (number of seconds since 1 Jan 1970) that tells us
   when the current replication state (marked in ``_replication_state``)
   was set.

-  ``_replication_state_reason``

   If ``replication_state`` is ``error``, this field contains the reason.

.. code-block:: javascript

    {
    "_id": "my_rep",
    "_rev": "2-9f2c0d9372f4ee4dc75652ab8f8e7c70",
    "source": "foodb",
    "target": "bardb",
    "_replication_state": "error",
    "_replication_state_time": "2013-12-13T18:48:00+01:00",
    "_replication_state_reason": "db_not_found: could not open foodb",
    "_replication_id": "fe965cdc47b4d5f6c02811d9d351ac3d"
    }

When the replication finishes, it will update the ``_replication_state``
field (and ``_replication_state_time``) with the value ``completed``, so
the document will look like:

.. code-block:: javascript

    {
        "_id": "my_rep",
        "source":  "http://myserver.com:5984/foo",
        "target":  "bar",
        "create_target":  true,
        "_replication_id":  "c0ebe9256695ff083347cbf95f93e280",
        "_replication_state":  "completed",
        "_replication_state_time":  1297974122
    }

When an error happens during replication, the ``_replication_state``
field is set to ``error`` (and ``_replication_state_reason`` and
``_replication_state_time`` are updated).

When you PUT/POST a document to the ``_replicator`` database, CouchDB
will attempt to start the replication up to 10 times (configurable under
``[replicator]``, parameter ``max_replication_retry_count``). If it
fails on the first attempt, it waits 5 seconds before doing a second
attempt. If the second attempt fails, it waits 10 seconds before doing a
third attempt. If the third attempt fails, it waits 20 seconds before
doing a fourth attempt (each attempt doubles the previous wait period).
When an attempt fails, the Couch log will show you something like:

.. code-block:: text

    [error] [<0.149.0>] Error starting replication `67c1bb92010e7abe35d7d629635f18b6+create_target` (document `my_rep_2`): {db_not_found,<<"could not open http://myserver:5986/foo/">>

.. note::
   The ``_replication_state`` field is only set to ``error`` when all
   the attempts were unsuccessful.

There are only 3 possible values for the ``_replication_state`` field:
``triggered``, ``completed`` and ``error``. Continuous replications
never get their state set to ``completed``.

Documents describing the same replication
-----------------------------------------

Lets suppose 2 documents are added to the ``_replicator`` database in
the following order:

.. code-block:: javascript

    {
        "_id": "doc_A",
        "source":  "http://myserver.com:5984/foo",
        "target":  "bar"
    }

and

.. code-block:: javascript

    {
        "_id": "doc_B",
        "source":  "http://myserver.com:5984/foo",
        "target":  "bar"
    }

Both describe exactly the same replication (only their ``_ids`` differ).
In this case document ``doc_A`` triggers the replication, getting
updated by CouchDB with the fields ``_replication_state``,
``_replication_state_time`` and ``_replication_id``, just like it was
described before. Document ``doc_B`` however, is only updated with one
field, the ``_replication_id`` so it will look like this:

.. code-block:: javascript

    {
        "_id": "doc_B",
        "source":  "http://myserver.com:5984/foo",
        "target":  "bar",
        "_replication_id":  "c0ebe9256695ff083347cbf95f93e280"
    }

While document ``doc_A`` will look like this:

.. code-block:: javascript

    {
        "_id": "doc_A",
        "source":  "http://myserver.com:5984/foo",
        "target":  "bar",
        "_replication_id":  "c0ebe9256695ff083347cbf95f93e280",
        "_replication_state":  "triggered",
        "_replication_state_time":  1297974122
    }

Note that both document get exactly the same value for the
``_replication_id`` field. This way you can identify which documents
refer to the same replication - you can for example define a view which
maps replication IDs to document IDs.

Canceling replications
----------------------

To cancel a replication simply ``DELETE`` the document which triggered
the replication. The Couch log will show you an entry like the
following:

.. code-block:: text

    [Thu, 17 Feb 2011 20:16:29 GMT] [info] [<0.125.0>] Stopped replication `c0ebe9256695ff083347cbf95f93e280+continuous+create_target` because replication document `doc_A` was deleted

.. note::
   You need to ``DELETE`` the document that triggered the replication.
   ``DELETE``-ing another document that describes the same replication
   but did not trigger it, will not cancel the replication.

Server restart
--------------

When CouchDB is restarted, it checks its ``_replicator`` database and
restarts any replication that is described by a document that either has
its ``_replication_state`` field set to ``triggered`` or it doesn't have
yet the ``_replication_state`` field set.

.. note::
   Continuous replications always have a ``_replication_state`` field
   with the value ``triggered``, therefore they're always restarted
   when CouchDB is restarted.

Changing the Replicator Database
--------------------------------

Imagine your replicator database (default name is ``_replicator``) has the
two following documents that represent pull replications from servers A
and B:

.. code-block:: javascript

    {
        "_id": "rep_from_A",
        "source":  "http://aserver.com:5984/foo",
        "target":  "foo_a",
        "continuous":  true,
        "_replication_id":  "c0ebe9256695ff083347cbf95f93e280",
        "_replication_state":  "triggered",
        "_replication_state_time":  1297971311
    }

.. code-block:: javascript

    {
        "_id": "rep_from_B",
        "source":  "http://bserver.com:5984/foo",
        "target":  "foo_b",
        "continuous":  true,
        "_replication_id":  "231bb3cf9d48314eaa8d48a9170570d1",
        "_replication_state":  "triggered",
        "_replication_state_time":  1297974122
    }

Now without stopping and restarting CouchDB, you change the name of the
replicator database to ``another_replicator_db``:

.. code-block:: bash

    $ curl -X PUT http://localhost:5984/_config/replicator/db -d '"another_replicator_db"'
    "_replicator"

As soon as this is done, both pull replications defined before, are
stopped. This is explicitly mentioned in CouchDB's log:

.. code-block:: text

    [Fri, 11 Mar 2011 07:44:20 GMT] [info] [<0.104.0>] Stopping all ongoing replications because the replicator database was deleted or changed
    [Fri, 11 Mar 2011 07:44:20 GMT] [info] [<0.127.0>] 127.0.0.1 - - PUT /_config/replicator/db 200

Imagine now you add a replication document to the new replicator
database named ``another_replicator_db``:

.. code-block:: javascript

    {
        "_id": "rep_from_X",
        "source":  "http://xserver.com:5984/foo",
        "target":  "foo_x",
        "continuous":  true
    }

From now own you have a single replication going on in your system: a
pull replication pulling from server X. Now you change back the
replicator database to the original one ``_replicator``:

::

    $ curl -X PUT http://localhost:5984/_config/replicator/db -d '"_replicator"'
    "another_replicator_db"

Immediately after this operation, the replication pulling from server X
will be stopped and the replications defined in the ``_replicator``
database (pulling from servers A and B) will be resumed.

Changing again the replicator database to ``another_replicator_db`` will
stop the pull replications pulling from servers A and B, and resume the
pull replication pulling from server X.

Replicating the replicator database
-----------------------------------

Imagine you have in server C a replicator database with the two
following pull replication documents in it:

.. code-block:: javascript

    {
         "_id": "rep_from_A",
         "source":  "http://aserver.com:5984/foo",
         "target":  "foo_a",
         "continuous":  true,
         "_replication_id":  "c0ebe9256695ff083347cbf95f93e280",
         "_replication_state":  "triggered",
         "_replication_state_time":  1297971311
    }

.. code-block:: javascript

    {
         "_id": "rep_from_B",
         "source":  "http://bserver.com:5984/foo",
         "target":  "foo_b",
         "continuous":  true,
         "_replication_id":  "231bb3cf9d48314eaa8d48a9170570d1",
         "_replication_state":  "triggered",
         "_replication_state_time":  1297974122
    }

Now you would like to have the same pull replications going on in server
D, that is, you would like to have server D pull replicating from
servers A and B. You have two options:

-  Explicitly add two documents to server's D replicator database

-  Replicate server's C replicator database into server's D replicator
   database

Both alternatives accomplish exactly the same goal.

Delegations
-----------

Replication documents can have a custom ``user_ctx`` property. This
property defines the user context under which a replication runs. For
the old way of triggering a replication (POSTing to ``/_replicate/``),
this property is not needed. That's because information about the
authenticated user is readily available during the replication, which is
not persistent in that case. Now, with the replicator database, the
problem is that information about which user is starting a particular
replication is only present when the replication document is written.
The information in the replication document and the replication itself
are persistent, however. This implementation detail implies that in the
case of a non-admin user, a ``user_ctx`` property containing the user's
name and a subset of their roles must be defined in the replication
document. This is enforced by the document update validation function
present in the default design document of the replicator database. The
validation function also ensures that non-admin users are unable to set
the value of the user context's ``name`` property to anything other than
their own user name. The same principle applies for roles.

For admins, the ``user_ctx`` property is optional, and if it's missing
it defaults to a user context with name ``null`` and an empty list of
roles, which means design documents won't be written to local targets.
If writing design documents to local targets is desired, the role
``_admin`` must be present in the user context's list of roles.

Also, for admins the ``user_ctx`` property can be used to trigger a
replication on behalf of another user. This is the user context that
will be passed to local target database document validation functions.

.. note::
   The ``user_ctx`` property only has effect for local endpoints.

Example delegated replication document:

.. code-block:: javascript

    {
         "_id": "my_rep",
         "source":  "http://bserver.com:5984/foo",
         "target":  "bar",
         "continuous":  true,
         "user_ctx": {
              "name": "joe",
              "roles": ["erlanger", "researcher"]
         }
    }

As stated before, the ``user_ctx`` property is optional for admins, while
being mandatory for regular (non-admin) users. When the roles property
of ``user_ctx`` is missing, it defaults to the empty list ``[]``.
