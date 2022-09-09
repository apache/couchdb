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

===================
Replicator Database
===================

.. versionchanged:: 2.1.0 Scheduling replicator was introduced.
   Replication states, by default are not written back to documents
   anymore. There are new replication job states and new API endpoints
   ``_scheduler/jobs`` and ``_scheduler/docs``.

.. versionchanged:: 3.2.0 Fair share scheduling was introduced. Multiple
   ``_replicator`` databases get an equal chance (configurable) of running
   their jobs. Previously replication jobs were scheduled without any regard of
   their originating database.

The ``_replicator`` database works like any other in CouchDB, but
documents added to it will trigger replications. Create (``PUT`` or
``POST``) a document to start replication. ``DELETE`` a replication
document to cancel an ongoing replication.

These documents have exactly the same content as the JSON objects we used to
``POST`` to ``_replicate`` (fields ``source``, ``target``, ``create_target``,
``create_target_params``, ``continuous``, ``doc_ids``, ``filter``,
``query_params``, ``use_checkpoints``, ``checkpoint_interval``).

Replication documents can have a user defined ``_id`` (handy for finding
a specific replication request later). Design Documents (and ``_local``
documents) added to the replicator database are ignored.

The default replicator database is ``_replicator``. Additional
replicator databases can be created. To be recognized as such by the
system, their database names should end with ``/_replicator``.

Basics
======

Let's say you POST the following document into ``_replicator``:

.. code-block:: javascript

    {
        "_id": "my_rep",
        "source": "http://myserver.com/foo",
        "target": {
            "url": "http://localhost:5984/bar",
            "auth": {
                "basic": {
                    "username": "user",
                    "password": "pass"
                }
            }
        },
        "create_target":  true,
        "continuous": true
    }

In the couch log you'll see 2 entries like these:

.. code-block:: text

    [notice] 2017-04-05T17:16:19.646716Z node1@127.0.0.1 <0.29432.0> -------- Replication `"a81a78e822837e66df423d54279c15fe+continuous+create_target"` is using:
        4 worker processes
        a worker batch size of 500
        20 HTTP connections
        a connection timeout of 30000 milliseconds
        10 retries per request
        socket options are: [{keepalive,true},{nodelay,false}]
    [notice] 2017-04-05T17:16:19.646759Z node1@127.0.0.1 <0.29432.0> -------- Document `my_rep` triggered replication `a81a78e822837e66df423d54279c15fe+continuous+create_target`

Replication state of this document can then be queried from
``http://adm:pass@localhost:5984/_scheduler/docs/_replicator/my_rep``

.. code-block:: json

     {
         "database": "_replicator",
         "doc_id": "my_rep",
         "error_count": 0,
         "id": "a81a78e822837e66df423d54279c15fe+continuous+create_target",
         "info": {
             "revisions_checked": 113,
             "missing_revisions_found": 113,
             "docs_read": 113,
             "docs_written": 113,
             "changes_pending": 0,
             "doc_write_failures": 0,
             "checkpointed_source_seq": "113-g1AAAACTeJzLYWBgYMpgTmHgz8tPSTV0MDQy1zMAQsMckEQiQ1L9____szKYE01ygQLsZsYGqcamiZjKcRqRxwIkGRqA1H-oSbZgk1KMLCzTDE0wdWUBAF6HJIQ",
             "source_seq": "113-g1AAAACTeJzLYWBgYMpgTmHgz8tPSTV0MDQy1zMAQsMckEQiQ1L9____szKYE01ygQLsZsYGqcamiZjKcRqRxwIkGRqA1H-oSbZgk1KMLCzTDE0wdWUBAF6HJIQ",
             "through_seq": "113-g1AAAACTeJzLYWBgYMpgTmHgz8tPSTV0MDQy1zMAQsMckEQiQ1L9____szKYE01ygQLsZsYGqcamiZjKcRqRxwIkGRqA1H-oSbZgk1KMLCzTDE0wdWUBAF6HJIQ"
         },
         "last_updated": "2017-04-05T19:18:15Z",
         "node": "node1@127.0.0.1",
         "source_proxy": null,
         "target_proxy": null,
         "source": "http://myserver.com/foo/",
         "start_time": "2017-04-05T19:18:15Z",
         "state": "running",
         "target": "http://localhost:5984/bar/"
     }

The state is ``running``. That means replicator has scheduled this
replication job to run. Replication document contents stay the same.
Previously, before version 2.1, it was updated with the ``triggered``
state.

The replication job will also appear in

``http://adm:pass@localhost:5984/_scheduler/jobs``

.. code-block:: json

      {
          "jobs": [
              {
                  "database": "_replicator",
                  "doc_id": "my_rep",
                  "history": [
                      {
                          "timestamp": "2017-04-05T19:18:15Z",
                          "type": "started"
                      },
                      {
                          "timestamp": "2017-04-05T19:18:15Z",
                          "type": "added"
                      }
                  ],
                  "id": "a81a78e822837e66df423d54279c15fe+continuous+create_target",
                  "info": {
                      "changes_pending": 0,
                      "checkpointed_source_seq": "113-g1AAAACTeJzLYWBgYMpgTmHgz8tPSTV0MDQy1zMAQsMckEQiQ1L9____szKYE01ygQLsZsYGqcamiZjKcRqRxwIkGRqA1H-oSbZgk1KMLCzTDE0wdWUBAF6HJIQ",
                      "doc_write_failures": 0,
                      "docs_read": 113,
                      "docs_written": 113,
                      "missing_revisions_found": 113,
                      "revisions_checked": 113,
                      "source_seq": "113-g1AAAACTeJzLYWBgYMpgTmHgz8tPSTV0MDQy1zMAQsMckEQiQ1L9____szKYE01ygQLsZsYGqcamiZjKcRqRxwIkGRqA1H-oSbZgk1KMLCzTDE0wdWUBAF6HJIQ",
                      "through_seq": "113-g1AAAACTeJzLYWBgYMpgTmHgz8tPSTV0MDQy1zMAQsMckEQiQ1L9____szKYE01ygQLsZsYGqcamiZjKcRqRxwIkGRqA1H-oSbZgk1KMLCzTDE0wdWUBAF6HJIQ"
                  },
                  "node": "node1@127.0.0.1",
                  "pid": "<0.1174.0>",
                  "source": "http://myserver.com/foo/",
                  "start_time": "2017-04-05T19:18:15Z",
                  "target": "http://localhost:5984/bar/",
                  "user": null
              }
          ],
          "offset": 0,
          "total_rows": 1
      }

``_scheduler/jobs`` shows more information, such as a detailed history of
state changes. If a persistent replication has not yet started,
has failed, or is completed, information about its state can only be found
in ``_scheduler/docs``. Keep in mind that some replication documents could be
invalid and could not become a replication job. Others might be delayed
because they are fetching data from a slow source database.

If there is an error, for example if the source database is missing, the
replication job will crash and retry after a wait period. Each
successive crash will result in a longer waiting period.

For example, POST-ing this document

.. code-block:: javascript

    {
        "_id": "my_rep_crashing",
        "source": "http://myserver.com/missing",
        "target": {
            "url": "http://localhost:5984/bar",
            "auth": {
                "basic": {
                    "username": "user",
                    "password": "pass"
                }
            }
        },
        "create_target":  true,
        "continuous": true
    }

when source database is missing, will result in periodic starts and
crashes with an increasingly larger interval. The ``history`` list from
``_scheduler/jobs`` for this replication would look something like this:

.. code-block:: json

    [
          {
              "reason": "db_not_found: could not open http://adm:*****@localhost:5984/missing/",
              "timestamp": "2017-04-05T20:55:10Z",
              "type": "crashed"
          },
          {
              "timestamp": "2017-04-05T20:55:10Z",
              "type": "started"
          },
          {
              "reason": "db_not_found: could not open http://adm:*****@localhost:5984/missing/",
              "timestamp": "2017-04-05T20:47:10Z",
              "type": "crashed"
          },
          {
              "timestamp": "2017-04-05T20:47:10Z",
              "type": "started"
          }
    ]

``_scheduler/docs`` shows a shorter summary:

.. code-block:: json

    {
          "database": "_replicator",
          "doc_id": "my_rep_crashing",
          "error_count": 6,
          "id": "cb78391640ed34e9578e638d9bb00e44+create_target",
          "info": {
               "error": "db_not_found: could not open http://myserver.com/missing/"
          },
          "last_updated": "2017-04-05T20:55:10Z",
          "node": "node1@127.0.0.1",
          "source_proxy": null,
          "target_proxy": null,
          "source": "http://myserver.com/missing/",
          "start_time": "2017-04-05T20:38:34Z",
          "state": "crashing",
          "target": "http://localhost:5984/bar/"
    }

Repeated crashes are described as a ``crashing`` state. ``-ing`` suffix
implies this is a temporary state. User at any moment could create the
missing database and then replication job could return back to the
normal.

Documents describing the same replication
=========================================

Lets suppose 2 documents are added to the ``_replicator`` database in
the following order:

.. code-block:: javascript

    {
        "_id": "my_rep",
        "source": "http://myserver.com/foo",
        "target":  "http://user:pass@localhost:5984/bar",
        "create_target":  true,
        "continuous": true
    }

and

.. code-block:: javascript

    {
        "_id": "my_rep_dup",
        "source": "http://myserver.com/foo",
        "target":  "http://user:pass@localhost:5984/bar",
        "create_target":  true,
        "continuous": true
    }

Both describe exactly the same replication (only their ``_ids`` differ).
In this case document ``my_rep`` triggers the replication, while
``my_rep_dup``` will fail. Inspecting ``_scheduler/docs`` explains
exactly why it failed:

.. code-block:: json

        {
            "database": "_replicator",
            "doc_id": "my_rep_dup",
            "error_count": 1,
            "id": null,
            "info": {
                "error": "Replication `a81a78e822837e66df423d54279c15fe+continuous+create_target` specified by document `my_rep_dup` already started, triggered by document `my_rep` from db `_replicator`"
            },
            "last_updated": "2017-04-05T21:41:51Z",
            "source": "http://myserver.com/foo/",
            "start_time": "2017-04-05T21:41:51Z",
            "state": "failed",
            "target": "http://user:****@localhost:5984/bar",
        }

Notice the state for this replication is ``failed``. Unlike
``crashing``, ``failed`` state is terminal. As long as both documents
are present the replicator will not retry to run ``my_rep_dup``
replication. Another reason could be malformed documents. For example if
worker process count is specified as a string (``"worker_processes": "a
few"``) instead of an integer, failure will occur.

Replication Scheduler
=====================

Once replication jobs are created they are managed by the scheduler. The
scheduler is the replication component which periodically stops some
jobs and starts others. This behavior makes it possible to have a
larger number of jobs than the cluster could run simultaneously.
Replication jobs which keep failing will be penalized and forced to
wait. The wait time increases exponentially with each consecutive
failure.

When deciding which jobs to stop and which to start, the scheduler uses
a round-robin algorithm to ensure fairness. Jobs which have been running
the longest time will be stopped, and jobs which have been waiting the
longest time will be started.

.. note:: Non-continuous (normal) replication are treated differently
          once they start running. See :ref:`Normal vs Continuous
          Replications` section for more information.

The behavior of the scheduler can configured via ``max_jobs``,
``interval`` and ``max_churn`` options. See :ref:`Replicator
configuration section <config/replicator>` for additional information.

.. _replicator/states:

Replication states
==================

Replication jobs during their life-cycle pass through various states.
This is a diagram of all the states and transitions between them:

.. figure:: ../../images/replication-state-diagram.svg
     :align: center
     :alt: Replication state diagram

     Replication state diagram

Blue and yellow shapes represent replication job states.

Trapezoidal shapes represent external APIs, that's how users interact
with the replicator. Writing documents to ``_replicator`` is the
preferred way of creating replications, but posting to the
``_replicate`` HTTP endpoint is also supported.

Six-sided shapes are internal API boundaries. They are optional for this
diagram and are only shown as additional information to help clarify how the
replicator works. There are two processing stages: the first is where
replication documents are parsed and become replication jobs, and the second is
the scheduler itself. The scheduler runs replication jobs, periodically
stopping and starting some. Jobs posted via the ``_replicate`` endpoint bypass
the first component and go straight to the scheduler.

States descriptions
-------------------

Before explaining the details of each state, it is worth noticing that
color and shape of each state in the diagram:

`Blue` vs `yellow` partitions states into "healthy" and "unhealthy",
respectively. Unhealthy states indicate something has gone wrong and it
might need user's attention.

`Rectangle` vs `oval` separates "terminal" states from "non-terminal"
ones. Terminal states are those which will not transition to other
states any more. Informally, jobs in a terminal state will not be
retried and don't consume memory or CPU resources.

 * ``Initializing``: Indicates replicator has noticed the change from
   the replication document. Jobs should transition quickly through this
   state. Being stuck here for a while could mean there is an internal
   error.

 * ``Failed``: Replication document could not be processed and turned
   into a valid replication job for the scheduler. This state is
   terminal and requires user intervention to fix the problem. A typical
   reason for ending up in this state is a malformed document. For
   example, specifying an integer for a parameter which accepts a
   boolean. Another reason for failure could be specifying a duplicate
   replication. A duplicate replication is a replication with identical
   parameters but a different document ID.

 * ``Error``: Replication document update could not be turned into a
   replication job. Unlike the ``Failed`` state, this one is temporary,
   and replicator will keep retrying periodically. There is an
   exponential backoff applied in case of consecutive failures. The main
   reason this state exists is to handle filtered replications with
   custom user functions. Filter function content is needed in order to
   calculate the replication ID. A replication job could not be created
   until the function code is retrieved. Because retrieval happens over
   the network, temporary failures have to be handled.

 * ``Running``: Replication job is running normally. This means, there
   might be a change feed open, and if changes are noticed, they would
   be processed and posted to the target. Job is still considered
   ``Running`` even if its workers are currently not streaming changes
   from source to target and are just waiting on the change feed.
   Continuous replications will most likely end up in this state.

 * ``Pending``: Replication job is not running and is waiting its turn.
   This state is reached when the number of replication jobs added to
   the scheduler exceeds ``replicator.max_jobs``. In that case scheduler
   will periodically stop and start subsets of jobs trying to give each
   one a fair chance at making progress.

 * ``Crashing``: Replication job has been successfully added to the
   replication scheduler. However an error was encountered during the
   last run. Error could be a network failure, a missing source
   database, a permissions error, etc. Repeated consecutive crashes
   result in an exponential backoff. This state is considered temporary
   (non-terminal) and replication jobs will be periodically retried.
   Maximum backoff interval is around a day or so.

 * ``Completed``: This is a terminal, successful state for
   non-continuous replications. Once in this state the replication is
   "forgotten" by the scheduler and it doesn't consume any more CPU or
   memory resources. Continuous replication jobs will never reach this
   state.

.. _Normal vs Continuous Replications:

Normal vs Continuous Replications
---------------------------------

Normal (non-continuous) replications once started will be allowed to run
to completion. That behavior is to preserve their semantics of
replicating a snapshot of the source database to the target. For example
if new documents are added to the source after the replication are
started, those updates should not show up on the target database.
Stopping and restring a normal replication would violate that
constraint.

.. warning:: When there is a mix of continuous and normal replications,
    once normal replication are scheduled to run, they might temporarily
    starve continuous replication jobs.

However, normal replications will still be stopped and rescheduled if an
operator reduces the value for the maximum number of replications. This
is so that if an operator decides replications are overwhelming a node
that it has the ability to recover. Any stopped replications will be
resubmitted to the queue to be rescheduled.

Compatibility Mode
==================

Previous version of CouchDB replicator wrote state updates back to
replication documents. In cases where user code programmatically read
those states, there is compatibility mode enabled via a configuration
setting::

  [replicator]
  update_docs = true

In this mode replicator will continue to write state updates to the
documents.

To effectively disable the scheduling behavior, which periodically stop
and starts jobs, set ``max_jobs`` configuration setting to a large
number. For example::

  [replicator]
  max_jobs = 9999999

See :ref:`Replicator configuration section <config/replicator>` for
other replicator configuration options.

Canceling replications
======================

To cancel a replication simply ``DELETE`` the document which triggered
the replication. To update a replication, for example, change the number
of worker or the source, simply update the document with new data. If
there is extra application-specific data in the replication documents,
that data is ignored by the replicator.

Server restart
==============

When CouchDB is restarted, it checks its ``_replicator`` databases and
restarts replications described by documents if they are not already in
in a ``completed`` or ``failed`` state. If they are, they are ignored.

Clustering
==========

In a cluster, replication jobs are balanced evenly among all the nodes
nodes such that a replication job runs on only one node at a time.

Every time there is a cluster membership change, that is when nodes are
added or removed, as it happens in a rolling reboot, replicator
application will notice the change, rescan all the document and running
replication, and re-evaluate their cluster placement in light of the new
set of live nodes. This mechanism also provides replication fail-over in
case a node fails. Replication jobs started from replication documents
(but not those started from ``_replicate`` HTTP endpoint) will
automatically migrate one of the live nodes.

Additional Replicator Databases
===============================

Imagine replicator database (``_replicator``) has these two documents
which represent pull replications from servers A and B:

.. code-block:: javascript

    {
        "_id": "rep_from_A",
        "source":  "http://aserver.com:5984/foo",
        "target": {
            "url": "http://localhost:5984/foo_a",
            "auth": {
                "basic": {
                    "username": "user",
                    "password": "pass"
                }
            }
        },
        "continuous":  true
    }

.. code-block:: javascript

    {
        "_id": "rep_from_B",
        "source":  "http://bserver.com:5984/foo",
        "target": {
            "url": "http://localhost:5984/foo_b",
            "auth": {
                "basic": {
                    "username": "user",
                    "password": "pass"
                }
            }
        },
        "continuous":  true
    }

Now without stopping and restarting CouchDB, add another replicator
database. For example ``another/_replicator``:

.. code-block:: bash

    $ curl -X PUT http://user:pass@localhost:5984/another%2F_replicator/
    {"ok":true}

.. note::
   A / character in a database name, when used in a URL, should be escaped.

Then add a replication document to the new replicator database:

.. code-block:: javascript

    {
        "_id": "rep_from_X",
        "source":  "http://xserver.com:5984/foo",
        "target":  "http://user:pass@localhost:5984/foo_x",
        "continuous":  true
    }

From now on, there are three replications active in the system: two
replications from A and B, and a new one from X.

Then remove the additional replicator database:

.. code-block:: bash

    $ curl -X DELETE http://user:pass@localhost:5984/another%2F_replicator/
    {"ok":true}

After this operation, replication pulling from server X will be stopped
and the replications in the ``_replicator`` database (pulling from
servers A and B) will continue.

Fair Share Job Scheduling
=========================

When multiple ``_replicator`` databases are used, and the total number
of jobs on any node is greater than ``max_jobs``, replication jobs
will be scheduled such that each of the ``_replicator`` databases by
default get an equal chance of running their jobs.

This is accomplished by assigning a number of "shares" to each
``_replicator`` database and then automatically adjusting the
proportion of running jobs to match each database's proportion of
shares. By default, each ``_replicator`` database is assigned 100
shares. It is possible to alter the share assignments for each
individual ``_replicator`` database in the :ref:`[replicator.shares]
<config/replicator.shares>` configuration section.

The fair share behavior is perhaps easier described with a set of
examples. Each example assumes the default of ``max_jobs = 500``, and
two replicator databases: ``_replicator`` and ``another/_replicator``.

Example 1: If ``_replicator`` has 1000 jobs and
``another/_replicator`` has 10, the scheduler will run about 490 jobs
from ``_replicator`` and 10 jobs from ``another/_replicator``.

Example 2: If ``_replicator`` has 200 jobs and ``another/_replicator``
also has 200 jobs, all 400 jobs will get to run as the sum of all the
jobs is less than the ``max_jobs`` limit.

Example 3: If both replicator databases have 1000 jobs each, the
scheduler will run about 250 jobs from each database on average.

Example 4: If both replicator databases have 1000 jobs each, but
``_replicator`` was assigned 400 shares, then on average the scheduler
would run about 400 jobs from ``_replicator`` and 100 jobs from
``_another/replicator``.

The proportions described in the examples are approximate and might
oscillate a bit, and also might take anywhere from tens of minutes to
an hour to converge.

Replicating the replicator database
===================================

Imagine you have in server C a replicator database with the two
following pull replication documents in it:

.. code-block:: javascript

    {
         "_id": "rep_from_A",
         "source":  "http://aserver.com:5984/foo",
         "target":  "http://user:pass@localhost:5984/foo_a",
         "continuous":  true
    }

.. code-block:: javascript

    {
         "_id": "rep_from_B",
         "source":  "http://bserver.com:5984/foo",
         "target":  "http://user:pass@localhost:5984/foo_b",
         "continuous":  true
    }

Now you would like to have the same pull replications going on in server
D, that is, you would like to have server D pull replicating from
servers A and B. You have two options:

- Explicitly add two documents to server's D replicator database

- Replicate server's C replicator database into server's D replicator
  database

Both alternatives accomplish exactly the same goal.

Delegations
===========

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

.. note:: The ``user_ctx`` property only has effect for local endpoints.

Example delegated replication document:

.. code-block:: javascript

    {
        "_id": "my_rep",
        "source":  "http://bserver.com:5984/foo",
        "target":  "http://user:pass@localhost:5984/bar",
        "continuous":  true,
        "user_ctx": {
            "name": "joe",
            "roles": ["erlanger", "researcher"]
        }
    }

As stated before, the ``user_ctx`` property is optional for admins,
while being mandatory for regular (non-admin) users. When the roles
property of ``user_ctx`` is missing, it defaults to the empty list
``[]``.

.. _selectorobj:

Selector Objects
================

Including a Selector Object in the replication document enables you to
use a query expression to determine if a document should be included in
the replication.

The selector specifies fields in the document, and provides an expression
to evaluate with the field content or other data. If the expression resolves
to ``true``, the document is replicated.

The selector object must:

-  Be structured as valid JSON.
-  Contain a valid query expression.

The syntax for a selector is the same as the
:ref:`selectorsyntax <find/selectors>` used for :ref:`_find <api/db/_find>`.

Using a selector is significantly more efficient than using a JavaScript
filter function, and is the recommended option if filtering on document
attributes only.

Specifying Usernames and Passwords
===================================

There are multiple ways to specify usernames and passwords for replication endpoints:

 - In an ``{"auth": {"basic": ...}}`` object:

    .. versionadded:: 3.2.0

    .. code-block:: javascript

        {
            "target": {
                "url": "http://someurl.com/mydb",
                "auth": {
                    "basic": {
                        "username": "$username",
                        "password": "$password"
                     }
                }
            },
            ...
        }

   This is the prefererred format as it allows including characters like ``@``, ``:``
   and others in the username and password fields.

 - In the userinfo part of the endpoint URL. This allows for a more compact
   endpoint represention however, it prevents using characters like ``@`` and ``:``
   in usernames or passwords:

    .. code-block:: javascript

        {
            "target":  "http://user:pass@localhost:5984/bar"
            ...
        }

   Specifying credentials in the userinfo part of the URL is deprecated as per
   `RFC3986 <https://datatracker.ietf.org/doc/html/rfc3986#section-3.2.1>`_.
   CouchDB still supports this way of specifying credentials and doesn't yet
   have a target release when support will be removed.

 - In an ``"Authorization: Basic $b64encoded_username_and_password"`` header:

    .. code-block:: javascript

        {
            "target": {
                "url": "http://someurl.com/mydb",
                    "headers": {
                        "Authorization": "Basic dXNlcjpwYXNz"
                    }
                },
            ...
        }

    This method has the downside of the going through the extra step of base64
    encoding. In addition, it could give the impression that it encrypts or
    hides the credentials so it could encourage invadvertent sharing and
    leaking credentials.

When credentials are provided in multiple forms, they are selected in the following order:

  - ``"auth": {"basic": {...}}`` object
  - URL userinfo
  - ``"Authorization: Basic ..."`` header.

First, the ``auth`` object is checked, and if credentials are defined there,
they are used. If they are not, then URL userinfo is checked. If credentials
are found there, then those credentials are used, otherwise basic auth header
is used.
