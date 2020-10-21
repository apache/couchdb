Developer Oriented Resharding Description
=========================================

This is a technical description of the resharding logic. The discussion will focus on: job creation and life-cycle, data definitions, and on the state transition mechanisms.


Job Life-Cycle
--------------

Job creation happens in the `mem3_reshard_httpd` API handler module. That module uses `mem3_reshard_http_util` to do some validation, and eventually calls `mem3_reshard:start_split_job/2` on one or more nodes in the cluster depending where the new jobs should run.

`mem3_reshard:start_split_job/2` is the main Erlang API entry point.  After some more validation it creates a `#job{}` record and calls the `mem3_reshard` job manager. The manager then will add the job to its jobs ets table, save it to a `_local` document in the shards db, and most importantly, start a new resharding process. That process will be supervised by the `mem3_reshard_job_sup` supervisor.

Each job will be running in a gen_server implemented in `mem3_reshard_job` module. When splitting a shard, a job will go through a series of steps such as `initial_copy`, `build_indices`, `update_shard_map`, etc. Between each step it will report progress and checkpoint with `mem3_reshard` manager. A checkpoint involved the `mem3_reshard` manager persisting that job's state to disk in `_local` document in `_dbs` db. Then job continues until `completed` state or until it failed in the `failed` state.

If a user stops shard splitting on the whole cluster, then all running jobs will stop. When shard splitting is resumed, they will try to recover from their last checkpoint.

A job can also be individually stopped or resumed. If a job is individually stopped it will stay so even if the global shard splitting state is `running`. A user has to explicitly set that job to a `running` state for it to resume. If a node with running jobs is turned off, when it is rebooted running jobs will resume from their last checkpoint.


Data Definitions
----------------

This section focuses on record definition and how data is transformed to and from various formats.

Right below the `mem3_reshard:start_split_job/1` API level a job is converted to a `#job{}` record defined in the `mem3_reshard.hrl` header file. That record is then used throughout most of the resharding code. The job manager `mem3_reshard` stores it in its jobs ets table, then when a job process is spawn it single argument also just a `#job{}` record. As a job process is executing it will periodically report state back to the `mem3_reshard` manager as an updated `#job{}` record.

Some interesting fields from the `#job{}` record:

 - `id` Uniquely identifies a job in a cluster. It is derived from the source shard name, node and a version (currently = 1).
 - `type` Currently the only type supported is `split` but `merge` or `rebalance` might be added in the future.
 - `job_state` The running state of the job. Indicates if the job is `running`, `stopped`, `completed` or `failed`.
 - `split_state` Once the job is running this indicates how far along it got in the splitting process.
 - `source` Source shard file. If/when merge is implemented this will be a list.
 - `target` List of target shard files. This is expected to be a list of 2 items currently.
 - `history` A time-line of state transitions represented as a list of tuples.
 - `pid` When job is running this will be set to the pid of the process.


In the `mem3_reshard_job_store` module the `#job{}` record is translated to an json document so it can be persisted to disk. Translation functions to and from a json in that module are also used by the HTTP API layer to return a job's state and other information to the user.

Another important piece of data is the global resharding state. When a user disables resharding on a cluster, a call is made to `mem3_reshard` manager on each node and they store that in a `#state{}` record. This record is defined in the `mem3_reshard.hrl` module, and just like the `#job{}` record can be translated to/from ejson in the `mem3_reshard_store` and stored and loaded from disk.


State Transitions
-----------------

Resharding logic has 3 separate states to keep track of:

1. Per-node resharding state. This state can be toggled between `running` and `stopped`. That toggle happens via the `mem3_reshard:start/0` and `mem3_reshard:stop/1` function.  This state is kept in the `#state{}` record of the `mem3_reshard` manager gen_server. This state is also persisted to the local shard map database as a `_local` document so that it is maintained through a node restart. When the state is `running` then all jobs that are not individually `stopped`, and have not failed or completed, will be `running`. When the state is `stopped` all the running jobs will be `stopped`.

2. Job's running state held in the `#job{}` `job_state` field. This is the general running state of a resharding job. It can be `new`, `running`, `stopped`, `completed` or `failed`. This state is most relevant for the `mem3_reshard` manager. In other words, it is the `mem3_reshard` gen_server that starts the job, monitors it to see if it exits successfully on completion or with an error.

3. Job's internal splitting state. This state tracks the steps taken during shard splitting by each job. This state is mostly relevant for the `mem3_reshard_job` module. This state is kept in `#job{}`'s `split_state` field. The progression of these states is linear going from one state to the next. That's reflected in the code as well, when one state is done, `mem3_reshard_job:get_next_state/1` is called which returns the next state in the list. The list is defined in the `SPLIT_STATES` macro. This simplistic transition is also one of the reasons why a gen_fsm wasn't considered for `mem3_reshard_job` logic.

Another interesting aspect is how the `split_state` transitions happen in the `mem3_reshard_job` module. What follows is an examination of that.

A job starts running in the `new` state or from a previously checkpointed state. In the later case, the job goes through some recovery logic (see `?STATE_RESTART` macro in `mem3_reshard_job`) where it tries to resume its work from where it left of. It means, for example, if it was in the `initial_copy` state and was interrupted it might have to reset the target files and copy everything again. After recovery, the state execution logic is driven by `run(#job{})` which ends up calling `?MODULE:State(#job{})` state specific functions for each state.

In `mem3_reshard_job:switch_to_next_state/2` job's history is updated, any current `state_info` is cleared, job state is switched in the `#job{}` record. Then, the new state is checkpointed in the `checkpoint/1` function. Checkpoint will cast a message to the `mem3_reshard` manager. After that message is sent the job process sits and waits.

In the meantime `mem3_reshard` manager checkpoints the state, which means it updates both its ETS table with the new `#job{}` record, persists the state with the `mem3_reshard_store` module, then, finally, it notifies the job process that checkpointing is done by calling `mem3_reshard_job:checkpoint_done/1`.

`mem3_reshard_job:checkpoint_done/1` function call sends a `checkpoint_done` message to the job's process, at which point it starts executing that state.

Most states in `mem3_reshard_job` try not to block the main job process and instead launch worker processes to perform long running operations. It is usually just one worker process but it could be multiple as well. After that it waits for the workers to finish and inspects their exit signal (see `wait_for_workers/1` function). When all the workers exit for a particular `split_state`, the job is switched to the next state with `switch_to_next_state/1` and the whole thing repeats until the `completed` state is reached when the whole job exits normally.

If the source is updated at high rate and the cluster is under load, there is a possibility for the resharding jobs to take longer to finish. The cluster would have to be running at the limit where both compaction and internal replication will have difficulty catching up as fundamentally the logic used for the initial bulk copy is similar the compaction code, and topoff states are just reusing the internal replicator code. Eventually when the load subsides the jobs should catch up and finish.

Individual Modules Description
------------------------------

These are mostly random notes about various modules involved in resharding. Most, but not all, are in the `mem3` application.

* `mem3_reshard`: Main API entry point and the job manager.

* `mem3_reshard_job` : Individual job logic.

* `mem3_reshard_dbdoc` : Responsible for updating shard doc in the `_db`'s database. Besides just having a bunch of utility function there is a gen_server spawned which is used to update shard documents in a cluster in such a way as to minimize the risk of conflicts. That is accomplished by having each shard updater calling only one such updater for the whole cluster. This coordinator is picked by sorting the list of all the live mem3 nodes and picking the first one in the list.

* `mem3_reshard_httpd` : API endpoint definitions.

* `mem3_reshard_api` : Cluster API endpoint. This module is responsible for sending requests to all the nodes in a cluster and gathering results.

* `mem3_reshard_index` : This is a helper module used by workers in the `build_indices` state.

* `mem3_reshard_job_sup` : Simple one for one supervisor which keeps track of running jobs.

* `mem3_reshard_store` : State persistence module. It knows how to save/restore `#job{}` and `#state{}` records to/from `_local` docs. It is also re-used for serializing `#job{}` into ejson by the HTTP API module.

* `mem3_reshard_validate` : Validate that source exists, target ranges don't have gaps in them, etc.

* `couch_db_split` : This module is not in `mem3` app but it does all the heavy lifting during the initial data copy. Given a source db and some targets, and a function to decide which doc go to which target, it will copy all data from the source to the targets. It's best to think of this module as a form of compactor. Unlike `couch_bt_engine_compactor` this one lives above the `couch_db_engine` API, and instead of copying data to one new file it copies it to 2 or more. Unsurprisingly because of that it uses some lower level `couch_db_engine` API directly, including linking to a couch_db_updater, force setting db update sequences and others.
