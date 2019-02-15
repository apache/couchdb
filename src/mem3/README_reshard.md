Developer Oriented Description of How Resharding Works
======================================================

This is a technical description of the resharding logic. The discussion will focus on a few aspect such as general job life cycle, supervision hierarchy, data definitions and job state transition mechanism. The main audience is CouchDB developers and assumes knowledge of Erlang/OTP supervision trees as well as CouchDB internals, specifically related to shards.


General Job Life cycle Description
---------------------------------

Jobs creation happens in the `mem3_reshard_httpd` API handler module. That module then uses `mem3_reshard_http_util` to do some validation, and eventually calls `mem3_reshard:start_split_job/2` on one or more nodes in the cluster.

`mem3_reshard:start_split_job/2` is the main Erlang API entry point.  After some more validation it creates a `#job{}` record and calls the `mem3_reshard` job manager. The manager then will add the job to its jobs ets table, save it to a _local document in the shards db, and most importantly, start a new resharding process. That process will be supervised by the `mem3_reshard_job_sup` supervisor.

A single shard splitting job will be running in a gen_server from the `mem3_reshard_job` module. That job will go through a series of steps such as `initial_copy`, `build_indices`, `update_shard_map`, .... So, it will copy the data from the source to the targets, then build indices, then update the shard map and so on. After each splitting step is complete, the job goes to the next step and immediately "checkpoints" where it reports back to `mem3_reshard` manager. The manager persists that split start a `_local/...` document in _dbs db and notifies the job to continue executing. Eventually, after the jobs successfully goes through all the steps, the job process exits, goes to the `completed` state and stays in the system for the user to inspect it and then remove it.

Data!Definitions
------------------------

Right below the `mem3_reshard:start_split_job/1` API level a job is converted to a `#job{}` record defined in the `mem3_reshard.hrl` internal header file. That record is then used throughout the resharding logic: the job manager `mem3_reshard` gen_server stores it in its jobs ets table as is. When a job process is started in `mem3_reshard_job` it gets passed a `#job{}` record as well. Then as the job process is running it will periodically report state back to the `mem3_reshard` manager again as an updated `#job{}` record instance.

A few interesting fields from the `#job{}` record:

 - `id` Uniquely identifies a job in a cluster. It is derived from the source shard name, node and a version (currently = 1).
 - `type` Currently the only type supported is `split` but `merge` or `rebalance` might be added in the future.
 - `job_state` The running state of the job. Indicates if the job is `running`, `stopped`, `completed` or `failed`.
 - `split_state` Once the job is running this indicates how far along it got in the splitting process.
 - `state_info` A proplist of extra info regarding the current state.
 - `source` Source shard file
 - `target` List of target shard files. This is expected to be a list of 2 times currently.
 - `history` A time-line of state transitions represented as a list of tuples.
 - `pid` When job is running this will be set to the pid of the process.


Because jobs persist across node restarts, this `#job{}` record has to be saved to disk. That happens in the `mem3_reshard_job_store` module. There a `#job{}` record is transformed to/from an ejson representation. Those transformation functions are dual-purpose and are also used to the HTTP API to return job's state and other information to the user.

Another piece of data is the global resharding state. Then a user disables resharding on a cluster, a call is made to `mem3_reshard` manager on each node and they store that in a `#state{}` record. This record is defined in the `mem3_reshard.hrl` module, and just like the `#job{}` record can be translated to/from ejson in the `mem3_reshard_store` and stored and loaded from disk.


State Transitions
-----------------

Resharding logic has 3 separate states to keep track of:

1. Global resharding state. This state can be toggled between `running` and `stopped`. That toggle happens via the `mem3_reshard:start/0` and `mem3_reshard:stop/1` function.  This state is kept in the `#state{}` record of the `mem3_reshard` manager gen_server. This state is also persisted to the local shard map database as a `_local/...` document so that it is maintained on a node reboot. When the state is `running` then all jobs that are not individually `stopped` and have not failed or completed will be running. When the state is `stopped` all the running jobs will be `stopped`.

2. Job's running state held in the `#job{}` `job_state` field. This is the general running state of a resharding job. It can be `new`, `running`, `stopped`, `completed` or `failed`. This state is most relevant for the `mem3_reshard` manager. In other words, it is the `mem3_reshard` gen_server that starts the job, monitors it to see if it exits successfully on completion or with an error and marks it `failed`.

3. Job's internal splitting state. This state track the steps taken during shard splitting by each individual job. This state is mostly relevant in `mem3_reshard_job` only and is mostly separate from the job's running state. For example the job's running state would be the same for another kind of job like say `merge` or `rebalance` but those jobs will likely have a very different set of internal states they progress through. This state is kept in `#job{}`'s `split_state` field. The progression of these states is linear basically going from one state to the next. That's reflected in the code as well, when one state is done, `mem3_reshard_job:next_state/1` is called which returns the next state in the list. The list is defined in `mem3_reshard.hrl` file in `SPLIT_STATES` macro. This simplistic transition is also one of the reasons why a gen_fsm wasn't considered for `mem3_reshard_job` logic.

Another interesting aspect is how`split_state` transitions happen in `mem3_reshard_job` so the next sub-section will examine that in more detail:

A job starts running in the `new` state or from a previously checkpointed state. In the later case, the job goes through a recovery logic (see `maybe_recover` in `mem3_reshard_job`) where it tries to resume its work where it left of. It means for example if it was doing the initial copy and was interrupted it might have to reset the target files and copy everything again. The after that the main state transition logic starts with the `switch_state(#job{}, StateName)` function. That's what triggers jumping to a new state.

In `mem3_reshard_job:switch_state/2` job's history is updated, any current `state_info` is cleared, job state is switch in the `#job{}` record and then `mem3_reshard` (the manager) is called to checkpoint this state. Interestingly enough, that's just a `gen_server:cast(...)` and after that `mem3_reshard_job` sits and waits. In the meantime `mem3_reshard` manager does the checkpointing, where it updates both its ets table with the new `#job{}` record and also persists the state with the `mem3_reshard_store` and finally it notifies the job process that checkpoint is done with the `mem3_reshard_job:checkpoint_done/1` call.

`mem3_reshard_job:checkpoint_done/1` function all sends a `do_state` cast to the job's gen_server which then proceeds to execute that state. Most splitting states, try not to block the main `mem3_reshard` gen_server and instead launch worker processes to do time consuming work. It is usually just one process but it could be multiple. After that it mostly waits for the workers to finish and inspects their exit signal. If they all exit and they are normal (see `mem_reshard:worker_exited/2`) it calls `switch_state/2` again with the next state and the whole process repeats itself until completion.


Supervision Hierarchy
---------------------

Resharding logic is mostly implemented in the `mem3_reshard*` modules. The supervision hierarchy looks something like this:

`mem3_sup` : Existing mem3 supervisor
    ...
    `mem3_reshard_sup` : Resharding supervisor
       `mem3_reshard_dbdoc` : gen_server responsible for updating the shard map
       `mem3_reshard_job_sup` : Supervisor responsible for resharding jobs
           `mem3_reshard_job` : gen_servers running the actual resharding logic
       `mem3_reshard` : Erlang API entry point for job life-cycle manager



Module Description And Various Other Notes
------------------------------------------

* `mem3_reshard`: Main API entry point and the job manager.

* `mem3_reshard_job` : Individual job logic.

* `mem3_reshard_dbdoc` : Responsible for updating shard doc in the `_db`'s database. Besides just having a bunch of utility function there is a gen_server spawned which is used to update shard documents in a cluster in such a way as to minimize the risk of conflicts. That is accomplished by having each shard updater calling only one such updater for the whole cluster. This coordinator is picked by sorting the list of all the live mem3 nodes and picking the first one in the list.

* `mem3_reshard_httpd` : API endpoint definitions.

* `mem3_reshard_httpd_util` : API endpoint utilities. This module is also responsible for sending requests to all the nodes in a cluster and gathering results.

* `mem3_reshard_index` : This is a helper module used by workers in the `build_indices` state.

* `mem3_reshard_job_sup` : Simple one for one supervisor which keeps track of running jobs.

* `mem3_reshard_store` : State persistence module. It knows how to save/restore `#job{}` and `#state{}` records to/from `_local/...` docs. It is also re-used for serializing `#job{}` into ejson by the HTTP API module.

* `mem3_reshard_validate` : Validate that source exists, target ranges don't have gaps in them, etc.

* `couch_db_split` : This module is not in `mem3` app but it does all the heavy lifting during the initial data copy. Given a source db and some targets, and a function to decide which doc go to which target, it will copy all data from the source to the targets. It's best to think of this module as a form of compactor. Unlike `couch_bt_engine_compactor` this one lives above the `couch_db_engine` API, and instead of copying data to one new file it copies it to 2 or more. Unsurprisingly because of that it uses some lower level `couch_db_engine` API directly, including linking to a couch_db_updater, force setting db update sequences and others.
