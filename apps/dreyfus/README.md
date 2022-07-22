What is dreyfus?
-------------
Dreyfus manages Clouseau nodes to deliver full-text search features.

Dreyfus consists of the following files:

- **dreyfus.app.src** - application resource file. As can be seen from this file, a callback module for the application is dreyfus_app, and the two registered processes started in this application are: dreyfus_index_manager and dreyfus_sup.
- **dreyfus_app.erl** - a callback module for the application that starts the top supervisor by dreyfus_sup:start_link().
- **dreyfus_sup.erl** - the top supervisor that starts dreyfus_index_manager as its child worker process.
- **dreyfus_index_manager.erl** - manages multiple processes of dreyfus_index.
- **dreyfus_index.erl** - contains main callback functions to operate on index. One process is created for every index (a distinct index function in a design document).
- **dreyfus_index_updater.erl** - contains callback functions for index update.
- **dreyfus_httpd.erl** - handles http requests.
- **dreyfus_fabric.erl**, dreyfus_fabric_cleanup.erl, dreyfus_fabric_group1.erl, dreyfus_fabric_group2.erl, dreyfus_fabric_info.erl, dreyfus_fabric_search.erl - collection of proxy functions for operations in a cluster with shards.
- **dreyfus_rpc.erl** - proxy functions executed for every shard.
- **clouseau_rpc.erl** - contains remote procedure calls functions to Clouseau nodes.
- **dreyfus_bookmark.erl** - utility functions for managing bookmarks for retrieving the next set of results
- **dreyfus_util.erl** - various utility functions



Life of http request
-------------
Http requests have the following life cycle:

![Dreyfus](https://cloud.githubusercontent.com/assets/5738841/7590919/cbaf1c50-f898-11e4-8a4c-462a1a680135.png)

1. A request from chttpd goes to dreyfus_httpd.
2. dreyfus_httpd:
    - passes and validates the request in functions: `parse_index_params` & `validate_index_query`.
    - depending on the type of the request invokes one of the fabric_functions: dreyfus_fabric_search, dreyfus_fabric_group1, dreyfus_fabric_group2, dreyfus_fabric_info, or dreyfus_fabric_cleanup.
3. dreyfus_fabric:
	- Get shards and workers to be executed on every shard:
	`Shards = dreyfus_util:get_shards(DbName, QueryArgs)`,
    `Workers = fabric_util:submit_jobs(Shards, dreyfus_rpc, search,
                          [DDoc, IndexName, dreyfus_util:export(QueryArgs)])`
   - spawns processes to execute jobs on every shard using a RPC server rexi: `rexi_utils:recv(Workers, #shard.ref, fun handle_message/3, State, infinity, 1000 * 60 * 60)
`
4. dreyfus_rpc:
	- is executed on every shard of every node at the same time.
	- calls `dreyfus_index_manager:get_index(DbName, Index)` to get an index. dreyfus_index_manager will spawn a process of creating an index if the index doesn't exist.
	- an index of every shard will be updated if necessary with an instruction `dreyfus_index:await(Pid, MinSeq)`.
	- calls `dreyfus_index:Fun(Pid, QueryArgs)` with a corresponding search request.

5. dreyfus_index:
	- synchronously calls `clouseau_rpc:search`.
6. clouseau_rp:
	-  calls `ioq:call(Ref, Msg, erlang:get(io_priority))` to run search on clouseau nodes using Lucene.
7.	top_docs are returned from Lucene
8.	top_docs are passed to dreyfus_index
9.	top_docs are passed to dreyfus_rpc
10.	dreyfus_rpc processes pass their individual top_docs as a reply `rexi:reply(Result)`  to the initial dreyfus_fabric process that spawned them.
11.	dreyfus_fabric merges documents from all shards: `MergedTopDocs = merge_top_docs(TopDocs, Sortable, Limit, Sort)` and returns the results to dreyfus_httpd.
12.	dreyfus_httpd returns the formatted results to chttpd through send_json(..)


Indexing
-------------

### Indexing triggered by a search request
During a search request, before dreyfus_rpc calls dreyfus_index:search, dreyfus_rpc first initiates the updating of Lucene indexes. It does it in the following way:

![DreyfusIndexing.png](https://cloud.githubusercontent.com/assets/5738841/7590923/d12303fe-f898-11e4-833d-b1387b7048a6.png)

1. The last sequence number (signifying the number of the last change in the database) in calculated: `{_LastSeq, MinSeq} = calculate_seqs(Db, Stale)`. For the stale queries (queries that don't need to reflect recent changes in the database), MinSeq will be 0, meaning that they don't need to initiate update of the index, before returning query results. The meaning of 0 is 'wait until index is at least at update_seq 0' which is true even for empty indexes.

2. Function call  `dreyfus_index:design_doc_to_index(DDoc, IndexName)` returns a record representation of an index: `#index{
               analyzer=Analyzer,
               ddoc_id=Id,
               def=Def,
               def_lang=Language,
               name=IndexName,
               sig=Sig}`.  `Sig` here is a hashed version of an index function and an analyzer represented in a Javascript function in a design document. `Sig` is used to check if an index description is changed, and the index needs to be reconstructed.


3. Function call `dreyfus_index_manager:get_index(DbName, Index)` will return Pid of the corresponding to this index dreyfus_index process. dreyfus_index_manager stores all the dreyfus_index processes for all indexes in the storage: `ets:new(?BY_SIG, [set, private, named_table])`. If the dreyfus_index process of the given index exists in the ets ?BY_SIG, it will be returned. If it doesn't exist, a new dreyfus_index process will be spawned.  For this, dreyfus_index_manager in the `handle_call({get_index,..)` will return `{noreply, State};` to not block gen_server, and will transfer handling creation of a new index process to the spawned process - `spawn_link(fun() -> new_index(DbName, Index) end)`, remembering the Pid of the caller in the ets ?BY_SIG.  `new_index` will create a new index process, sending `open_ok` message to the dreyfus_index_manager gen_server. `handle_call({open_ok,..) ` will retrieve the Pid - `From` of the original caller, and send a reply to this caller, a message containing a Pid of the created index process - NewPid. Calling `add_to_ets(NewPid, DbName, Sig)` will update two ets ?BY_SIG and ?BY_Pid.

4. `dreyfus_index:await(Pid, MinSeq)` will initiate the update of the index, if the requested MinSeq is bigger than the current Seq stored in the index. It will do this by calling `dreyfus_index_updater:update(IndexPid, Index)`.  Dreyfus_index_updater will load all documents, modified since last seq stored in the drefus index, and for every document will call `clouseau_rpc:delete` to delete documents in Java Lucene Index, or `clouseau_rpc:update` to update an index in Java Lucene Index.
