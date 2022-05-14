% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License. You may obtain a copy of
% the License at
%
%   http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
% License for the specific language governing permissions and limitations under
% the License.

-module(couch_db_engine).

-include("couch_db.hrl").
-include("couch_db_int.hrl").

-type filepath() :: iolist().
-type docid() :: binary().
-type rev() :: {non_neg_integer(), binary()}.
-type revs() :: [rev()].
-type json() :: any().
-type uuid() :: binary().
-type purge_seq() :: non_neg_integer().

-type doc_pair() :: {
    #full_doc_info{} | not_found,
    #full_doc_info{} | not_found
}.

-type doc_pairs() :: [doc_pair()].

-type db_open_options() :: [
    create
].

-type delete_options() :: [
    {context, delete | compaction}
    | sync
].

-type purge_info() :: {purge_seq(), uuid(), docid(), revs()}.
-type epochs() :: [{Node :: atom(), UpdateSeq :: non_neg_integer()}].
-type size_info() :: [{Name :: atom(), Size :: non_neg_integer()}].
-type partition_info() :: [
    {partition, Partition :: binary()}
    | {doc_count, DocCount :: non_neg_integer()}
    | {doc_del_count, DocDelCount :: non_neg_integer()}
    | {sizes, size_info()}
].

-type write_stream_options() :: [
    {buffer_size, Size :: pos_integer()}
    | {encoding, atom()}
    | {compression_level, non_neg_integer()}
].

-type doc_fold_options() :: [
    {start_key, Key :: any()}
    | {end_key, Key :: any()}
    | {end_key_gt, Key :: any()}
    | {dir, fwd | rev}
    | include_reductions
    | include_deleted
].

-type changes_fold_options() :: [
    {dir, fwd | rev}
].

-type purge_fold_options() :: [
    {start_key, Key :: any()}
    | {end_key, Key :: any()}
    | {end_key_gt, Key :: any()}
    | {dir, fwd | rev}
].

-type db_handle() :: any().

-type doc_fold_fun() :: fun(
    (#full_doc_info{}, UserAcc :: any()) ->
        {ok, NewUserAcc :: any()}
        | {stop, NewUserAcc :: any()}
).

-type local_doc_fold_fun() :: fun(
    (#doc{}, UserAcc :: any()) ->
        {ok, NewUserAcc :: any()}
        | {stop, NewUserAcc :: any()}
).

-type changes_fold_fun() :: fun(
    (#doc_info{}, UserAcc :: any()) ->
        {ok, NewUserAcc :: any()}
        | {stop, NewUserAcc :: any()}
).

-type purge_fold_fun() :: fun(
    (purge_info(), UserAcc :: any()) ->
        {ok, NewUserAcc :: any()}
        | {stop, NewUserAcc :: any()}
).

% This is called by couch_server to determine which
% engine should be used for the given database. DbPath
% is calculated based on the DbName and the configured
% extension for a given engine. The first engine to
% return true is the engine that will be used for the
% database.
-callback exists(DbPath :: filepath()) -> boolean().

% This is called by couch_server to delete a database. It
% is called from inside the couch_server process which
% means that the storage engine does not have to guarantee
% its own consistency checks when executing in this
% context. Although since this is executed in the context
% of couch_server it should return relatively quickly.
-callback delete(
    RootDir :: filepath(),
    DbPath :: filepath(),
    DelOpts :: delete_options()
) ->
    ok | {error, Reason :: atom()}.

% This function can be called from multiple contexts. It
% will either be called just before a call to delete/3 above
% or when a compaction is cancelled which executes in the
% context of a couch_db_updater process. It is intended to
% remove any temporary files used during compaction that
% may be used to recover from a failed compaction swap.
-callback delete_compaction_files(
    RootDir :: filepath(),
    DbPath :: filepath(),
    DelOpts :: delete_options()
) ->
    ok.

% This is called from the couch_db_updater:init/1 context. As
% such this means that it is guaranteed to only have one process
% executing for a given DbPath argument (ie, opening a given
% database is guaranteed to only happen in a single process).
% However, multiple process may be trying to open different
% databases concurrently so if a database requires a shared
% resource that will require concurrency control at the storage
% engine layer.
%
% The returned DbHandle should be a term that can be freely
% copied between processes and accessed concurrently. However
% its guaranteed that the handle will only ever be mutated
% in a single threaded context (ie, within the couch_db_updater
% process).
-callback init(DbPath :: filepath(), db_open_options()) ->
    {ok, DbHandle :: db_handle()}.

% This is called in the context of couch_db_updater:terminate/2
% and as such has the same properties for init/2. It's guaranteed
% to be consistent for a given database but may be called by many
% databases concurrently.
-callback terminate(Reason :: any(), DbHandle :: db_handle()) -> Ignored :: any().

% This is called in the context of couch_db_updater:handle_call/3
% for any message that is unknown. It can be used to handle messages
% from asynchronous processes like the engine's compactor if it has one.
-callback handle_db_updater_call(Msg :: any(), DbHandle :: db_handle()) ->
    {reply, Resp :: any(), NewDbHandle :: db_handle()}
    | {stop, Reason :: any(), Resp :: any(), NewDbHandle :: db_handle()}.

% This is called in the context of couch_db_updater:handle_info/2
% and has the same properties as handle_call/3.
-callback handle_db_updater_info(Msg :: any(), DbHandle :: db_handle()) ->
    {noreply, NewDbHandle :: db_handle()}
    | {noreply, NewDbHandle :: db_handle(), Timeout :: timeout()}
    | {stop, Reason :: any(), NewDbHandle :: db_handle()}.

% These functions are called by any process opening or closing
% a database. As such they need to be able to handle being
% called concurrently. For example, the legacy engine uses these
% to add monitors to the main engine process.
-callback incref(DbHandle :: db_handle()) -> {ok, NewDbHandle :: db_handle()}.
-callback decref(DbHandle :: db_handle()) -> ok.
-callback monitored_by(DbHande :: db_handle()) -> [pid()].

% This is called in the context of couch_db_updater:handle_info/2
% and should return the timestamp of the last activity of
% the database. If a storage has no notion of activity or the
% value would be hard to report its ok to just return the
% result of os:timestamp/0 as this will just disable idle
% databases from automatically closing.
-callback last_activity(DbHandle :: db_handle()) -> erlang:timestamp().

% All of the get_* functions may be called from many
% processes concurrently.

% The database should make a note of the update sequence when it
% was last compacted. If the database doesn't need compacting it
% can just hard code a return value of 0.
-callback get_compacted_seq(DbHandle :: db_handle()) ->
    CompactedSeq :: non_neg_integer().

% The number of documents in the database which have all leaf
% revisions marked as deleted.
-callback get_del_doc_count(DbHandle :: db_handle()) ->
    DelDocCount :: non_neg_integer().

% This number is reported in the database info properties and
% as such can be any JSON value.
-callback get_disk_version(DbHandle :: db_handle()) -> Version :: json().

% The number of documents in the database that have one or more
% leaf revisions not marked as deleted.
-callback get_doc_count(DbHandle :: db_handle()) -> DocCount :: non_neg_integer().

% The epochs track which node owned the database starting at
% a given update sequence. Each time a database is opened it
% should look at the epochs. If the most recent entry is not
% for the current node it should add an entry that will be
% written the next time a write is performed. An entry is
% simply a {node(), CurrentUpdateSeq} tuple.
-callback get_epochs(DbHandle :: db_handle()) -> Epochs :: epochs().

% Get the current purge sequence known to the engine. This
% value should be updated during calls to purge_docs.
-callback get_purge_seq(DbHandle :: db_handle()) -> purge_seq().

% Get the oldest purge sequence known to the engine
-callback get_oldest_purge_seq(DbHandle :: db_handle()) -> purge_seq().

% Get the purged infos limit. This should just return the last
% value that was passed to set_purged_docs_limit/2.
-callback get_purge_infos_limit(DbHandle :: db_handle()) -> pos_integer().

% Get the revision limit. This should just return the last
% value that was passed to set_revs_limit/2.
-callback get_revs_limit(DbHandle :: db_handle()) -> RevsLimit :: pos_integer().

% Get the current security properties. This should just return
% the last value that was passed to set_security/2.
-callback get_security(DbHandle :: db_handle()) -> SecProps :: any().

% Get the current properties.
-callback get_props(DbHandle :: db_handle()) -> Props :: [any()].

% This information is displayed in the database info poperties. It
% should just be a list of {Name::atom(), Size::non_neg_integer()}
% tuples that will then be combined across shards. Currently,
% various modules expect there to at least be values for:
%
%   file     - Number of bytes on disk
%
%   active   - Theoretical minimum number of bytes to store this db on disk
%              which is used to guide decisions on compaction
%
%   external - Number of bytes that would be required to represent the
%              contents outside of the database (for capacity and backup
%              planning)
-callback get_size_info(DbHandle :: db_handle()) -> SizeInfo :: size_info().

% This returns the information for the given partition.
% It should just be a list of {Name::atom(), Size::non_neg_integer()}
% It returns the partition name, doc count, deleted doc count and two sizes:
%
%   active   - Theoretical minimum number of bytes to store this partition on disk
%
%   external - Number of bytes that would be required to represent the
%              contents of this partition outside of the database
-callback get_partition_info(DbHandle :: db_handle(), Partition :: binary()) ->
    partition_info().

% The current update sequence of the database. The update
% sequence should be incrememnted for every revision added to
% the database.
-callback get_update_seq(DbHandle :: db_handle()) -> UpdateSeq :: non_neg_integer().

% Whenever a database is created it should generate a
% persistent UUID for identification in case the shard should
% ever need to be moved between nodes in a cluster.
-callback get_uuid(DbHandle :: db_handle()) -> UUID :: binary().

% These functions are only called by couch_db_updater and
% as such are guaranteed to be single threaded calls. The
% database should simply store these values somewhere so
% they can be returned by the corresponding get_* calls.

-callback set_revs_limit(DbHandle :: db_handle(), RevsLimit :: pos_integer()) ->
    {ok, NewDbHandle :: db_handle()}.

-callback set_purge_infos_limit(DbHandle :: db_handle(), Limit :: pos_integer()) ->
    {ok, NewDbHandle :: db_handle()}.

-callback set_security(DbHandle :: db_handle(), SecProps :: any()) ->
    {ok, NewDbHandle :: db_handle()}.

% This function is only called by couch_db_updater and
% as such is guaranteed to be single threaded calls. The
% database should simply store provided property list
% unaltered.

-callback set_props(DbHandle :: db_handle(), Props :: any()) ->
    {ok, NewDbHandle :: db_handle()}.

% Set the current update sequence of the database. The intention is to use this
% when copying a database such that the destination update sequence should
% match exactly the source update sequence.
-callback set_update_seq(
    DbHandle :: db_handle(),
    UpdateSeq :: non_neg_integer()
) ->
    {ok, NewDbHandle :: db_handle()}.

% This function will be called by many processes concurrently.
% It should return a #full_doc_info{} record or not_found for
% every provided DocId in the order those DocId's appear in
% the input.
%
% Traditionally this function will only return documents that
% were present in the database when the DbHandle was retrieved
% from couch_server. It is currently unknown what would break
% if a storage engine deviated from that property.
-callback open_docs(DbHandle :: db_handle(), DocIds :: [docid()]) ->
    [#full_doc_info{} | not_found].

% This function will be called by many processes concurrently.
% It should return a #doc{} record or not_found for every
% provided DocId in the order they appear in the input.
%
% The same caveats around database snapshots from open_docs
% apply to this function (although this function is called
% rather less frequently so it may not be as big of an
% issue).
-callback open_local_docs(DbHandle :: db_handle(), DocIds :: [docid()]) ->
    [#doc{} | not_found].

% This function will be called from many contexts concurrently.
% The provided RawDoc is a #doc{} record that has its body
% value set to the body value returned from write_doc_body/2.
%
% This API exists so that storage engines can store document
% bodies externally from the #full_doc_info{} record (which
% is the traditional approach and is recommended).
-callback read_doc_body(DbHandle :: db_handle(), RawDoc :: doc()) ->
    doc().

% This function will be called from many contexts concurrently.
% If the storage engine has a purge_info() record for any of the
% provided UUIDs, those purge_info() records should be returned. The
% resulting list should have the same length as the input list of
% UUIDs.
-callback load_purge_infos(DbHandle :: db_handle(), [uuid()]) ->
    [purge_info() | not_found].

% This function is called concurrently by any client process
% that is writing a document. It should accept a #doc{}
% record and return a #doc{} record with a mutated body it
% wishes to have written to disk by write_doc_body/2.
%
% This API exists so that storage engines can compress
% document bodies in parallel by client processes rather
% than forcing all compression to occur single threaded
% in the context of the couch_db_updater process.
-callback serialize_doc(DbHandle :: db_handle(), Doc :: doc()) ->
    doc().

% This function is called in the context of a couch_db_updater
% which means its single threaded for the given DbHandle.
%
% The returned #doc{} record should have its Body set to a value
% that will be stored in the #full_doc_info{} record's revision
% tree leaves which is passed to read_doc_body/2 above when
% a client wishes to read a document.
%
% The BytesWritten return value is used to determine the number
% of active bytes in the database which can is used to make
% a determination of when to compact this database.
-callback write_doc_body(DbHandle :: db_handle(), Doc :: doc()) ->
    {ok, FlushedDoc :: doc(), BytesWritten :: non_neg_integer()}.

% This function is called from the context of couch_db_updater
% and as such is guaranteed single threaded for the given
% DbHandle.
%
% This is probably the most complicated function in the entire
% API due to a few subtle behavior requirements required by
% CouchDB's storage model.
%
% The Pairs argument is a list of pairs (2-tuples) of
% #full_doc_info{} records. The first element of the pair is
% the #full_doc_info{} that exists on disk. The second element
% is the new version that should be written to disk. There are
% two basic cases that should be followed:
%
%     1. {not_found, #full_doc_info{}} - A new document was created
%     2. {#full_doc_info{}, #full_doc_info{}} - A document was updated
%
% The cases are fairly straight forward as long as proper
% accounting for moving entries in the update sequence are accounted
% for.
%
% The LocalDocs variable is applied separately. Its important to
% note for new storage engine authors that these documents are
% separate because they should *not* be included as part of the
% changes index for the database.
%
% Traditionally an invocation of write_doc_infos should be all
% or nothing in so much that if an error occurs (or the VM dies)
% then the database doesn't retain any of the changes. However
% as long as a storage engine maintains consistency this should
% not be an issue as it has never been a guarantee and the
% batches are non-deterministic (from the point of view of the
% client).
-callback write_doc_infos(
    DbHandle :: db_handle(),
    Pairs :: doc_pairs(),
    LocalDocs :: [#doc{}]
) ->
    {ok, NewDbHandle :: db_handle()}.

% This function is called from the context of couch_db_updater
% and as such is guaranteed single threaded for the given
% DbHandle.
%
% Each doc_pair() is a 2-tuple of #full_doc_info{} records. The
% first element of the pair is the #full_doc_info{} that exists
% on disk. The second element is the new version that should be
% written to disk. There are three basic cases that should be considered:
%
%     1. {#full_doc_info{}, #full_doc_info{}} - A document was partially purged
%     2. {#full_doc_info{}, not_found} - A document was completely purged
%     3. {not_found, not_found} - A no-op purge
%
% In case 1, non-tail-append engines may have to remove revisions
% specifically rather than rely on compaction to remove them. Also
% note that the new #full_doc_info{} will have a different update_seq
% that will need to be reflected in the changes feed.
%
% In case 2 you'll notice is "purged completely" which
% means it needs to be removed from the database including the
% update sequence.
%
% In case 3 we just need to store the purge_info() to know that it
% was processed even though it produced no changes to the database.
%
% The purge_info() tuples contain the purge_seq, uuid, docid and
% revisions that were requested to be purged. This should be persisted
% in such a way that we can efficiently load purge_info() by its UUID
% as well as iterate over purge_info() entries in order of their PurgeSeq.
-callback purge_docs(DbHandle :: db_handle(), [doc_pair()], [purge_info()]) ->
    {ok, NewDbHandle :: db_handle()}.

% This function should be called from a single threaded context and
% should be used to copy purge infos from on database to another
% when copying a database
-callback copy_purge_infos(DbHandle :: db_handle(), [purge_info()]) ->
    {ok, NewDbHandle :: db_handle()}.

% This function is called in the context of couch_db_udpater and
% as such is single threaded for any given DbHandle.
%
% This call is made periodically to ensure that the database has
% stored all updates on stable storage. (ie, here is where you fsync).
-callback commit_data(DbHandle :: db_handle()) ->
    {ok, NewDbHande :: db_handle()}.

% This function is called by multiple processes concurrently.
%
% This function along with open_read_stream are part of the
% attachments API. For the time being I'm leaving these mostly
% undocumented. There are implementations of this in both the
% legacy btree engine as well as the alternative engine
% implementations for the curious, however this is a part of the
% API for which I'd like feed back.
%
% Currently an engine can elect to not implement these API's
% by throwing the atom not_supported.
-callback open_write_stream(
    DbHandle :: db_handle(),
    Options :: write_stream_options()
) ->
    {ok, pid()}.

% See the documentation for open_write_stream
-callback open_read_stream(DbHandle :: db_handle(), StreamDiskInfo :: any()) ->
    {ok, {Module :: atom(), ReadStreamState :: any()}}.

% See the documentation for open_write_stream
-callback is_active_stream(DbHandle :: db_handle(), ReadStreamState :: any()) ->
    boolean().

% This funciton is called by many processes concurrently.
%
% This function is called to fold over the documents in
% the database sorted by the raw byte collation order of
% the document id. For each document id, the supplied user
% function should be invoked with the first argument set
% to the #full_doc_info{} record and the second argument
% set to the current user supplied accumulator. The return
% value of the user function is a 2-tuple of {Go, NewUserAcc}.
% The NewUserAcc value should then replace the current
% user accumulator. If Go is the atom ok, iteration over
% documents should continue. If Go is the atom stop, then
% iteration should halt and the return value should be
% {ok, NewUserAcc}.
%
% Possible options to this function include:
%
%     1. start_key - Start iteration at the provided key or
%        or just after if the key doesn't exist
%     2. end_key - Stop iteration just after the provided key
%     3. end_key_gt - Stop iteration prior to visiting the provided
%        key
%     4. dir - The atom fwd or rev. This is to be able to iterate
%        over documents in reverse order. The logic for comparing
%        start_key, end_key, and end_key_gt are then reversed (ie,
%        when rev, start_key should be greater than end_key if the
%        user wishes to see results)
%     5. include_reductions - This is a hack for _all_docs since
%        it currently relies on reductions to count an offset. This
%        is a terrible hack that will need to be addressed by the
%        API in the future. If this option is present the supplied
%        user function expects three arguments, where the first
%        argument is a #full_doc_info{} record, the second argument
%        is the current list of reductions to the left of the current
%        document, and the third argument is the current user
%        accumulator. The return value from the user function is
%        unaffected. However the final return value of the function
%        should include the final total reductions as the second
%        element of a 3-tuple. Like I said, this is a hack.
%     6. include_deleted - By default deleted documents are not
%        included in fold_docs calls. However in some special
%        cases we do want to see them (as of now, just in couch_changes
%        during the design document changes optimization)
%
% Historically, if a process calls this function repeatedly it
% would see the same results returned even if there were concurrent
% updates happening. However there doesn't seem to be any instance of
% that actually happening so a storage engine that includes new results
% between invocations shouldn't have any issues.
-callback fold_docs(
    DbHandle :: db_handle(),
    UserFold :: doc_fold_fun(),
    UserAcc :: any(),
    doc_fold_options()
) ->
    {ok, LastUserAcc :: any()}.

% This function may be called by many processes concurrently.
%
% This should behave exactly the same as fold_docs/4 except that it
% should only return local documents and the first argument to the
% user function is a #doc{} record, not a #full_doc_info{}.
-callback fold_local_docs(
    DbHandle :: db_handle(),
    UserFold :: local_doc_fold_fun(),
    UserAcc :: any(),
    doc_fold_options()
) ->
    {ok, LastUserAcc :: any()}.

% This function may be called by many processes concurrently.
%
% This function is called to fold over the documents (not local
% documents) in order of their most recent update. Each document
% in the database should have exactly one entry in this sequence.
% If a document is updated during a call to this function it should
% not be included twice as that will probably lead to Very Bad Things.
%
% This should behave similarly to fold_docs/4 in that the supplied
% user function should be invoked with a #full_doc_info{} record
% as the first argument and the current user accumulator as the
% second argument. The same semantics for the return value from the
% user function should be handled as in fold_docs/4.
%
% The StartSeq parameter indicates where the fold should start
% *after*. As in, if a change with a value of StartSeq exists in the
% database it should not be included in the fold.
%
% The only option currently supported by the API is the `dir`
% option that should behave the same as for fold_docs.
-callback fold_changes(
    DbHandle :: db_handle(),
    StartSeq :: non_neg_integer(),
    UserFold :: changes_fold_fun(),
    UserAcc :: any(),
    changes_fold_options()
) ->
    {ok, LastUserAcc :: any()}.

% This function may be called by many processes concurrently.
%
% This function is called to fold over purged requests in order of
% their oldest purge (increasing purge_seq order)
%
% The StartPurgeSeq parameter indicates where the fold should start *after*.
-callback fold_purge_infos(
    DbHandle :: db_handle(),
    StartPurgeSeq :: purge_seq(),
    UserFold :: purge_fold_fun(),
    UserAcc :: any(),
    purge_fold_options()
) ->
    {ok, LastUserAcc :: any()}.

% This function may be called by many processes concurrently.
%
% This function is called to count the number of documents changed
% since the given UpdateSeq (ie, not including the possible change
% at exactly UpdateSeq). It is currently only used internally to
% provide a status update in a replication's _active_tasks entry
% to indicate how many documents are left to be processed.
%
% This is a fairly difficult thing to support in engine's that don't
% behave exactly like a tree with efficient support for counting rows
% between keys. As such returning 0 or even just the difference between
% the current update sequence is possibly the best some storage engines
% can provide. This may lead to some confusion when interpreting the
% _active_tasks entry if the storage engine isn't accounted for by the
% client.
-callback count_changes_since(
    DbHandle :: db_handle(),
    UpdateSeq :: non_neg_integer()
) ->
    TotalChanges :: non_neg_integer().

% This function is called in the context of couch_db_updater and as
% such is guaranteed to be single threaded for the given DbHandle.
%
% If a storage engine requires compaction this is a trigger to start
% it off. However a storage engine can do whatever it wants here. As
% this is fairly engine specific there's not a lot guidance that is
% generally applicable.
%
% When compaction is finished the compactor should use
% gen_server:cast/2 to send a {compact_done, CompactEngine, CompactInfo}
% message to the Parent pid provided. Currently CompactEngine
% must be the same engine that started the compaction and CompactInfo
% is an arbitrary term that's passed to finish_compaction/4.
-callback start_compaction(
    DbHandle :: db_handle(),
    DbName :: binary(),
    Options :: db_open_options(),
    Parent :: pid()
) ->
    {ok, NewDbHandle :: db_handle(), CompactorPid :: pid()}.

% This function is called in the context of couch_db_udpater and as
% such is guarnateed to be single threaded for the given DbHandle.
%
% Same as for start_compaction, this will be extremely specific to
% any given storage engine.
%
% The split in the API here is so that if the storage engine needs
% to update the DbHandle state of the couch_db_updater it can as
% finish_compaction/4 is called in the context of the couch_db_updater.
-callback finish_compaction(
    OldDbHandle :: db_handle(),
    DbName :: binary(),
    Options :: db_open_options(),
    CompactInfo :: any()
) ->
    {ok, CompactedDbHandle :: db_handle(), CompactorPid :: pid() | undefined}.

-export([
    exists/2,
    delete/4,
    delete_compaction_files/4,
    is_compacting/2,

    init/3,
    terminate/2,
    handle_db_updater_call/3,
    handle_db_updater_info/2,

    incref/1,
    decref/1,
    monitored_by/1,

    last_activity/1,

    get_engine/1,
    get_compacted_seq/1,
    get_del_doc_count/1,
    get_disk_version/1,
    get_doc_count/1,
    get_epochs/1,
    get_purge_seq/1,
    get_oldest_purge_seq/1,
    get_purge_infos_limit/1,
    get_revs_limit/1,
    get_security/1,
    get_props/1,
    get_size_info/1,
    get_partition_info/2,
    get_update_seq/1,
    get_uuid/1,

    set_revs_limit/2,
    set_security/2,
    set_purge_infos_limit/2,
    set_props/2,

    set_update_seq/2,

    open_docs/2,
    open_local_docs/2,
    read_doc_body/2,
    load_purge_infos/2,

    raft_lookup/2,
    raft_insert/2,
    raft_discard/2,
    raft_last/1,

    serialize_doc/2,
    write_doc_body/2,
    write_doc_infos/3,
    purge_docs/3,
    copy_purge_infos/2,
    commit_data/1,

    open_write_stream/2,
    open_read_stream/2,
    is_active_stream/2,

    fold_docs/4,
    fold_local_docs/4,
    fold_changes/5,
    fold_purge_infos/5,
    count_changes_since/2,

    start_compaction/1,
    finish_compaction/2,
    trigger_on_compact/1
]).

exists(Engine, DbPath) ->
    Engine:exists(DbPath).

delete(Engine, RootDir, DbPath, DelOpts) when is_list(DelOpts) ->
    Engine:delete(RootDir, DbPath, DelOpts).

delete_compaction_files(Engine, RootDir, DbPath, DelOpts) when
    is_list(DelOpts)
->
    Engine:delete_compaction_files(RootDir, DbPath, DelOpts).

is_compacting(Engine, DbName) ->
    Engine:is_compacting(DbName).

init(Engine, DbPath, Options) ->
    case Engine:init(DbPath, Options) of
        {ok, EngineState} ->
            {ok, {Engine, EngineState}};
        Error ->
            throw(Error)
    end.

terminate(Reason, #db{} = Db) ->
    #db{engine = {Engine, EngineState}} = Db,
    Engine:terminate(Reason, EngineState).

handle_db_updater_call(Msg, _From, #db{} = Db) ->
    #db{
        engine = {Engine, EngineState}
    } = Db,
    case Engine:handle_db_updater_call(Msg, EngineState) of
        {reply, Resp, NewState} ->
            {reply, Resp, Db#db{engine = {Engine, NewState}}};
        {stop, Reason, Resp, NewState} ->
            {stop, Reason, Resp, Db#db{engine = {Engine, NewState}}}
    end.

handle_db_updater_info(Msg, #db{} = Db) ->
    #db{
        name = Name,
        engine = {Engine, EngineState}
    } = Db,
    case Engine:handle_db_updater_info(Msg, EngineState) of
        {noreply, NewState} ->
            {noreply, Db#db{engine = {Engine, NewState}}};
        {noreply, NewState, Timeout} ->
            {noreply, Db#db{engine = {Engine, NewState}}, Timeout};
        {stop, Reason, NewState} ->
            couch_log:error("DB ~s shutting down: ~p", [Name, Msg]),
            {stop, Reason, Db#db{engine = {Engine, NewState}}}
    end.

incref(#db{} = Db) ->
    #db{engine = {Engine, EngineState}} = Db,
    {ok, NewState} = Engine:incref(EngineState),
    {ok, Db#db{engine = {Engine, NewState}}}.

decref(#db{} = Db) ->
    #db{engine = {Engine, EngineState}} = Db,
    Engine:decref(EngineState).

monitored_by(#db{} = Db) ->
    #db{engine = {Engine, EngineState}} = Db,
    Engine:monitored_by(EngineState).

last_activity(#db{} = Db) ->
    #db{engine = {Engine, EngineState}} = Db,
    Engine:last_activity(EngineState).

get_engine(#db{} = Db) ->
    #db{engine = {Engine, _}} = Db,
    Engine.

get_compacted_seq(#db{} = Db) ->
    #db{engine = {Engine, EngineState}} = Db,
    Engine:get_compacted_seq(EngineState).

get_del_doc_count(#db{} = Db) ->
    #db{engine = {Engine, EngineState}} = Db,
    Engine:get_del_doc_count(EngineState).

get_disk_version(#db{} = Db) ->
    #db{engine = {Engine, EngineState}} = Db,
    Engine:get_disk_version(EngineState).

get_doc_count(#db{} = Db) ->
    #db{engine = {Engine, EngineState}} = Db,
    Engine:get_doc_count(EngineState).

get_epochs(#db{} = Db) ->
    #db{engine = {Engine, EngineState}} = Db,
    Engine:get_epochs(EngineState).

get_purge_seq(#db{} = Db) ->
    #db{engine = {Engine, EngineState}} = Db,
    Engine:get_purge_seq(EngineState).

get_oldest_purge_seq(#db{} = Db) ->
    #db{engine = {Engine, EngineState}} = Db,
    Engine:get_oldest_purge_seq(EngineState).

get_purge_infos_limit(#db{} = Db) ->
    #db{engine = {Engine, EngineState}} = Db,
    Engine:get_purge_infos_limit(EngineState).

get_revs_limit(#db{} = Db) ->
    #db{engine = {Engine, EngineState}} = Db,
    Engine:get_revs_limit(EngineState).

get_security(#db{} = Db) ->
    #db{engine = {Engine, EngineState}} = Db,
    Engine:get_security(EngineState).

get_props(#db{} = Db) ->
    #db{engine = {Engine, EngineState}} = Db,
    Engine:get_props(EngineState).

get_size_info(#db{} = Db) ->
    #db{engine = {Engine, EngineState}} = Db,
    Engine:get_size_info(EngineState).

get_partition_info(#db{} = Db, Partition) ->
    #db{engine = {Engine, EngineState}} = Db,
    Engine:get_partition_info(EngineState, Partition).

get_update_seq(#db{} = Db) ->
    #db{engine = {Engine, EngineState}} = Db,
    Engine:get_update_seq(EngineState).

get_uuid(#db{} = Db) ->
    #db{engine = {Engine, EngineState}} = Db,
    Engine:get_uuid(EngineState).

set_revs_limit(#db{} = Db, RevsLimit) ->
    #db{engine = {Engine, EngineState}} = Db,
    {ok, NewSt} = Engine:set_revs_limit(EngineState, RevsLimit),
    {ok, Db#db{engine = {Engine, NewSt}}}.

set_purge_infos_limit(#db{} = Db, PurgedDocsLimit) ->
    #db{engine = {Engine, EngineState}} = Db,
    {ok, NewSt} = Engine:set_purge_infos_limit(EngineState, PurgedDocsLimit),
    {ok, Db#db{engine = {Engine, NewSt}}}.

set_security(#db{} = Db, SecProps) ->
    #db{engine = {Engine, EngineState}} = Db,
    {ok, NewSt} = Engine:set_security(EngineState, SecProps),
    {ok, Db#db{engine = {Engine, NewSt}}}.

set_props(#db{} = Db, Props) ->
    #db{engine = {Engine, EngineState}} = Db,
    {ok, NewSt} = Engine:set_props(EngineState, Props),
    {ok, Db#db{engine = {Engine, NewSt}}}.

set_update_seq(#db{} = Db, UpdateSeq) ->
    #db{engine = {Engine, EngineState}} = Db,
    {ok, NewSt} = Engine:set_update_seq(EngineState, UpdateSeq),
    {ok, Db#db{engine = {Engine, NewSt}}}.

open_docs(#db{} = Db, DocIds) ->
    #db{engine = {Engine, EngineState}} = Db,
    Engine:open_docs(EngineState, DocIds).

open_local_docs(#db{} = Db, DocIds) ->
    #db{engine = {Engine, EngineState}} = Db,
    Engine:open_local_docs(EngineState, DocIds).

read_doc_body(#db{} = Db, RawDoc) ->
    #db{engine = {Engine, EngineState}} = Db,
    Engine:read_doc_body(EngineState, RawDoc).

load_purge_infos(#db{} = Db, UUIDs) ->
    #db{engine = {Engine, EngineState}} = Db,
    Engine:load_purge_infos(EngineState, UUIDs).

serialize_doc(#db{} = Db, #doc{} = Doc) ->
    #db{engine = {Engine, EngineState}} = Db,
    Engine:serialize_doc(EngineState, Doc).

write_doc_body(#db{} = Db, #doc{} = Doc) ->
    #db{engine = {Engine, EngineState}} = Db,
    Engine:write_doc_body(EngineState, Doc).

write_doc_infos(#db{} = Db, DocUpdates, LocalDocs) ->
    #db{engine = {Engine, EngineState}} = Db,
    {ok, NewSt} = Engine:write_doc_infos(EngineState, DocUpdates, LocalDocs),
    {ok, Db#db{engine = {Engine, NewSt}}}.

purge_docs(#db{} = Db, DocUpdates, Purges) ->
    #db{engine = {Engine, EngineState}} = Db,
    {ok, NewSt} = Engine:purge_docs(
        EngineState, DocUpdates, Purges
    ),
    {ok, Db#db{engine = {Engine, NewSt}}}.

copy_purge_infos(#db{} = Db, Purges) ->
    #db{engine = {Engine, EngineState}} = Db,
    {ok, NewSt} = Engine:copy_purge_infos(
        EngineState, Purges
    ),
    {ok, Db#db{engine = {Engine, NewSt}}}.

raft_insert(#db{} = Db, Entries) ->
    #db{engine = {Engine, EngineState}} = Db,
    {ok, NewSt} = Engine:raft_insert(
        EngineState, Entries
    ),
    {ok, Db#db{engine = {Engine, NewSt}}}.

raft_lookup(#db{} = Db, Indexes) ->
    #db{engine = {Engine, EngineState}} = Db,
    Engine:raft_lookup(EngineState, Indexes).

raft_discard(#db{} = Db, UpTo) ->
    #db{engine = {Engine, EngineState}} = Db,
    {ok, NewSt} = Engine:raft_discard(
        EngineState, UpTo
    ),
    {ok, Db#db{engine = {Engine, NewSt}}}.

raft_last(#db{} = Db) ->
    #db{engine = {Engine, EngineState}} = Db,
    Engine:raft_last(EngineState).

commit_data(#db{} = Db) ->
    #db{engine = {Engine, EngineState}} = Db,
    {ok, NewSt} = Engine:commit_data(EngineState),
    {ok, Db#db{engine = {Engine, NewSt}}}.

open_write_stream(#db{} = Db, Options) ->
    #db{engine = {Engine, EngineState}} = Db,
    Engine:open_write_stream(EngineState, Options).

open_read_stream(#db{} = Db, StreamDiskInfo) ->
    #db{engine = {Engine, EngineState}} = Db,
    Engine:open_read_stream(EngineState, StreamDiskInfo).

is_active_stream(#db{} = Db, ReadStreamState) ->
    #db{engine = {Engine, EngineState}} = Db,
    Engine:is_active_stream(EngineState, ReadStreamState).

fold_docs(#db{} = Db, UserFun, UserAcc, Options) ->
    #db{engine = {Engine, EngineState}} = Db,
    Engine:fold_docs(EngineState, UserFun, UserAcc, Options).

fold_local_docs(#db{} = Db, UserFun, UserAcc, Options) ->
    #db{engine = {Engine, EngineState}} = Db,
    Engine:fold_local_docs(EngineState, UserFun, UserAcc, Options).

fold_changes(#db{} = Db, StartSeq, UserFun, UserAcc, Options) ->
    #db{engine = {Engine, EngineState}} = Db,
    Engine:fold_changes(EngineState, StartSeq, UserFun, UserAcc, Options).

fold_purge_infos(#db{} = Db, StartPurgeSeq, UserFun, UserAcc, Options) ->
    #db{engine = {Engine, EngineState}} = Db,
    Engine:fold_purge_infos(
        EngineState, StartPurgeSeq, UserFun, UserAcc, Options
    ).

count_changes_since(#db{} = Db, StartSeq) ->
    #db{engine = {Engine, EngineState}} = Db,
    Engine:count_changes_since(EngineState, StartSeq).

start_compaction(#db{} = Db) ->
    #db{
        engine = {Engine, EngineState},
        name = DbName,
        options = Options
    } = Db,
    {ok, NewEngineState, Pid} = Engine:start_compaction(
        EngineState, DbName, Options, self()
    ),
    {ok, Db#db{
        engine = {Engine, NewEngineState},
        compactor_pid = Pid
    }}.

finish_compaction(Db, CompactInfo) ->
    #db{
        engine = {Engine, St},
        name = DbName,
        options = Options
    } = Db,
    NewDb =
        case Engine:finish_compaction(St, DbName, Options, CompactInfo) of
            {ok, NewState, undefined} ->
                couch_event:notify(DbName, compacted),
                Db#db{
                    engine = {Engine, NewState},
                    compactor_pid = nil
                };
            {ok, NewState, CompactorPid} when is_pid(CompactorPid) ->
                Db#db{
                    engine = {Engine, NewState},
                    compactor_pid = CompactorPid
                }
        end,
    ok = couch_server:db_updated(NewDb),
    {ok, NewDb}.

trigger_on_compact(DbName) ->
    {ok, DDocs} = get_ddocs(DbName),
    couch_db_plugin:on_compact(DbName, DDocs).

get_ddocs(<<"shards/", _/binary>> = DbName) ->
    {_, Ref} = spawn_monitor(fun() ->
        exit(fabric:design_docs(mem3:dbname(DbName)))
    end),
    receive
        {'DOWN', Ref, _, _, {ok, JsonDDocs}} ->
            {ok,
                lists:map(
                    fun(JsonDDoc) ->
                        couch_doc:from_json_obj(JsonDDoc)
                    end,
                    JsonDDocs
                )};
        {'DOWN', Ref, _, _, Else} ->
            Else
    end;
get_ddocs(DbName) ->
    couch_util:with_db(DbName, fun(Db) ->
        FoldFun = fun(FDI, Acc) ->
            {ok, Doc} = couch_db:open_doc_int(Db, FDI, []),
            {ok, [Doc | Acc]}
        end,
        {ok, Docs} = couch_db:fold_design_docs(Db, FoldFun, [], []),
        {ok, lists:reverse(Docs)}
    end).
