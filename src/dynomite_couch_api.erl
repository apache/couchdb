%% This is a Dynomite plugin for calling the CouchDB raw Erlang API
%%
%% Most calls will have come from any of the web endpoints to execute
%% these functions on the proper node for the key(s).

-module(dynomite_couch_api).
-author('brad@cloudant.com').

-export([create_db/1, delete_db/1, get/1, put/1,
         bulk_docs/1, missing_revs/1, get_db_info/1, get_view_group_info/1,
         ensure_full_commit/1
        ]).

-include("../../couch/src/couch_db.hrl").
-include("../include/common.hrl").


%%--------------------------------------------------------------------
%% @spec create_db([Part, DbName, Options]) -> {ok,Db} | {error,Error}
%% Description: Creates the database shard.
%%--------------------------------------------------------------------
create_db([Part, DbName, Options]) ->
    case couch_server:create(partitions:shard_name(Part, DbName), Options) of
        {ok, Shard} ->
            couch_db:close(Shard),
            ok;
        Error -> Error
    end.


%%--------------------------------------------------------------------
%% @spec delete_db([Part, DbName, Options]) -> {ok,deleted} | {error,Error}
%% Description: Deletes the database shard.
%%--------------------------------------------------------------------
delete_db([Part, DbName, Options]) ->
    couch_server:delete(partitions:shard_name(Part, DbName), Options).


get([Part, Db, DocId, Revs, Options]) ->
    case showroom_db:open_shard(node(), Part, Db) of
        {ok, Shard} ->
            {Status, Doc} = couch_api:open_doc(Shard, DocId, Revs, Options),
            showroom_db:close_shard(Shard),
            {Status, {[], [Doc]}};
        Error ->
            Error
    end.


put([Part, Db, Doc = #doc{clock=Clock}, Options]) ->
    case showroom_db:open_shard(node(), Part, Db) of
        {ok, Shard} ->
            {Status, NewRev} = couch_db:update_doc(Shard, Doc, Options),
            showroom_db:close_shard(Shard),
            {Status, {Clock, [NewRev]}};
        Error ->
            Error
    end.


bulk_docs([Part, SeqsDocs, Db, Options, Type]) ->
    {Seqs, Docs} = lists:unzip(SeqsDocs),
    case Docs of
    [] -> {ok, []};
    _ ->
        case showroom_db:open_shard(node(), Part, Db) of
        {ok, Shard} ->
            {ok, Results1} = couch_db:update_docs(Shard, Docs, Options, Type),
            showroom_db:close_shard(Shard),
            Results = int_zip(Seqs, Results1),
            {ok, Results};
        Error ->
            Error
        end
    end.


missing_revs([Part, SeqsIdsRevs, Db]) ->
    {_Seqs, IdsRevs} = lists:unzip(SeqsIdsRevs),
    case IdsRevs of
    [] -> {ok, []};
    _ ->
        case showroom_db:open_shard(node(), Part, Db) of
        {ok, Shard} ->
            {ok, Results1} = couch_db:get_missing_revs(Shard, IdsRevs),
            showroom_db:close_shard(Shard),
            {ok, Results1};
        Error ->
            Error
        end
    end.


get_db_info([Part, Db]) ->
    case showroom_db:open_shard(node(), Part, Db) of
        {ok, Shard} ->
            {Status, Info} = couch_db:get_db_info(Shard),
            showroom_db:close_shard(Shard),
            {Status, {[], Info}};
        Error ->
            Error
    end.

get_view_group_info([Part, Db, DesignId]) ->
    case showroom_db:open_shard(node(), Part, Db) of
    {ok, Shard} ->
        {ok, EmptyGroup} = showroom_view:build_skeleton_view_group(Db, DesignId),
        <<"S", ShardName/binary>> = Shard#db.name,
        {ok, Pid} = gen_server:call(couch_view, {get_group_server,
            ShardName, EmptyGroup}),
        {ok, Info} = couch_view_group:request_group_info(Pid),
        showroom_db:close_shard(Shard),
        {ok, {[], Info}};
    Error ->
        Error
    end.


ensure_full_commit([Part, Db]) ->
    case showroom_db:open_shard(node(), Part, Db) of
        {ok, Shard} ->
            {Status, Info} = couch_db:ensure_full_commit(Shard),
            showroom_db:close_shard(Shard),
            {Status, {[], Info}};
        Error ->
            Error
    end.


%% =======================
%%   internal
%% =======================

int_zip(Seqs, Docs) when length(Seqs) == length(Docs) ->
    lists:zip(Seqs, Docs);
int_zip(_Seqs, []) ->
    [];
int_zip(Seqs, Docs) ->
    ?debugFmt("~nWTF?  int_zip~nSeqs: ~p~nDocs: ~p~n", [Seqs, Docs]),
    [].
