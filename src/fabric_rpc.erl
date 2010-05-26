-module(fabric_rpc).

-export([open_doc/4, get_doc_info/1]).
-export([receive_loop/3]).

-include("../../dynomite/include/membership.hrl").


open_doc(DbName, DocId, Revs, Options) ->
    case couch_db:open(DbName, []) of
    {ok, Db} ->
        try
            couch_api:open_doc(Db, DocId, Revs, Options)
        after
            couch_db:close(Db)
        end;
    {not_found, no_db_file} ->
        throw({not_found, <<"The database does not exist.">>});
    Error ->
        throw(Error)
    end.

get_doc_info(DbName) ->
    case couch_db:open(DbName, []) of
    {ok, Db} ->
        try
            couch_db:get_db_info(Db)
        after
            couch_db:close(Db)
        end;
    {not_found, no_db_file} ->
        throw({not_found, <<"The database does not exist.">>});
    Error ->
        throw(Error)
    end.

%%
%% helper funs
%%

%% @doc set up the receive loop with an overall timeout
-spec receive_loop([ref_part_map()], integer(), function()) -> {ok, beg_acc()}.
receive_loop(RefPartMap, Timeout, Loop) ->
    TimeoutRef = erlang:make_ref(),
    {ok, TRef} = timer:send_after(Timeout, {timeout, TimeoutRef}),
    Results = Loop(RefPartMap, TimeoutRef, []),
    timer:cancel(TRef),
    Results.
%{Status, Info} = couch_db:get_db_info(Shard),
