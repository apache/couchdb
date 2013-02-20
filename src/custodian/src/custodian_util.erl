% Copyright 2013 Cloudant. All rights reserved.

-module(custodian_util).
-include("custodian.hrl").

-include_lib("mem3/include/mem3.hrl").
-include_lib("couch/include/couch_db.hrl").

-export([summary/0, report/0]).

%% public functions.

summary() ->
    Fun = fun(_Id, _Range,  unavailable, {Unavailable, Impaired}) ->
                  {Unavailable + 1, Impaired};
             (_Id, _Range,  {impaired, _N}, {Unavailable, Impaired}) ->
                  {Unavailable, Impaired + 1}
          end,
    fold_dbs({0, 0}, Fun).

report() ->
    Fun = fun(Id, Range,  unavailable, Acc) ->
                  [{Id, Range, unavailable}|Acc];
             (Id, Range,  {impaired, N}, Acc) ->
                  [{Id, Range, {impaired, N}}|Acc]
          end,
    fold_dbs([], Fun).

%% private functions.

fold_dbs(Acc0, Fun) ->
    Live = [node() | nodes()],
    N = list_to_integer(couch_config:get("cluster", "n", "3")),
    {ok, Db} = ensure_dbs_exists(),
    try
        {ok, _, {_, _, _, Acc1}} = couch_db:enum_docs(Db, fun fold_dbs/3, {Live, N, Fun, Acc0}, []),
        Acc1
    after
        couch_db:close(Db)
    end.

ensure_dbs_exists() ->
    DbName = couch_config:get("mem3", "shards_db", "dbs"),
    erlang:put(io_priority, {low, DbName}),
    {ok, Db} = mem3_util:ensure_exists(DbName),
    ensure_custodian_ddoc_exists(Db),
    {ok, Db}.

fold_dbs(#full_doc_info{id = <<"_design/", _/binary>>}, _, Acc) ->
    {ok, Acc};
fold_dbs(#full_doc_info{deleted=true}, _, Acc) ->
    {ok, Acc};
fold_dbs(#full_doc_info{id = Id}, _, Acc) ->
    Shards = mem3:shards(Id),
    Rs = [R || #shard{range=R} <- lists:ukeysort(#shard.range, Shards)],
    ActualN = [{R1, [N || #shard{node=N,range=R2} <- Shards, R1 == R2]} ||  R1 <- Rs],
    fold_dbs(Id, ActualN, Acc);
fold_dbs(_Id, [], Acc) ->
    {ok, Acc};
fold_dbs(Id, [{Range, Nodes}|Rest], {Live, N, Fun, Acc0}) ->
    Nodes1 = maybe_redirect(Nodes),
    Nodes2 = [Node || Node <- Nodes1, lists:member(Node, Live)],
    Acc1 = case length(Nodes2) of
        0 ->
            Fun(Id, Range, unavailable, Acc0);
        N1 when N1 < N ->
            Fun(Id, Range, {impaired, N1}, Acc0);
        _ ->
            Acc0
    end,
    fold_dbs(Id, Rest, {Live, N, Fun, Acc1}).

maybe_redirect(Nodes) ->
    maybe_redirect(Nodes, []).

maybe_redirect([], Acc) ->
    Acc;
maybe_redirect([Node|Rest], Acc) ->
    case couch_config:get("mem3.redirects", atom_to_list(Node)) of
        undefined ->
            maybe_redirect(Rest, [Node|Acc]);
        Redirect ->
            maybe_redirect(Rest, [list_to_atom(Redirect)|Acc])
    end.

ensure_custodian_ddoc_exists(Db) ->
    case couch_db:open_doc(Db, ?CUSTODIAN_ID) of
        {not_found, _Reason} ->
            try couch_db:update_doc(Db, custodian_ddoc(), []) of
            {ok, _} ->
                ok
            catch conflict ->
                ensure_custodian_ddoc_exists(Db)
            end;
        {ok, Doc} ->
            {Props} = couch_doc:to_json_obj(Doc, []),
            Props1 = lists:keystore(<<"validate_doc_update">>, 1, Props, {<<"validate_doc_update">>, ?CUSTODIAN_VALIDATION}),
            case Props =:= Props1 of
                true ->
                    ok;
                false ->
                    try couch_db:update_doc(Db, couch_doc:from_json_obj({Props1}), []) of
                    {ok, _} ->
                        ok
                    catch conflict ->
                        ensure_custodian_ddoc_exists(Db)
                    end
            end
    end.

custodian_ddoc() ->
    Props = [
        {<<"_id">>, ?CUSTODIAN_ID},
        {<<"language">>, <<"javascript">>},
        {<<"validate_doc_update">>, ?CUSTODIAN_VALIDATION}
    ],
    couch_doc:from_json_obj({Props}).
