% Copyright 2013 Cloudant. All rights reserved.

-module(custodian).

-include_lib("couch/include/couch_db.hrl").
-include_lib("mem3/include/mem3.hrl").
-include("custodian.hrl").

-export([summary/0, summary/1]).
-export([report/0, report/1]).

% public functions.

summary() ->
    {ok, TrulyDown} = custodian_server:truly_down(),
    summary(TrulyDown).

summary(TrulyDown) ->
    scan(0, fun summary/5, TrulyDown).

report() ->
    {ok, TrulyDown} = custodian_server:truly_down(),
    report(TrulyDown).

report(TrulyDown) ->
    scan([], fun report/5, TrulyDown).

% private functions.

scan(Init, AccFun, TrulyDown) ->
    ExpectedN = list_to_integer(couch_config:get("cluster", "n", "3")),
    {ok, Db} = ensure_dbs_exists(ExpectedN),
    try
        Init1 = {TrulyDown, ExpectedN, Init, AccFun},
        {ok, _, LastAcc} = couch_db:enum_docs(Db, fun fold_fun/3, Init1, []),
        {_, _, Acc, _} = LastAcc,
        {ok, Acc}
    after
        couch_db:close(Db)
    end.

fold_fun(#full_doc_info{id = <<"_", _/binary>>}, _, Acc) ->
    {ok, Acc};
fold_fun(#full_doc_info{deleted=true}, _, Acc) ->
    {ok, Acc};
fold_fun(#full_doc_info{id = Id}, _, Acc) ->
    Shards = mem3:shards(Id),
    Rs = [R || #shard{range=R} <- lists:ukeysort(#shard.range, Shards)],
    ActualN = [{R1, [N || #shard{node=N,range=R2} <- Shards, R1 == R2]} ||  R1 <- Rs],
    fold_fun(Id, ActualN, Acc);
fold_fun(_Id, [], Acc) ->
    {ok, Acc};
fold_fun(Id, [{Range, Nodes}|Rest], {TrulyDown, ExpectedN, Acc, Fun}) ->
    Nodes1 = [maybe_redirect(Node) || Node <- Nodes],
    Nodes2 = Nodes1 -- TrulyDown,
    NewAcc = case length(Nodes2) < ExpectedN of
        true ->
            Fun(Id, Range, length(Nodes2), ExpectedN, Acc);
        false ->
            Acc
    end,
    fold_fun(Id, Rest, {TrulyDown, ExpectedN, NewAcc, Fun}).

maybe_redirect(Node) ->
    case couch_config:get("mem3.redirects", atom_to_list(Node)) of
        undefined ->
            Node;
        Redirect ->
            list_to_existing_atom(Redirect)
    end.

summary(_Id, _Range, _ActualN, _ExpectedN, Acc) ->
    Acc + 1.

report(Id, Range, ActualN, ExpectedN, Acc) ->
    [{Id, Range, ActualN, ExpectedN}|Acc].

ensure_dbs_exists(N) ->
    DbName = couch_config:get("mem3", "shards_db", "dbs"),
    {ok, Db} = mem3_util:ensure_exists(DbName),
    ensure_custodian_ddoc_exists(Db, N),
    {ok, Db}.

ensure_custodian_ddoc_exists(Db, N) ->
    case couch_db:open_doc(Db, <<"_design/custodian">>) of
        {not_found, _Reason} ->
            try couch_db:update_doc(Db, custodian_ddoc(N), []) of
            {ok, _} ->
                ok
            catch conflict ->
                ensure_custodian_ddoc_exists(Db, N)
            end;
        {ok, Doc} ->
            {Props} = couch_doc:to_json_obj(Doc, []),
            Props1 = lists:keystore(<<"views">>, 1, Props, {<<"views">>, custodian_views(N)}),
            Props2 = lists:keystore(<<"validate_doc_update">>, 1, Props1, {<<"validate_doc_update">>, ?CUSTODIAN_VALIDATION}),
            case Props =:= Props2 of
                true ->
                    ok;
                false ->
                    try couch_db:update_doc(Db, couch_doc:from_json_obj({Props2}), []) of
                    {ok, _} ->
                        ok
                    catch conflict ->
                        ensure_custodian_ddoc_exists(Db, N)
                    end
            end
    end.

custodian_ddoc(N) ->
    Props = [
        {<<"_id">>, <<"_design/custodian">>},
        {<<"language">>, <<"javascript">>},
        {<<"views">>, custodian_views(N)},
        {<<"validate_doc_update">>, ?CUSTODIAN_VALIDATION}
    ],
    couch_doc:from_json_obj({Props}).

custodian_views(N) when is_integer(N) ->
    {[under_n_view(N), by_node_range_view()]}.

under_n_view(N) when is_integer(N) ->
    N1 = list_to_binary(integer_to_list(N)),
    {<<"under_n">>, view(?CUSTODIAN_UNDER_N(N1), <<"_sum">>)}.

by_node_range_view() ->
    {<<"by_node_range">>, view(?CUSTODIAN_BY_NODE_RANGE, <<"_sum">>)}.

view(Map, Reduce) ->
    {[{<<"map">>, Map},{<<"reduce">>, Reduce}]}.
