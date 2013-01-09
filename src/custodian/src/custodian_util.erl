% Copyright 2013 Cloudant. All rights reserved.

-module(custodian_util).
-include_lib("couch/include/couch_db.hrl").
-include("custodian.hrl").

-export([hosting_nodes/0, replication_count/1]).

% public functions.

-spec replication_count(non_neg_integer()) -> {ok, non_neg_integer()}.
replication_count(N) when N >= 0 ->
    Fun = fun(_K, Red, Acc) -> {ok, Red + Acc} end,
    KeyArgs = #view_query_args{start_key=[N], end_key=[N, {}]},
    Options0 = couch_httpd_view:make_key_options(KeyArgs),
    Options = [{key_group_fun, group_level(1)} | Options0],
    query_view(<<"by_n">>, Fun, 0, Options).

-spec hosting_nodes() -> {ok, [binary()]}.
hosting_nodes() ->
    Fun = fun(K, _Red, Acc) -> {ok, [hd(K) | Acc]} end,
    Options = [{key_group_fun, group_level(1)}],
    query_view(<<"by_node_range">>, Fun, [], Options).

send_data_loss_alert(Count) when is_integer(Count) ->
    os:cmd("send_snmptrap --trap CLOUDANT-DBCORE-MIB::cloudantDbcoreDataLossEvent cloudantDbcoreShardCount "
           ++ integer_to_list(Count)).

send_impaired_alert(Count) when is_integer(Count) ->
    os:cmd("send_snmptrap --trap CLOUDANT-DBCORE-MIB::cloudantDbcoreRedundancyImpairedEvent cloudantDbcoreShardCount "
           ++ integer_to_list(Count)).

clear_alert() ->
    os:cmd("send_snmptrap --trap CLOUDANT-DBCORE-MIB::cloudantDbcoreRedundancyRestoredEvent").

% private functions.

query_view(ViewName, Fun, Acc, Options) ->
    {ok, Db} = ensure_dbs_exists(),
    try
        {ok, View, _} = couch_view:get_reduce_view(Db, ?CUSTODIAN_ID, ViewName, false),
        couch_view:fold_reduce(View, Fun, Acc, Options)
    after
        couch_db:close(Db)
    end.

ensure_dbs_exists() ->
    DbName = couch_config:get("mem3", "shards_db", "dbs"),
    {ok, Db} = mem3_util:ensure_exists(DbName),
    ensure_custodian_ddoc_exists(Db),
    {ok, Db}.

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
            Props1 = lists:keystore(<<"views">>, 1, Props, {<<"views">>, custodian_views()}),
            Props2 = lists:keystore(<<"validate_doc_update">>, 1, Props1, {<<"validate_doc_update">>, ?CUSTODIAN_VALIDATION}),
            case Props =:= Props2 of
                true ->
                    ok;
                false ->
                    try couch_db:update_doc(Db, couch_doc:from_json_obj({Props2}), []) of
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
        {<<"views">>, custodian_views()},
        {<<"validate_doc_update">>, ?CUSTODIAN_VALIDATION}
    ],
    couch_doc:from_json_obj({Props}).

custodian_views() ->
    {[by_n_view(), by_node_range_view()]}.

by_n_view() ->
    {<<"by_n">>, view(?CUSTODIAN_BY_N, <<"_sum">>)}.

by_node_range_view() ->
    {<<"by_node_range">>, view(?CUSTODIAN_BY_NODE_RANGE, <<"_sum">>)}.

view(Map, Reduce) ->
    {[{<<"map">>, Map},{<<"reduce">>, Reduce}]}.

group_level(exact) ->
    fun({K1, _}, {K2, _}) -> K1 =:= K2 end;
group_level(1) ->
    fun({K1, _}, {K2, _}) -> hd(K1) =:= hd(K2) end.
