% Copyright 2013 Cloudant. All rights reserved.

-module(custodian_util).
-include("custodian.hrl").

-export([ensure_dbs_exists/0]).

% public functions.

ensure_dbs_exists() ->
    DbName = couch_config:get("mem3", "shards_db", "dbs"),
    {ok, Db} = mem3_util:ensure_exists(DbName),
    ensure_custodian_ddoc_exists(Db),
    {ok, Db}.

% private functions.

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
