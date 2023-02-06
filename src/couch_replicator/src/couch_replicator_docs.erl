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

-module(couch_replicator_docs).

-export([
    before_doc_update/3,
    after_doc_read/2,
    delete_old_rep_ddoc/1,
    remove_state_fields/2,
    update_doc_completed/3,
    update_failed/3,
    update_rep_id/1,
    update_triggered/2,
    update_error/2
]).

-include_lib("couch/include/couch_db.hrl").
-include("couch_replicator.hrl").

% The ID of now deleted design doc. On every *_replicator db discovery we try
% to delete it. At some point in the future, remove this logic altogether.
-define(REP_DESIGN_DOC, <<"_design/_replicator">>).
-define(OWNER, <<"owner">>).
-define(CTX, {user_ctx, #user_ctx{roles = [<<"_admin">>, <<"_replicator">>]}}).
-define(replace(L, K, V), lists:keystore(K, 1, L, {K, V})).

remove_state_fields(DbName, DocId) ->
    update_rep_doc(DbName, DocId, [
        {<<"_replication_state">>, undefined},
        {<<"_replication_state_time">>, undefined},
        {<<"_replication_state_reason">>, undefined},
        {<<"_replication_id">>, undefined},
        {<<"_replication_stats">>, undefined}
    ]).

-spec update_doc_completed(binary(), binary(), [_]) -> any().
update_doc_completed(DbName, DocId, Stats) ->
    update_rep_doc(DbName, DocId, [
        {<<"_replication_state">>, <<"completed">>},
        {<<"_replication_state_reason">>, undefined},
        {<<"_replication_stats">>, {Stats}}
    ]),
    couch_stats:increment_counter([
        couch_replicator,
        docs,
        completed_state_updates
    ]).

-spec update_failed(binary(), binary(), any()) -> any().
update_failed(DbName, DocId, Error) ->
    Reason = error_reason(Error),
    couch_log:error(
        "Error processing replication doc `~s` from `~s`: ~s",
        [DocId, DbName, Reason]
    ),
    update_rep_doc(DbName, DocId, [
        {<<"_replication_state">>, <<"failed">>},
        {<<"_replication_stats">>, undefined},
        {<<"_replication_state_reason">>, Reason}
    ]),
    couch_stats:increment_counter([
        couch_replicator,
        docs,
        failed_state_updates
    ]).

-spec update_triggered(#rep{}, rep_id()) -> ok.
update_triggered(Rep, {Base, Ext}) ->
    #rep{
        db_name = DbName,
        doc_id = DocId
    } = Rep,
    update_rep_doc(DbName, DocId, [
        {<<"_replication_state">>, <<"triggered">>},
        {<<"_replication_state_reason">>, undefined},
        {<<"_replication_id">>, iolist_to_binary([Base, Ext])},
        {<<"_replication_stats">>, undefined}
    ]),
    ok.

-spec update_error(#rep{}, any()) -> ok.
update_error(#rep{db_name = DbName, doc_id = DocId, id = RepId}, Error) ->
    Reason = error_reason(Error),
    BinRepId =
        case RepId of
            {Base, Ext} ->
                iolist_to_binary([Base, Ext]);
            _Other ->
                null
        end,
    update_rep_doc(DbName, DocId, [
        {<<"_replication_state">>, <<"error">>},
        {<<"_replication_state_reason">>, Reason},
        {<<"_replication_stats">>, undefined},
        {<<"_replication_id">>, BinRepId}
    ]),
    ok.

-spec delete_old_rep_ddoc(binary()) -> ok.
delete_old_rep_ddoc(RepDb) ->
    case mem3:belongs(RepDb, ?REP_DESIGN_DOC) of
        true -> delete_old_rep_ddoc(RepDb, ?REP_DESIGN_DOC);
        false -> ok
    end.

-spec delete_old_rep_ddoc(binary(), binary()) -> ok.
delete_old_rep_ddoc(RepDb, DDocId) ->
    case open_rep_doc(RepDb, DDocId) of
        {not_found, no_db_file} ->
            ok;
        {not_found, _Reason} ->
            ok;
        {ok, Doc} ->
            DeletedDoc = Doc#doc{deleted = true, body = {[]}},
            try
                save_rep_doc(RepDb, DeletedDoc)
            catch
                throw:conflict ->
                    % ignore, we'll retry next time
                    ok
            end
    end,
    ok.

% Update a #rep{} record with a replication_id. Calculating the id might involve
% fetching a filter from the source db, and so it could fail intermetently.
% In case of a failure to fetch the filter this function will throw a
%  `{filter_fetch_error, Reason} exception.
update_rep_id(Rep) ->
    RepId = couch_replicator_ids:replication_id(Rep),
    Rep#rep{id = RepId}.

update_rep_doc(RepDbName, RepDocId, KVs) ->
    update_rep_doc(RepDbName, RepDocId, KVs, 1).

update_rep_doc(RepDbName, RepDocId, KVs, Wait) when is_binary(RepDocId) ->
    try
        case open_rep_doc(RepDbName, RepDocId) of
            {ok, LastRepDoc} ->
                update_rep_doc(RepDbName, LastRepDoc, KVs, Wait * 2);
            _ ->
                ok
        end
    catch
        throw:conflict ->
            Msg = "Conflict when updating replication doc `~s`. Retrying.",
            couch_log:error(Msg, [RepDocId]),
            ok = timer:sleep(couch_rand:uniform(erlang:min(128, Wait)) * 100),
            update_rep_doc(RepDbName, RepDocId, KVs, Wait * 2)
    end;
update_rep_doc(RepDbName, #doc{body = {RepDocBody}} = RepDoc, KVs, _Try) ->
    NewRepDocBody = lists:foldl(
        fun
            ({K, undefined}, Body) ->
                lists:keydelete(K, 1, Body);
            ({<<"_replication_state">> = K, State} = KV, Body) ->
                case get_json_value(K, Body) of
                    State ->
                        Body;
                    _ ->
                        Body1 = lists:keystore(K, 1, Body, KV),
                        Timestamp = couch_replicator_utils:iso8601(os:timestamp()),
                        lists:keystore(
                            <<"_replication_state_time">>,
                            1,
                            Body1,
                            {<<"_replication_state_time">>, Timestamp}
                        )
                end;
            ({K, _V} = KV, Body) ->
                lists:keystore(K, 1, Body, KV)
        end,
        RepDocBody,
        KVs
    ),
    case NewRepDocBody of
        RepDocBody ->
            ok;
        _ ->
            % Might not succeed - when the replication doc is deleted right
            % before this update (not an error, ignore).
            save_rep_doc(RepDbName, RepDoc#doc{body = {NewRepDocBody}})
    end.

open_rep_doc(DbName, DocId) ->
    ioq:maybe_set_io_priority({system, DbName}),
    case couch_db:open_int(DbName, [?CTX, sys_db]) of
        {ok, Db} ->
            try
                couch_db:open_doc(Db, DocId, [ejson_body])
            after
                couch_db:close(Db)
            end;
        Else ->
            Else
    end.

save_rep_doc(DbName, Doc) ->
    ioq:maybe_set_io_priority({system, DbName}),
    {ok, Db} = couch_db:open_int(DbName, [?CTX, sys_db]),
    try
        couch_db:update_doc(Db, Doc, [])
    catch
        % User can accidentally write a VDU which prevents _replicator from
        % updating replication documents. Avoid crashing replicator and thus
        % preventing all other replication jobs on the node from running.
        throw:{forbidden, Reason} ->
            Msg = "~p VDU function preventing doc update to ~s ~s ~p",
            couch_log:error(Msg, [?MODULE, DbName, Doc#doc.id, Reason]),
            {ok, forbidden}
    after
        couch_db:close(Db)
    end.

-spec before_doc_update(#doc{}, Db :: any(), couch_db:update_type()) -> #doc{}.
before_doc_update(#doc{id = <<?DESIGN_DOC_PREFIX, _/binary>>} = Doc, _Db, _UpdateType) ->
    Doc;
before_doc_update(#doc{id = <<?LOCAL_DOC_PREFIX, _/binary>>} = Doc, _Db, _UpdateType) ->
    Doc;
before_doc_update(#doc{} = Doc, _Db, ?REPLICATED_CHANGES) ->
    % Skip internal replicator updates
    Doc;
before_doc_update(#doc{body = {Body}} = Doc, Db, _UpdateType) ->
    #user_ctx{roles = Roles, name = Name} = couch_db:get_user_ctx(Db),
    IsReplicator = lists:member(<<"_replicator">>, Roles),
    Doc1 =
        case IsReplicator of
            true -> Doc;
            false -> before_doc_update_owner(get_value(?OWNER, Body), Name, Db, Doc)
        end,
    IsFailed = get_value(<<"_replication_state">>, Body) =:= <<"failed">>,
    case IsReplicator orelse Doc1#doc.deleted orelse IsFailed of
        true ->
            ok;
        false ->
            try
                couch_replicator_parse:parse_rep_doc_without_id(Doc1#doc.body)
            catch
                throw:{bad_rep_doc, Error} ->
                    throw({forbidden, Error})
            end
    end,
    Doc1.

before_doc_update_owner(undefined, Name, _Db, #doc{body = {Body}} = Doc) ->
    Doc#doc{body = {?replace(Body, ?OWNER, Name)}};
before_doc_update_owner(Name, Name, _Db, #doc{} = Doc) ->
    Doc;
before_doc_update_owner(Other, Name, Db, #doc{body = {Body}} = Doc) ->
    case (catch couch_db:check_is_admin(Db)) of
        ok when Other =:= null ->
            Doc#doc{body = {?replace(Body, ?OWNER, Name)}};
        ok ->
            Doc;
        _ ->
            Err = <<"Can't update replication documents from other users.">>,
            throw({forbidden, Err})
    end.

-spec after_doc_read(#doc{}, Db :: any()) -> #doc{}.
after_doc_read(#doc{id = <<?DESIGN_DOC_PREFIX, _/binary>>} = Doc, _Db) ->
    Doc;
after_doc_read(#doc{body = {Body}} = Doc, Db) ->
    #user_ctx{name = Name} = couch_db:get_user_ctx(Db),
    case (catch couch_db:check_is_admin(Db)) of
        ok ->
            Doc;
        _ ->
            case get_value(?OWNER, Body) of
                Name ->
                    Doc;
                _Other ->
                    Source = strip_credentials(get_value(<<"source">>, Body)),
                    Target = strip_credentials(get_value(<<"target">>, Body)),
                    NewBody0 = ?replace(Body, <<"source">>, Source),
                    NewBody = ?replace(NewBody0, <<"target">>, Target),
                    #doc{revs = {Pos, [_ | Revs]}} = Doc,
                    NewDoc = Doc#doc{body = {NewBody}, revs = {Pos - 1, Revs}},
                    NewRevId = couch_db:new_revid(NewDoc),
                    NewDoc#doc{revs = {Pos, [NewRevId | Revs]}}
            end
    end.

-spec strip_credentials
    (undefined) -> undefined;
    (binary()) -> binary();
    ({[_]}) -> {[_]}.
strip_credentials(undefined) ->
    undefined;
strip_credentials(Url) when is_binary(Url) ->
    re:replace(
        Url,
        "http(s)?://(?:[^:]+):[^@]+@(.*)$",
        "http\\1://\\2",
        [{return, binary}]
    );
strip_credentials({Props0}) ->
    Props1 = lists:keydelete(<<"headers">>, 1, Props0),
    % Strip "auth" just like headers, for replication plugins it can be a place
    % to stash credential that are not necessarily in headers
    Props2 = lists:keydelete(<<"auth">>, 1, Props1),
    {Props2}.

error_reason({shutdown, Error}) ->
    error_reason(Error);
error_reason({bad_rep_doc, Reason}) ->
    to_binary(Reason);
error_reason({error, {Error, Reason}}) when
    is_atom(Error), is_binary(Reason)
->
    to_binary(io_lib:format("~s: ~s", [Error, Reason]));
error_reason({error, Reason}) ->
    to_binary(Reason);
error_reason(Reason) ->
    to_binary(Reason).

to_binary(Val) ->
    couch_util:to_binary(Val).

get_value(Key, Props) ->
    couch_util:get_value(Key, Props).

get_json_value(Key, Obj) ->
    couch_replicator_utils:get_json_value(Key, Obj).

-ifdef(TEST).

-include_lib("couch/include/couch_eunit.hrl").

check_strip_credentials_test() ->
    [
        ?assertEqual(Expected, strip_credentials(Body))
     || {Expected, Body} <- [
            {
                undefined,
                undefined
            },
            {
                <<"https://remote_server/database">>,
                <<"https://foo:bar@remote_server/database">>
            },
            {
                {[{<<"_id">>, <<"foo">>}]},
                {[{<<"_id">>, <<"foo">>}, {<<"headers">>, <<"bar">>}]}
            },
            {
                {[{<<"_id">>, <<"foo">>}, {<<"other">>, <<"bar">>}]},
                {[{<<"_id">>, <<"foo">>}, {<<"other">>, <<"bar">>}]}
            },
            {
                {[{<<"_id">>, <<"foo">>}]},
                {[{<<"_id">>, <<"foo">>}, {<<"headers">>, <<"baz">>}]}
            },
            {
                {[{<<"_id">>, <<"foo">>}]},
                {[{<<"_id">>, <<"foo">>}, {<<"auth">>, <<"pluginsecret">>}]}
            }
        ]
    ].

setup() ->
    TmpDbName = ?tempdb(),
    DbName = <<TmpDbName/binary, "/_replicator">>,
    {ok, Db} = couch_db:create(DbName, [?ADMIN_CTX]),
    ok = couch_db:close(Db),
    DbName.

teardown(DbName) when is_binary(DbName) ->
    couch_server:delete(DbName, [?ADMIN_CTX]),
    ok.

create_old_rep_ddoc(DbName) ->
    couch_util:with_db(DbName, fun(Db) ->
        Doc = #doc{id = ?REP_DESIGN_DOC, body = {[]}},
        {ok, _} = couch_db:update_docs(Db, [Doc]),
        ok
    end).

clean_old_replicator_ddoc_test_() ->
    {
        setup,
        fun test_util:start_couch/0,
        fun test_util:stop_couch/1,
        {
            foreach,
            fun setup/0,
            fun teardown/1,
            [
                ?TDEF_FE(t_clean_old_ddoc),
                ?TDEF_FE(t_old_ddoc_already_cleaned),
                ?TDEF_FE(t_ddoc_delete_missing_db)
            ]
        }
    }.

t_clean_old_ddoc(DbName) ->
    ok = create_old_rep_ddoc(DbName),
    ?assertMatch({ok, #doc{}}, open_rep_doc(DbName, ?REP_DESIGN_DOC)),
    delete_old_rep_ddoc(DbName),
    ?assertEqual({not_found, deleted}, open_rep_doc(DbName, ?REP_DESIGN_DOC)).

t_old_ddoc_already_cleaned(DbName) ->
    ok = delete_old_rep_ddoc(DbName),
    ?assertEqual({not_found, missing}, open_rep_doc(DbName, ?REP_DESIGN_DOC)).

t_ddoc_delete_missing_db(_DbName) ->
    ok = delete_old_rep_ddoc(<<"someotherdb">>).

replicator_can_update_docs_test_() ->
    {
        setup,
        fun test_util:start_couch/0,
        fun test_util:stop_couch/1,
        {
            foreach,
            fun setup/0,
            fun teardown/1,
            [
                ?TDEF_FE(t_remove_state_fields),
                ?TDEF_FE(t_update_doc_completed),
                ?TDEF_FE(t_update_failed),
                ?TDEF_FE(t_update_triggered),
                ?TDEF_FE(t_update_error)
            ]
        }
    }.

t_remove_state_fields(DbName) ->
    DocId = <<"doc1">>,
    Doc = #doc{
        id = DocId,
        body = {[{<<"_replication_state">>, <<"triggered">>}]}
    },
    save_rep_doc(DbName, Doc),
    remove_state_fields(DbName, DocId),
    {ok, Doc2} = open_rep_doc(DbName, DocId),
    ?assertEqual({[]}, Doc2#doc.body).

t_update_doc_completed(DbName) ->
    DocId = <<"doc1">>,
    Doc = #doc{
        id = DocId,
        body = {[{<<"_replication_state">>, <<"triggered">>}]}
    },
    save_rep_doc(DbName, Doc),
    update_doc_completed(DbName, DocId, [{<<"foo">>, 1}]),
    {ok, Doc2} = open_rep_doc(DbName, DocId),
    {Props} = Doc2#doc.body,
    State = get_value(<<"_replication_state">>, Props),
    ?assertEqual(<<"completed">>, State),
    Stats = get_value(<<"_replication_stats">>, Props),
    ?assertEqual({[{<<"foo">>, 1}]}, Stats).

t_update_failed(DbName) ->
    DocId = <<"doc1">>,
    Doc = #doc{
        id = DocId,
        body =
            {[
                {<<"_replication_state">>, <<"triggered">>},
                {<<"_replication_stats">>, {[{<<"foo">>, 1}]}}
            ]}
    },
    save_rep_doc(DbName, Doc),
    Error = {error, {foo, bar}},
    update_failed(DbName, DocId, Error),
    {ok, Doc2} = open_rep_doc(DbName, DocId),
    {Props} = Doc2#doc.body,
    State = get_value(<<"_replication_state">>, Props),
    ?assertEqual(<<"failed">>, State),
    Reason = get_value(<<"_replication_state_reason">>, Props),
    ?assertEqual(<<"{foo,bar}">>, Reason),
    % stats should have been cleared
    Stats = get_value(<<"_replication_stats">>, Props),
    ?assertEqual(undefined, Stats).

t_update_triggered(DbName) ->
    DocId = <<"doc1">>,
    Doc = #doc{
        id = DocId,
        body = {[{}]}
    },
    save_rep_doc(DbName, Doc),
    Rep = #rep{db_name = DbName, doc_id = DocId},
    update_triggered(Rep, {"123", "+continuous"}),
    {ok, Doc2} = open_rep_doc(DbName, DocId),
    {Props} = Doc2#doc.body,
    State = get_value(<<"_replication_state">>, Props),
    ?assertEqual(<<"triggered">>, State),
    Stats = get_value(<<"_replication_stats">>, Props),
    ?assertEqual(undefined, Stats),
    RepId = get_value(<<"_replication_id">>, Props),
    ?assertEqual(<<"123+continuous">>, RepId).

t_update_error(DbName) ->
    DocId = <<"doc1">>,
    Doc = #doc{
        id = DocId,
        body = {[{}]}
    },
    save_rep_doc(DbName, Doc),
    Rep = #rep{db_name = DbName, doc_id = DocId, id = null},
    Error = {error, foo},
    update_error(Rep, Error),
    {ok, Doc2} = open_rep_doc(DbName, DocId),
    {Props} = Doc2#doc.body,
    State = get_value(<<"_replication_state">>, Props),
    ?assertEqual(<<"error">>, State),
    Stats = get_value(<<"_replication_stats">>, Props),
    ?assertEqual(undefined, Stats),
    RepId = get_value(<<"_replication_id">>, Props),
    ?assertEqual(null, RepId).

-endif.
