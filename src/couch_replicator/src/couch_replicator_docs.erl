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
    remove_state_fields/3,
    update_completed/4,
    update_failed/4,
    before_doc_update/3,
    after_doc_read/2
]).

-include_lib("couch/include/couch_db.hrl").
-include_lib("couch_replicator/include/couch_replicator_api_wrap.hrl").
-include("couch_replicator.hrl").
-include_lib("kernel/include/logger.hrl").

-define(OWNER, <<"owner">>).
-define(CTX, {user_ctx, #user_ctx{roles = [<<"_admin">>, <<"_replicator">>]}}).
-define(replace(L, K, V), lists:keystore(K, 1, L, {K, V})).

remove_state_fields(null, null, null) ->
    ok;
remove_state_fields(DbName, DbUUID, DocId) ->
    update_rep_doc(DbName, DbUUID, DocId, [
        {?REPLICATION_STATE, undefined},
        {?REPLICATION_STATE_TIME, undefined},
        {?REPLICATION_STATE_REASON, undefined},
        {?REPLICATION_ID, undefined},
        {?REPLICATION_STATS, undefined}
    ]),
    ok.

-spec update_completed(binary(), binary(), binary(), [_]) -> ok.
update_completed(null, null, _, _) ->
    ok;
update_completed(DbName, DbUUID, DocId, #{} = Stats0) ->
    Stats = {maps:to_list(Stats0)},
    update_rep_doc(DbName, DbUUID, DocId, [
        {?REPLICATION_STATE, ?ST_COMPLETED},
        {?REPLICATION_STATE_REASON, undefined},
        {?REPLICATION_STATS, Stats}
    ]),
    couch_stats:increment_counter([
        couch_replicator,
        docs,
        completed_state_updates
    ]),
    ok.

-spec update_failed(binary(), binary(), binary(), any()) -> ok.
update_failed(null, null, null, _) ->
    ok;
update_failed(DbName, DbUUID, DocId, Error) ->
    Reason = error_reason(Error),
    ?LOG_ERROR(#{
        what => replication_failed,
        in => replicator,
        replicator_db => DbName,
        replicator_doc => DocId,
        details => Reason
    }),
    couch_log:error(
        "Error processing replication doc `~s` from `~s`: ~s",
        [DocId, DbName, Reason]
    ),
    update_rep_doc(DbName, DbUUID, DocId, [
        {?REPLICATION_STATE, ?ST_FAILED},
        {?REPLICATION_STATS, undefined},
        {?REPLICATION_STATE_REASON, Reason}
    ]),
    couch_stats:increment_counter([
        couch_replicator,
        docs,
        failed_state_updates
    ]),
    ok.

-spec before_doc_update(#doc{}, Db :: any(), couch_db:update_type()) -> #doc{}.
before_doc_update(#doc{id = <<?DESIGN_DOC_PREFIX, _/binary>>} = Doc, _, _) ->
    Doc;
before_doc_update(#doc{body = {Body}} = Doc, Db, _UpdateType) ->
    #user_ctx{roles = Roles, name = Name} = fabric2_db:get_user_ctx(Db),
    IsReplicator = lists:member(<<"_replicator">>, Roles),

    Doc1 =
        case IsReplicator of
            true ->
                Doc;
            false ->
                case couch_util:get_value(?OWNER, Body) of
                    undefined ->
                        Doc#doc{body = {?replace(Body, ?OWNER, Name)}};
                    Name ->
                        Doc;
                    Other ->
                        case (catch fabric2_db:check_is_admin(Db)) of
                            ok when Other =:= null ->
                                Doc#doc{body = {?replace(Body, ?OWNER, Name)}};
                            ok ->
                                Doc;
                            _ ->
                                throw(
                                    {forbidden,
                                        <<"Can't update replication",
                                            "documents from other users.">>}
                                )
                        end
                end
        end,

    Deleted = Doc1#doc.deleted,
    IsFailed = couch_util:get_value(?REPLICATION_STATE, Body) == ?ST_FAILED,
    case IsReplicator orelse Deleted orelse IsFailed of
        true ->
            ok;
        false ->
            try
                couch_replicator_parse:parse_rep_doc(Doc1#doc.body)
            catch
                throw:{bad_rep_doc, Error} ->
                    throw({forbidden, Error})
            end
    end,
    Doc1.

-spec after_doc_read(#doc{}, Db :: any()) -> #doc{}.
after_doc_read(#doc{id = <<?DESIGN_DOC_PREFIX, _/binary>>} = Doc, _Db) ->
    Doc;
after_doc_read(#doc{body = {Body}} = Doc, Db) ->
    #user_ctx{name = Name} = fabric2_db:get_user_ctx(Db),
    case (catch fabric2_db:check_is_admin(Db)) of
        ok ->
            Doc;
        _ ->
            case couch_util:get_value(?OWNER, Body) of
                Name ->
                    Doc;
                _ ->
                    Source0 = couch_util:get_value(<<"source">>, Body),
                    Target0 = couch_util:get_value(<<"target">>, Body),
                    Source = strip_credentials(Source0),
                    Target = strip_credentials(Target0),
                    NewBody0 = ?replace(Body, <<"source">>, Source),
                    NewBody = ?replace(NewBody0, <<"target">>, Target),
                    #doc{revs = {Pos, [_ | Revs]}} = Doc,
                    NewDoc = Doc#doc{body = {NewBody}, revs = {Pos - 1, Revs}},
                    fabric2_db:new_revid(Db, NewDoc)
            end
    end.

update_rep_doc(RepDbName, RepDbUUID, RepDocId, KVs) ->
    update_rep_doc(RepDbName, RepDbUUID, RepDocId, KVs, 1).

update_rep_doc(RepDbName, RepDbUUID, RepDocId, KVs, Wait) when
    is_binary(RepDocId)
->
    try
        case open_rep_doc(RepDbName, RepDbUUID, RepDocId) of
            {ok, LastRepDoc} ->
                update_rep_doc(
                    RepDbName,
                    RepDbUUID,
                    LastRepDoc,
                    KVs,
                    Wait * 2
                );
            _ ->
                ok
        end
    catch
        throw:conflict ->
            Delay = couch_rand:uniform(erlang:min(128, Wait)) * 100,
            ?LOG_ERROR(#{
                what => replication_doc_conflict,
                in => replicator,
                replication_db => RepDbName,
                replication_doc => RepDocId,
                retry_delay_sec => Delay
            }),
            Msg = "Conflict when updating replication doc `~s`. Retrying.",
            couch_log:error(Msg, [RepDocId]),
            ok = timer:sleep(Delay),
            update_rep_doc(RepDbName, RepDbUUID, RepDocId, KVs, Wait * 2)
    end;
update_rep_doc(RepDbName, RepDbUUID, #doc{body = {RepDocBody}} = RepDoc, KVs, _Try) ->
    NewRepDocBody = lists:foldl(
        fun
            ({K, undefined}, Body) when is_binary(K) ->
                lists:keydelete(K, 1, Body);
            ({?REPLICATION_STATE = K, State} = KV, Body) when is_binary(K) ->
                case couch_util:get_value(K, Body) of
                    State ->
                        Body;
                    _ ->
                        Body1 = lists:keystore(K, 1, Body, KV),
                        Timestamp = couch_replicator_utils:iso8601(),
                        lists:keystore(
                            ?REPLICATION_STATE_TIME,
                            1,
                            Body1,
                            {?REPLICATION_STATE_TIME, Timestamp}
                        )
                end;
            ({K, _V} = KV, Body) when is_binary(K) ->
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
            save_rep_doc(RepDbName, RepDbUUID, RepDoc#doc{body = {NewRepDocBody}})
    end.

open_rep_doc(DbName, DbUUID, DocId) when
    is_binary(DbName),
    is_binary(DbUUID),
    is_binary(DocId)
->
    try
        case fabric2_db:open(DbName, [?CTX, sys_db, {uuid, DbUUID}]) of
            {ok, Db} -> fabric2_db:open_doc(Db, DocId, [ejson_body]);
            Else -> Else
        end
    catch
        error:database_does_not_exist ->
            {not_found, database_does_not_exist}
    end.

save_rep_doc(DbName, DbUUID, Doc) when is_binary(DbName), is_binary(DbUUID) ->
    try
        {ok, Db} = fabric2_db:open(DbName, [?CTX, sys_db, {uuid, DbUUID}]),
        fabric2_db:update_doc(Db, Doc, [])
    catch
        error:database_does_not_exist ->
            {not_found, database_does_not_exist};
        % User can accidently write a VDU which prevents _replicator from
        % updating replication documents. Avoid crashing replicator and thus
        % preventing all other replication jobs on the node from running.
        throw:{forbidden, Reason} ->
            ?LOG_ERROR(#{
                what => replication_doc_update_forbidden,
                in => replicator,
                replication_db => DbName,
                replication_doc => Doc#doc.id,
                details => Reason
            }),
            Msg = "~p VDU or BDU function preventing doc update to ~s ~s ~p",
            couch_log:error(Msg, [?MODULE, DbName, Doc#doc.id, Reason]),
            {ok, forbidden}
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
    couch_util:to_binary(Reason);
error_reason(#{<<"error">> := Error, <<"reason">> := Reason}) when
    is_binary(Error), is_binary(Reason)
->
    couch_util:to_binary(io_list:format("~s: ~s", [Error, Reason]));
error_reason({error, {Error, Reason}}) when
    is_atom(Error), is_binary(Reason)
->
    couch_util:to_binary(io_lib:format("~s: ~s", [Error, Reason]));
error_reason({error, Reason}) ->
    couch_util:to_binary(Reason);
error_reason(Reason) ->
    couch_util:to_binary(Reason).
