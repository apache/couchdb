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

-module(couch_replicator).

-export([
    replicate/2,
    ensure_rep_db_exists/0,
    replication_states/0,
    job/1,
    doc/3,
    active_doc/2,
    info_from_doc/2,
    restart_job/1
]).

-include_lib("couch/include/couch_db.hrl").
-include("couch_replicator.hrl").
-include_lib("couch_replicator/include/couch_replicator_api_wrap.hrl").
-include_lib("couch_mrview/include/couch_mrview.hrl").
-include_lib("mem3/include/mem3.hrl").

-define(DESIGN_DOC_CREATION_DELAY_MSEC, 1000).
-define(REPLICATION_STATES, [
    initializing,  % Just added to scheduler
    error,         % Could not be turned into a replication job
    running,       % Scheduled and running
    pending,       % Scheduled and waiting to run
    crashing,      % Scheduled but crashing, backed off by the scheduler
    completed,     % Non-continuous (normal) completed replication
    failed         % Terminal failure, will not be retried anymore
]).

-import(couch_util, [
    get_value/2,
    get_value/3
]).


-spec replicate({[_]}, any()) ->
    {ok, {continuous, binary()}} |
    {ok, {[_]}} |
    {ok, {cancelled, binary()}} |
    {error, any()} |
    no_return().
replicate(PostBody, Ctx) ->
    {ok, Rep0} = couch_replicator_utils:parse_rep_doc(PostBody, Ctx),
    Rep = Rep0#rep{start_time = os:timestamp()},
    #rep{id = RepId, options = Options, user_ctx = UserCtx} = Rep,
    case get_value(cancel, Options, false) of
    true ->
        CancelRepId = case get_value(id, Options, nil) of
        nil ->
            RepId;
        RepId2 ->
            RepId2
        end,
        case check_authorization(CancelRepId, UserCtx) of
        ok ->
            cancel_replication(CancelRepId);
        not_found ->
            {error, not_found}
        end;
    false ->
        check_authorization(RepId, UserCtx),
        {ok, Listener} = rep_result_listener(RepId),
        Result = do_replication_loop(Rep),
        couch_replicator_notifier:stop(Listener),
        Result
    end.


% This is called from supervisor. Must respect supervisor protocol so
% it returns `ignore`.
-spec ensure_rep_db_exists() -> ignore.
ensure_rep_db_exists() ->
    {ok, _Db} = couch_replicator_docs:ensure_rep_db_exists(),
    ignore.


-spec do_replication_loop(#rep{}) ->
    {ok, {continuous, binary()}} | {ok, tuple()} | {error, any()}.
do_replication_loop(#rep{id = {BaseId, Ext} = Id, options = Options} = Rep) ->
    ok = couch_replicator_scheduler:add_job(Rep),
    case get_value(continuous, Options, false) of
    true ->
        {ok, {continuous, ?l2b(BaseId ++ Ext)}};
    false ->
        wait_for_result(Id)
    end.


-spec rep_result_listener(rep_id()) -> {ok, pid()}.
rep_result_listener(RepId) ->
    ReplyTo = self(),
    {ok, _Listener} = couch_replicator_notifier:start_link(
        fun({_, RepId2, _} = Ev) when RepId2 =:= RepId ->
                ReplyTo ! Ev;
            (_) ->
                ok
        end).


-spec wait_for_result(rep_id()) ->
    {ok, {[_]}} | {error, any()}.
wait_for_result(RepId) ->
    receive
    {finished, RepId, RepResult} ->
        {ok, RepResult};
    {error, RepId, Reason} ->
        {error, Reason}
    end.


-spec cancel_replication(rep_id()) ->
    {ok, {cancelled, binary()}} | {error, not_found}.
cancel_replication({BasedId, Extension} = RepId) ->
    FullRepId = BasedId ++ Extension,
    couch_log:notice("Canceling replication '~s' ...", [FullRepId]),
    case couch_replicator_scheduler:rep_state(RepId) of
    #rep{} ->
        ok = couch_replicator_scheduler:remove_job(RepId),
        couch_log:notice("Replication '~s' cancelled", [FullRepId]),
        {ok, {cancelled, ?l2b(FullRepId)}};
    nil ->
        couch_log:notice("Replication '~s' not found", [FullRepId]),
        {error, not_found}
    end.


-spec replication_states() -> [atom()].
replication_states() ->
    ?REPLICATION_STATES.


-spec strip_url_creds(binary() | {[_]}) -> binary().
strip_url_creds(Endpoint) ->
    case couch_replicator_docs:parse_rep_db(Endpoint, [], []) of
        #httpdb{url=Url} ->
            iolist_to_binary(couch_util:url_strip_password(Url));
        LocalDb when is_binary(LocalDb) ->
            LocalDb
    end.


-spec job(binary()) -> {ok, {[_]}} | {error, not_found}.
job(JobId0) when is_binary(JobId0) ->
    JobId = couch_replicator_ids:convert(JobId0),
    {Res, _Bad} = rpc:multicall(couch_replicator_scheduler, job, [JobId]),
    case [JobInfo || {ok, JobInfo} <- Res] of
        [JobInfo| _] ->
            {ok, JobInfo};
        [] ->
            {error, not_found}
    end.


-spec restart_job(binary() | list() | rep_id()) ->
    {ok, {[_]}} | {error, not_found}.
restart_job(JobId0) ->
    JobId = couch_replicator_ids:convert(JobId0),
    {Res, _} = rpc:multicall(couch_replicator_scheduler, restart_job, [JobId]),
    case [JobInfo || {ok, JobInfo} <- Res] of
        [JobInfo| _] ->
            {ok, JobInfo};
        [] ->
            {error, not_found}
    end.


-spec active_doc(binary(), binary()) -> {ok, {[_]}} | {error, not_found}.
active_doc(DbName, DocId) ->
    try
        Shards = mem3:shards(DbName),
        Live = [node() | nodes()],
        Nodes = lists:usort([N || #shard{node=N} <- Shards,
            lists:member(N, Live)]),
        Owner = mem3:owner(DbName, DocId, Nodes),
        case active_doc_rpc(DbName, DocId, [Owner]) of
            {ok, DocInfo} ->
                {ok, DocInfo};
            {error, not_found} ->
                active_doc_rpc(DbName, DocId, Nodes -- [Owner])
        end
    catch
        % Might be a local database
        error:database_does_not_exist ->
            active_doc_rpc(DbName, DocId, [node()])
    end.


-spec active_doc_rpc(binary(), binary(), [node()]) ->
    {ok, {[_]}} | {error, not_found}.
active_doc_rpc(_DbName, _DocId, []) ->
    {error, not_found};
active_doc_rpc(DbName, DocId, [Node]) when Node =:= node() ->
    couch_replicator_doc_processor:doc(DbName, DocId);
active_doc_rpc(DbName, DocId, Nodes) ->
    {Res, _Bad} = rpc:multicall(Nodes, couch_replicator_doc_processor, doc,
        [DbName, DocId]),
    case [DocInfo || {ok, DocInfo} <- Res] of
        [DocInfo | _] ->
            {ok, DocInfo};
        [] ->
            {error, not_found}
    end.


-spec doc(binary(), binary(), any()) -> {ok, {[_]}} | {error, not_found}.
doc(RepDb, DocId, UserCtx) ->
    case active_doc(RepDb, DocId) of
        {ok, DocInfo} ->
            {ok, DocInfo};
        {error, not_found} ->
            doc_from_db(RepDb, DocId, UserCtx)
    end.


-spec doc_from_db(binary(), binary(), any()) -> {ok, {[_]}} | {error, not_found}.
doc_from_db(RepDb, DocId, UserCtx) ->
    case fabric:open_doc(RepDb, DocId, [UserCtx, ejson_body]) of
        {ok, Doc} ->
            {ok, info_from_doc(RepDb, couch_doc:to_json_obj(Doc, []))};
         {not_found, _Reason} ->
            {error, not_found}
    end.


-spec info_from_doc(binary(), {[_]}) -> {[_]}.
info_from_doc(RepDb, {Props}) ->
    DocId = get_value(<<"_id">>, Props),
    Source = get_value(<<"source">>, Props),
    Target = get_value(<<"target">>, Props),
    State0 = state_atom(get_value(<<"_replication_state">>, Props, null)),
    StateTime = get_value(<<"_replication_state_time">>, Props, null),
    {State1, StateInfo, ErrorCount, StartTime} = case State0 of
        completed ->
            {InfoP} = get_value(<<"_replication_stats">>, Props, {[]}),
            case lists:keytake(<<"start_time">>, 1, InfoP) of
                {value, {_, Time}, InfoP1} ->
                    {State0, {InfoP1}, 0, Time};
                false ->
                    case lists:keytake(start_time, 1, InfoP) of
                        {value, {_, Time}, InfoP1} ->
                            {State0, {InfoP1}, 0, Time};
                        false ->
                            {State0, {InfoP}, 0, null}
                        end
            end;
        failed ->
            Info = get_value(<<"_replication_state_reason">>, Props, null),
            {State0, Info, 1, StateTime};
        _OtherState ->
            {null, null, 0, null}
    end,
    {[
        {doc_id, DocId},
        {database, RepDb},
        {id, null},
        {source, strip_url_creds(Source)},
        {target, strip_url_creds(Target)},
        {state, State1},
        {error_count, ErrorCount},
        {info, StateInfo},
        {start_time, StartTime},
        {last_updated, StateTime}
     ]}.


state_atom(<<"triggered">>) ->
    triggered;  % This handles a legacy case were document wasn't converted yet
state_atom(State) when is_binary(State) ->
    erlang:binary_to_existing_atom(State, utf8);
state_atom(State) when is_atom(State) ->
    State.


-spec check_authorization(rep_id(), #user_ctx{}) -> ok | not_found.
check_authorization(RepId, #user_ctx{name = Name} = Ctx) ->
    case couch_replicator_scheduler:rep_state(RepId) of
    #rep{user_ctx = #user_ctx{name = Name}} ->
        ok;
    #rep{} ->
        couch_httpd:verify_is_server_admin(Ctx);
    nil ->
        not_found
    end.


-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

authorization_test_() ->
    {
        foreach,
        fun () -> ok end,
        fun (_) -> meck:unload() end,
        [
            t_admin_is_always_authorized(),
            t_username_must_match(),
            t_replication_not_found()
        ]
    }.


t_admin_is_always_authorized() ->
    ?_test(begin
        expect_rep_user_ctx(<<"someuser">>, <<"_admin">>),
        UserCtx = #user_ctx{name = <<"adm">>, roles = [<<"_admin">>]},
        ?assertEqual(ok, check_authorization(<<"RepId">>, UserCtx))
    end).


t_username_must_match() ->
     ?_test(begin
        expect_rep_user_ctx(<<"user">>, <<"somerole">>),
        UserCtx1 = #user_ctx{name = <<"user">>, roles = [<<"somerole">>]},
        ?assertEqual(ok, check_authorization(<<"RepId">>, UserCtx1)),
        UserCtx2 = #user_ctx{name = <<"other">>, roles = [<<"somerole">>]},
        ?assertThrow({unauthorized, _}, check_authorization(<<"RepId">>,
            UserCtx2))
    end).


t_replication_not_found() ->
     ?_test(begin
        meck:expect(couch_replicator_scheduler, rep_state, 1, nil),
        UserCtx1 = #user_ctx{name = <<"user">>, roles = [<<"somerole">>]},
        ?assertEqual(not_found, check_authorization(<<"RepId">>, UserCtx1)),
        UserCtx2 = #user_ctx{name = <<"adm">>, roles = [<<"_admin">>]},
        ?assertEqual(not_found, check_authorization(<<"RepId">>, UserCtx2))
    end).


expect_rep_user_ctx(Name, Role) ->
    meck:expect(couch_replicator_scheduler, rep_state,
        fun(_Id) ->
            UserCtx = #user_ctx{name = Name, roles = [Role]},
            #rep{user_ctx = UserCtx}
        end).


strip_url_creds_test_() ->
     {
        foreach,
        fun () -> meck:expect(config, get,
            fun(_, _, Default) -> Default end)
        end,
        fun (_) -> meck:unload() end,
        [
            t_strip_http_basic_creds(),
            t_strip_http_props_creds()
        ]
    }.


t_strip_local_db_creds() ->
    ?_test(?assertEqual(<<"localdb">>, strip_url_creds(<<"localdb">>))).


t_strip_http_basic_creds() ->
    ?_test(begin
        Url1 = <<"http://adm:pass@host/db">>,
        ?assertEqual(<<"http://adm:*****@host/db/">>, strip_url_creds(Url1)),
        Url2 = <<"https://adm:pass@host/db">>,
        ?assertEqual(<<"https://adm:*****@host/db/">>, strip_url_creds(Url2)),
        Url3 = <<"http://adm:pass@host:80/db">>,
        ?assertEqual(<<"http://adm:*****@host:80/db/">>, strip_url_creds(Url3)),
        Url4 = <<"http://adm:pass@host/db?a=b&c=d">>,
        ?assertEqual(<<"http://adm:*****@host/db?a=b&c=d">>,
            strip_url_creds(Url4))
    end).


t_strip_http_props_creds() ->
    ?_test(begin
        Props1 = {[{<<"url">>, <<"http://adm:pass@host/db">>}]},
        ?assertEqual(<<"http://adm:*****@host/db/">>, strip_url_creds(Props1)),
        Props2 = {[ {<<"url">>, <<"http://host/db">>},
            {<<"headers">>, {[{<<"Authorization">>, <<"Basic pa55">>}]}}
        ]},
        ?assertEqual(<<"http://host/db/">>, strip_url_creds(Props2))
    end).

-endif.
