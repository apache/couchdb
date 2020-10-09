% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License.  You may obtain a copy of
% the License at
%
%   http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.  See the
% License for the specific language governing permissions and limitations under
% the License.

-module(chttpd_auth_cache).
-behaviour(gen_server).
-behaviour(config_listener).

-export([start_link/0, get_user_creds/2, update_user_creds/3]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
    code_change/3]).
-export([listen_for_changes/1, changes_callback/2]).
-export([handle_config_change/5, handle_config_terminate/3]).

-include_lib("couch/include/couch_db.hrl").
-include_lib("couch/include/couch_js_functions.hrl").

-define(CACHE, chttpd_auth_cache_lru).
-define(RELISTEN_DELAY, 5000).

-record(state, {
    changes_pid,
    last_seq="0"
}).

%% public functions

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

get_user_creds(Req, UserName) when is_list(UserName) ->
    get_user_creds(Req, ?l2b(UserName));
get_user_creds(_Req, UserName) when is_binary(UserName) ->
    Resp = case couch_auth_cache:get_admin(UserName) of
    nil ->
        get_from_cache(UserName);
    Props ->
        case get_from_cache(UserName) of
        nil ->
            Props;
        UserProps when is_list(UserProps) ->
            couch_auth_cache:add_roles(Props,
                couch_util:get_value(<<"roles">>, UserProps))
        end
    end,
    maybe_validate_user_creds(Resp).

update_user_creds(_Req, UserDoc, _Ctx) ->
    {_, Ref} = spawn_monitor(fun() ->
        {ok, Db} = fabric2_db:open(dbname(), [?ADMIN_CTX]),
        case fabric2_db:update_doc(Db, UserDoc) of
            {ok, _} ->
                exit(ok);
            Else ->
                exit(Else)
        end
    end),
    receive
        {'DOWN', Ref, _, _, ok} ->
            ok;
        {'DOWN', Ref, _, _, Else} ->
            Else
    end.

get_from_cache(UserName) ->
    try ets_lru:lookup_d(?CACHE, UserName) of
        {ok, Props} ->
            couch_stats:increment_counter([couchdb, auth_cache_hits]),
            couch_log:debug("cache hit for ~s", [UserName]),
            Props;
        _ ->
            maybe_increment_auth_cache_miss(UserName),
            case load_user_from_db(UserName) of
                nil ->
                    nil;
                Props ->
                    ets_lru:insert(?CACHE, UserName, Props),
                    Props
            end
    catch
        error:badarg ->
            maybe_increment_auth_cache_miss(UserName),
            load_user_from_db(UserName)
    end.

maybe_increment_auth_cache_miss(UserName) ->
    Admins = config:get("admins"),
    case lists:keymember(?b2l(UserName), 1, Admins) of
        false ->
            couch_stats:increment_counter([couchdb, auth_cache_misses]),
            couch_log:debug("cache miss for ~s", [UserName]);
        _True ->
            ok
    end.

%% gen_server callbacks

init([]) ->
    ensure_auth_db(),
    ok = config:listen_for_changes(?MODULE, nil),
    self() ! {start_listener, 0},
    {ok, #state{}}.

handle_call(reinit_cache, _From, State) ->
    #state{
        changes_pid = Pid
    } = State,

    % The database may currently be cached. This
    % ensures that we've removed it so that the
    % system db callbacks are installed.
    fabric2_server:remove(dbname()),

    ensure_auth_db(),
    ets_lru:clear(?CACHE),
    exit(Pid, shutdown),
    self() ! {start_listener, 0},

    {reply, ok, State#state{changes_pid = undefined}};

handle_call(_Call, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'DOWN', _, _, Pid, Reason}, #state{changes_pid=Pid} = State) ->
    Seq = case Reason of
        {seq, EndSeq} ->
            EndSeq;
    {database_does_not_exist, _} ->
            couch_log:notice("~p changes listener died because the _users database does not exist. Create the database to silence this notice.", [?MODULE]),
            0;
        _ ->
            couch_log:notice("~p changes listener died ~r", [?MODULE, Reason]),
            0
    end,
    erlang:send_after(5000, self(), {start_listener, Seq}),
    {noreply, State#state{last_seq=Seq}};
handle_info({start_listener, Seq}, State) ->
    {noreply, State#state{changes_pid = spawn_changes(Seq)}};
handle_info(restart_config_listener, State) ->
    ok = config:listen_for_changes(?MODULE, nil),
    {noreply, State};
handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, #state{changes_pid = Pid}) when is_pid(Pid) ->
    exit(Pid, kill);
terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, #state{}=State, _Extra) ->
    {ok, State}.

%% private functions

spawn_changes(Since) ->
    {Pid, _} = spawn_monitor(?MODULE, listen_for_changes, [Since]),
    Pid.

listen_for_changes(Since) ->
    {ok, Db} = fabric2_db:open(dbname(), [?ADMIN_CTX]),
    ensure_auth_ddoc_exists(Db, <<"_design/_auth">>),
    CBFun = fun ?MODULE:changes_callback/2,
    Args = #changes_args{
        feed = "continuous",
        since = Since,
        heartbeat = true,
        filter = {default, main_only}
    },
    ChangesFun = chttpd_changes:handle_db_changes(Args, nil, Db),
    ChangesFun({CBFun, Since}).

changes_callback(waiting_for_updates, Acc) ->
    {ok, Acc};
changes_callback(start, Since) ->
    {ok, Since};
changes_callback({stop, EndSeq, _Pending}, _) ->
    exit({seq, EndSeq});
changes_callback({change, {Change}}, _) ->
    case couch_util:get_value(<<"id">>, Change) of
        <<"_design/", _/binary>> ->
            ok;
        DocId ->
            UserName = username(DocId),
            couch_log:debug("Invalidating cached credentials for ~s", [UserName]),
            ets_lru:remove(?CACHE, UserName)
    end,
    {ok, couch_util:get_value(seq, Change)};
changes_callback({timeout, _ResponseType}, Acc) ->
    {ok, Acc};
changes_callback({error, _}, EndSeq) ->
    exit({seq, EndSeq}).


handle_config_change("chttpd_auth", "authentication_db", _DbName, _, _) ->
    {ok, gen_server:call(?MODULE, reinit_cache, infinity)};
handle_config_change(_, _, _, _, _) ->
    {ok, nil}.

handle_config_terminate(_, stop, _) ->
    ok;
handle_config_terminate(_Server, _Reason, _State) ->
    Dst = whereis(?MODULE),
    erlang:send_after(?RELISTEN_DELAY, Dst, restart_config_listener).


load_user_from_db(UserName) ->
    {ok, Db} = fabric2_db:open(dbname(), [?ADMIN_CTX]),
    try fabric2_db:open_doc(Db, docid(UserName), [conflicts]) of
    {ok, Doc} ->
        {Props} = couch_doc:to_json_obj(Doc, []),
        Props;
    _Else ->
        couch_log:debug("no record of user ~s", [UserName]),
        nil
    catch error:database_does_not_exist ->
        nil
    end.


ensure_auth_db() ->
    try
        fabric2_db:open(dbname(), [?ADMIN_CTX])
    catch error:database_does_not_exist ->
        case fabric2_db:create(dbname(), [?ADMIN_CTX]) of
            {ok, _} -> ok;
            {error, file_exists} -> ok
        end
    end.


dbname() ->
    DbNameStr = config:get("chttpd_auth", "authentication_db", "_users"),
    iolist_to_binary(DbNameStr).

docid(UserName) ->
    <<"org.couchdb.user:", UserName/binary>>.

username(<<"org.couchdb.user:", UserName/binary>>) ->
    UserName.

ensure_auth_ddoc_exists(Db, DDocId) ->
    case fabric2_db:open_doc(Db, DDocId) of
    {not_found, _Reason} ->
        {ok, AuthDesign} = couch_auth_cache:auth_design_doc(DDocId),
        update_doc_ignoring_conflict(Db, AuthDesign);
    {ok, Doc} ->
        {Props} = couch_doc:to_json_obj(Doc, []),
        case couch_util:get_value(<<"validate_doc_update">>, Props, []) of
            ?AUTH_DB_DOC_VALIDATE_FUNCTION ->
                ok;
            _ ->
                Props1 = lists:keyreplace(<<"validate_doc_update">>, 1, Props,
                    {<<"validate_doc_update">>,
                    ?AUTH_DB_DOC_VALIDATE_FUNCTION}),
                NewDoc = couch_doc:from_json_obj({Props1}),
                update_doc_ignoring_conflict(Db, NewDoc)
        end;
    {error, Reason} ->
        couch_log:notice("Failed to ensure auth ddoc ~s/~s exists for reason: ~p", [dbname(), DDocId, Reason]),
        ok
    end,
    ok.

update_doc_ignoring_conflict(DbName, Doc) ->
    try
        fabric2_db:update_doc(DbName, Doc)
    catch
        error:conflict ->
            ok
    end.

maybe_validate_user_creds(nil) ->
    nil;
% throws if UserCreds includes a _conflicts member
% returns UserCreds otherwise
maybe_validate_user_creds(UserCreds) ->
    AllowConflictedUserDocs = config:get_boolean("chttpd_auth", "allow_conflicted_user_docs", false),
    case {couch_util:get_value(<<"_conflicts">>, UserCreds), AllowConflictedUserDocs} of
        {undefined, _} ->
            {ok, UserCreds, nil};
        {_, true} ->
            {ok, UserCreds, nil};
        {_ConflictList, false} ->
            throw({unauthorized,
                <<"User document conflicts must be resolved before the document",
                  " is used for authentication purposes.">>
            })
    end.
