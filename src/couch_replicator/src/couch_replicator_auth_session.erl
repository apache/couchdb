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

% This is the replicator session auth plugin. It implements session based
% authentication for the replicator. The only public API are the functions from
% the couch_replicator_auth behaviour. Most of the logic and state is in the
% gen_server. An instance of a gen_server could be spawned for the source and
% target endpoints of each replication jobs.
%
% The workflow is roughly this:
%
%  * On initialization, try to get a cookie in `refresh/1` If an error occurs,
%    the crash. If `_session` endpoint fails with a 404 (not found), return
%    `ignore` assuming session authentication is not support or we simply hit a
%    non-CouchDb server.
%
%  * Before each request, auth framework calls `update_headers` API function.
%    Before updating the headers and returning, check if need to refresh again.
%    The check looks `next_refresh` time. If that time is set (not `infinity`)
%    and just expired, then obtain a new cookie, then update headers and
%    return.
%
%  * After each request, auth framework calls `handle_response` function. If
%    request was successful check if a new cookie was sent by the server in the
%    `Set-Cookie` header. If it was then then that becomes the current cookie.
%
%  * If last request has an auth failure, check if request used a stale cookie
%    In this case nothing is done, and the client is told to retry. Next time
%    it updates its headers befor the request it should pick up the latest
%    cookie.
%
%  * If last request failed and cookie was the latest known cookie, schedule a
%    refresh and tell client to retry. However, if the cookie was just updated,
%    tell the client to continue such that it will handle the auth failure on
%    its own via a set of retries with exponential backoffs. This is it to
%    ensure if something goes wrong and one of the endpoints issues invalid
%    cookies, replicator won't be stuck in a busy loop refreshing them.

-module(couch_replicator_auth_session).

-behaviour(couch_replicator_auth).
-behaviour(gen_server).

-export([
    initialize/1,
    update_headers/2,
    handle_response/3,
    cleanup/1
]).

-export([
    init/1,
    terminate/2,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    code_change/3,
    format_status/2
]).

-include_lib("ibrowse/include/ibrowse.hrl").
-include_lib("couch_replicator/include/couch_replicator_api_wrap.hrl").

-type headers() :: [{string(), string()}].
-type code() :: non_neg_integer().
-type time_sec() :: non_neg_integer().
-type age() :: time_sec() | undefined.

-define(MIN_UPDATE_INTERVAL_SEC, 5).
-define(DEFAULT_REFRESH_INTERVAL_SEC, 550).

-record(state, {
    epoch = 0 :: non_neg_integer(),
    cookie :: string() | undefined,
    user :: string() | undefined,
    pass :: string() | undefined,
    httpdb_timeout :: integer(),
    httpdb_pool :: pid(),
    httpdb_ibrowse_options = [] :: list(),
    session_url :: string(),
    next_refresh = infinity :: infinity | non_neg_integer(),
    refresh_tstamp = 0 :: non_neg_integer(),
    require_valid_user = false :: boolean()
}).

% Behavior API callbacks

-spec initialize(#httpdb{}) ->
    {ok, #httpdb{}, term()} | {error, term()} | ignore.
initialize(#httpdb{} = HttpDb) ->
    case init_state(HttpDb) of
        {ok, HttpDb1, State} ->
            {ok, Pid} = gen_server:start_link(?MODULE, [State], []),
            Epoch = State#state.epoch,
            Timeout = State#state.httpdb_timeout,
            {ok, HttpDb1, {Pid, Epoch, Timeout}};
        {error, Error} ->
            {error, Error};
        ignore ->
            ignore
    end.

-spec update_headers(term(), headers()) -> {headers(), term()}.
update_headers({Pid, Epoch, Timeout}, Headers) ->
    Args = {update_headers, Headers, Epoch},
    {Headers1, Epoch1} = gen_server:call(Pid, Args, Timeout * 10),
    {Headers1, {Pid, Epoch1, Timeout}}.

-spec handle_response(term(), code(), headers()) ->
    {continue | retry, term()}.
handle_response({Pid, Epoch, Timeout}, Code, Headers) ->
    Args = {handle_response, Code, Headers, Epoch},
    {Retry, Epoch1} = gen_server:call(Pid, Args, Timeout * 10),
    {Retry, {Pid, Epoch1, Timeout}}.

-spec cleanup(term()) -> ok.
cleanup({Pid, _Epoch, Timeout}) ->
    gen_server:call(Pid, stop, Timeout * 10).

%% gen_server functions

init([#state{} = State]) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

handle_call({update_headers, Headers, _Epoch}, _From, State) ->
    case maybe_refresh(State) of
        {ok, State1} ->
            Cookie = "AuthSession=" ++ State1#state.cookie,
            Headers1 = [{"Cookie", Cookie} | Headers],
            {reply, {Headers1, State1#state.epoch}, State1};
        {error, Error} ->
            LogMsg = "~p: Stopping session auth plugin because of error ~p",
            couch_log:error(LogMsg, [?MODULE, Error]),
            {stop, Error, State}
    end;
handle_call({handle_response, Code, Headers, Epoch}, _From, State) ->
    {Retry, State1} = process_response(Code, Headers, Epoch, State),
    {reply, {Retry, State1#state.epoch}, State1};
handle_call(stop, _From, State) ->
    {stop, normal, ok, State}.

handle_cast(Msg, State) ->
    couch_log:error("~p: Received un-expected cast ~p", [?MODULE, Msg]),
    {noreply, State}.

handle_info(Msg, State) ->
    couch_log:error("~p : Received un-expected message ~p", [?MODULE, Msg]),
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

format_status(_Opt, [_PDict, State]) ->
    [
        {epoch, State#state.epoch},
        {user, State#state.user},
        {session_url, State#state.session_url},
        {refresh_tstamp, State#state.refresh_tstamp}
    ].

%% Private helper functions

-spec init_state(#httpdb{}) ->
    {ok, #httpdb{}, #state{}} | {error, term()} | ignore.
init_state(#httpdb{} = HttpDb) ->
    case extract_creds(HttpDb) of
        {ok, User, Pass, HttpDb1} ->
            State = #state{
                user = User,
                pass = Pass,
                session_url = get_session_url(HttpDb1#httpdb.url),
                httpdb_pool = HttpDb1#httpdb.httpc_pool,
                httpdb_timeout = HttpDb1#httpdb.timeout,
                httpdb_ibrowse_options = HttpDb1#httpdb.ibrowse_options
            },
            case refresh(State) of
                {ok, State1} ->
                    {ok, HttpDb1, State1};
                {error, {session_not_supported, _, _}} ->
                    ignore;
                {error, {session_requires_valid_user, _, _}} ->
                    % If endpoint requires basic auth for _session then try
                    % to refresh again with basic auth creds, then remember
                    % this fact in the state for all subsequent requests to
                    % _session endpoint
                    case refresh(State#state{require_valid_user = true}) of
                        {ok, State1} ->
                            {ok, HttpDb1, State1};
                        {error, {session_not_supported, _, _}} ->
                            ignore;
                        {error, Error} ->
                            {error, Error}
                    end;
                {error, Error} ->
                    {error, Error}
            end;
        {error, missing_credentials} ->
            ignore;
        {error, Error} ->
            {error, Error}
    end.

-spec extract_creds(#httpdb{}) ->
    {ok, string(), string(), #httpdb{}} | {error, term()}.
extract_creds(#httpdb{} = HttpDb) ->
    case couch_replicator_utils:get_basic_auth_creds(HttpDb) of
        {undefined, undefined} ->
            % Return error. Session plugin should ignore this replication
            % endpoint as there are no valid creds which can be used
            {error, missing_credentials};
        {User, Pass} when is_list(User), is_list(Pass) ->
            HttpDb1 = couch_replicator_utils:remove_basic_auth_creds(HttpDb),
            {ok, User, Pass, HttpDb1}
    end.

-spec process_response(
    non_neg_integer(),
    headers(),
    non_neg_integer(),
    #state{}
) -> {retry | continue, #state{}}.
process_response(403, _Headers, Epoch, State) ->
    process_auth_failure(Epoch, State);
process_response(401, _Headers, Epoch, State) ->
    process_auth_failure(Epoch, State);
process_response(Code, Headers, _Epoch, State) when Code >= 200, Code < 300 ->
    % If server noticed cookie is about to time out it can send a new cookie in
    % the response headers. Take advantage of that and refresh the cookie.
    State1 =
        case maybe_update_cookie(Headers, State) of
            {ok, UpdatedState} ->
                UpdatedState;
            {error, cookie_not_found} ->
                State;
            {error, Other} ->
                LogMsg = "~p : Could not parse cookie from response headers ~p",
                couch_log:error(LogMsg, [?MODULE, Other]),
                State
        end,
    {continue, State1};
process_response(_Code, _Headers, _Epoch, State) ->
    {continue, State}.

-spec process_auth_failure(non_neg_integer(), #state{}) ->
    {retry | continue, #state{}}.
process_auth_failure(Epoch, #state{epoch = StateEpoch} = State) when
    StateEpoch > Epoch
->
    % This request used an outdated cookie, tell it to immediately retry
    % and it will pick up the current cookie when its headers are updated
    {retry, State};
process_auth_failure(Epoch, #state{epoch = Epoch} = State) ->
    MinInterval = min_update_interval(),
    case cookie_age_sec(State, now_sec()) of
        AgeSec when AgeSec < MinInterval ->
            % A recently acquired cookie failed. Schedule a refresh and
            % return `continue` to let httpc's retry apply a backoff
            {continue, schedule_refresh(now_sec() + MinInterval, State)};
        _AgeSec ->
            % Current cookie failed auth. Schedule refresh and ask
            % httpc to retry the request.
            {retry, schedule_refresh(now_sec(), State)}
    end.

-spec get_session_url(string()) -> string().
get_session_url(Url) ->
    #url{
        protocol = Proto,
        host = Host,
        port = Port
    } = ibrowse_lib:parse_url(Url),
    WithPort = lists:concat([Proto, "://", Host, ":", Port]),
    case lists:prefix(WithPort, Url) of
        true ->
            % Explicit port specified in the original url
            WithPort ++ "/_session";
        false ->
            % Implicit proto default port was used
            lists:concat([Proto, "://", Host, "/_session"])
    end.

-spec schedule_refresh(non_neg_integer(), #state{}) -> #state{}.
schedule_refresh(T, #state{next_refresh = Tc} = State) when T < Tc ->
    State#state{next_refresh = T};
schedule_refresh(_, #state{} = State) ->
    State.

-spec maybe_refresh(#state{}) -> {ok, #state{}} | {error, term()}.
maybe_refresh(#state{next_refresh = T} = State) ->
    case now_sec() >= T of
        true ->
            refresh(State#state{next_refresh = infinity});
        false ->
            {ok, State}
    end.

-spec refresh(#state{}) -> {ok, #state{}} | {error, term()}.
refresh(#state{session_url = Url, user = User, pass = Pass} = State) ->
    Body = mochiweb_util:urlencode([{name, User}, {password, Pass}]),
    Headers0 = [{"Content-Type", "application/x-www-form-urlencoded"}],
    Headers =
        case State#state.require_valid_user of
            true ->
                Headers0 ++ [{"Authorization", "Basic " ++ b64creds(User, Pass)}];
            false ->
                Headers0
        end,
    Result = http_request(State, Url, Headers, post, Body),
    http_response(Result, State).

-spec http_request(#state{}, string(), headers(), atom(), iolist()) ->
    {ok, string(), headers(), binary()} | {error, term()}.
http_request(#state{httpdb_pool = Pool} = State, Url, Headers, Method, Body) ->
    Timeout = State#state.httpdb_timeout,
    Opts = [
        {response_format, binary},
        {inactivity_timeout, Timeout}
        | State#state.httpdb_ibrowse_options
    ],
    {ok, Wrk} = couch_replicator_httpc_pool:get_worker(Pool),
    try
        Result = ibrowse:send_req_direct(
            Wrk,
            Url,
            Headers,
            Method,
            Body,
            Opts,
            Timeout
        ),
        case Result of
            {ok, _, ResultHeaders, _} ->
                stop_worker_if_server_requested(ResultHeaders, Wrk);
            _Other ->
                ok
        end,
        Result
    after
        ok = couch_replicator_httpc_pool:release_worker_sync(Pool, Wrk)
    end.

-spec stop_worker_if_server_requested(headers(), pid()) -> ok.
stop_worker_if_server_requested(ResultHeaders0, Worker) ->
    ResultHeaders = mochiweb_headers:make(ResultHeaders0),
    case mochiweb_headers:get_value("Connection", ResultHeaders) of
        "close" ->
            Ref = erlang:monitor(process, Worker),
            ibrowse_http_client:stop(Worker),
            receive
                {'DOWN', Ref, _, _, _} ->
                    ok
            end;
        _Other ->
            ok
    end.

-spec http_response(
    {ok, string(), headers(), binary()} | {error, term()},
    #state{}
) -> {ok, #state{}} | {error, term()}.
http_response({ok, "200", Headers, _}, State) ->
    maybe_update_cookie(Headers, State);
http_response({ok, "401", Headers0, _}, #state{
    session_url = Url,
    user = User
}) ->
    Headers = mochiweb_headers:make(Headers0),
    case mochiweb_headers:get_value("WWW-Authenticate", Headers) of
        undefined ->
            {error, {session_request_unauthorized, Url, User}};
        _SomeValue ->
            {error, {session_requires_valid_user, Url, User}}
    end;
http_response({ok, "403", _, _}, #state{session_url = Url, user = User}) ->
    {error, {session_request_forbidden, Url, User}};
http_response({ok, "404", _, _}, #state{session_url = Url, user = User}) ->
    {error, {session_not_supported, Url, User}};
http_response({ok, Code, _, _}, #state{session_url = Url, user = User}) ->
    {error, {session_unexpected_result, Code, Url, User}};
http_response({error, Error}, #state{session_url = Url, user = User}) ->
    {error, {session_request_failed, Url, User, Error}}.

-spec parse_cookie(list()) -> {ok, age(), string()} | {error, term()}.
parse_cookie(Headers0) ->
    Headers = mochiweb_headers:make(Headers0),
    case mochiweb_headers:get_value("Set-Cookie", Headers) of
        undefined ->
            {error, cookie_not_found};
        CookieHeader ->
            CookieKVs = mochiweb_cookies:parse_cookie(CookieHeader),
            CaseInsKVs = mochiweb_headers:make(CookieKVs),
            case mochiweb_headers:get_value("AuthSession", CaseInsKVs) of
                undefined ->
                    {error, cookie_format_invalid};
                Cookie ->
                    MaxAge = parse_max_age(CaseInsKVs),
                    {ok, MaxAge, Cookie}
            end
    end.

-spec parse_max_age(list()) -> age().
parse_max_age(CaseInsKVs) ->
    case mochiweb_headers:get_value("Max-Age", CaseInsKVs) of
        String when is_list(String) ->
            try list_to_integer(String) of
                MaxAge when MaxAge >= 0 ->
                    MaxAge;
                _ ->
                    undefined
            catch
                error:badarg ->
                    undefined
            end;
        _ ->
            undefined
    end.

-spec maybe_update_cookie(headers(), #state{}) ->
    {ok, string()} | {error, term()}.
maybe_update_cookie(ResponseHeaders, State) ->
    case parse_cookie(ResponseHeaders) of
        {ok, MaxAge, Cookie} ->
            {ok, update_cookie(State, Cookie, now_sec(), MaxAge)};
        {error, Error} ->
            {error, Error}
    end.

-spec update_cookie(#state{}, string(), time_sec(), age()) -> #state{}.
update_cookie(#state{cookie = Cookie} = State, Cookie, _, _) ->
    State;
update_cookie(#state{epoch = Epoch} = State, Cookie, NowSec, MaxAge) ->
    NextRefresh = next_refresh(NowSec, MaxAge, refresh_interval()),
    NewState = State#state{
        epoch = Epoch + 1,
        cookie = Cookie,
        refresh_tstamp = NowSec
    },
    schedule_refresh(NextRefresh, NewState).

-spec next_refresh(time_sec(), age(), time_sec()) -> time_sec().
next_refresh(NowSec, undefined, RefreshInterval) ->
    NowSec + RefreshInterval;
next_refresh(NowSec, MaxAge, _) when is_integer(MaxAge) ->
    % Apply a fudge factor to account for delays in receving the cookie
    % and / or time adjustments happening over a longer period of time
    NowSec + trunc(MaxAge * 0.9).

-spec cookie_age_sec(#state{}, time_sec()) -> time_sec().
cookie_age_sec(#state{refresh_tstamp = RefreshTs}, Now) ->
    max(0, Now - RefreshTs).

-spec now_sec() -> time_sec().
now_sec() ->
    {Mega, Sec, _Micro} = os:timestamp(),
    Mega * 1000000 + Sec.

-spec min_update_interval() -> time_sec().
min_update_interval() ->
    config:get_integer(
        "replicator",
        "session_min_update_interval",
        ?MIN_UPDATE_INTERVAL_SEC
    ).

-spec refresh_interval() -> integer().
refresh_interval() ->
    config:get_integer(
        "replicator",
        "session_refresh_interval_sec",
        ?DEFAULT_REFRESH_INTERVAL_SEC
    ).

-spec b64creds(string(), string()) -> string().
b64creds(User, Pass) ->
    base64:encode_to_string(User ++ ":" ++ Pass).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

get_session_url_test_() ->
    [
        ?_assertEqual(SessionUrl, get_session_url(Url))
     || {Url, SessionUrl} <- [
            {"http://host/db", "http://host/_session"},
            {"http://127.0.0.1/db", "http://127.0.0.1/_session"},
            {"http://host/x/y/z", "http://host/_session"},
            {"http://host:5984/db", "http://host:5984/_session"},
            {"https://host/db?q=1", "https://host/_session"}
        ]
    ].

extract_creds_success_test() ->
    HttpDb = #httpdb{
        auth_props = [
            {<<"basic">>,
                {[
                    {<<"username">>, <<"u2">>},
                    {<<"password">>, <<"p2">>}
                ]}}
        ]
    },
    ?assertEqual({ok, "u2", "p2", #httpdb{}}, extract_creds(HttpDb)),
    ?assertEqual({error, missing_credentials}, extract_creds(#httpdb{})).

cookie_update_test_() ->
    {
        setup,
        fun setup_all/0,
        fun teardown_all/1,
        {
            foreach,
            fun setup/0,
            fun teardown/1,
            [
                t_do_refresh_without_max_age(),
                t_do_refresh_with_max_age(),
                t_dont_refresh(),
                t_process_auth_failure(),
                t_process_auth_failure_stale_epoch(),
                t_process_auth_failure_too_frequent(),
                t_process_ok_update_cookie(),
                t_process_ok_no_cookie(),
                t_init_state_fails_on_401(),
                t_init_state_401_with_require_valid_user(),
                t_init_state_404(),
                t_init_state_no_creds(),
                t_init_state_http_error()
            ]
        }
    }.

t_do_refresh_without_max_age() ->
    ?_test(begin
        State = #state{next_refresh = 0},
        {ok, State1} = maybe_refresh(State),
        ?assertMatch(#state{epoch = 1, cookie = "Abc"}, State1),
        #state{next_refresh = NextRefresh} = State1,
        RefreshInterval = NextRefresh - now_sec(),
        ?assert(540 < RefreshInterval andalso RefreshInterval =< 550)
    end).

t_do_refresh_with_max_age() ->
    ?_test(begin
        State = #state{next_refresh = 0},
        mock_http_cookie_response_with_age("Zig", "100"),
        {ok, State1} = maybe_refresh(State),
        ?assertMatch(#state{epoch = 1, cookie = "Zig"}, State1),
        #state{next_refresh = NextRefresh} = State1,
        RefreshInterval = NextRefresh - now_sec(),
        ?assert(80 < RefreshInterval andalso RefreshInterval =< 90)
    end).

t_dont_refresh() ->
    ?_test(begin
        State = #state{
            next_refresh = now_sec() + 100,
            refresh_tstamp = now_sec()
        },
        {ok, State1} = maybe_refresh(State),
        ?assertMatch(State, State1),
        State2 = #state{
            next_refresh = infinity,
            refresh_tstamp = now_sec()
        },
        {ok, State3} = maybe_refresh(State2),
        ?assertMatch(State2, State3)
    end).

t_process_auth_failure() ->
    ?_test(begin
        State = #state{epoch = 1, refresh_tstamp = 0},
        {retry, State1} = process_auth_failure(1, State),
        NextRefresh = State1#state.next_refresh,
        ?assert(NextRefresh =< now_sec())
    end).

t_process_auth_failure_stale_epoch() ->
    ?_test(begin
        State = #state{epoch = 3},
        ?assertMatch({retry, State}, process_auth_failure(2, State))
    end).

t_process_auth_failure_too_frequent() ->
    ?_test(begin
        State = #state{epoch = 4, refresh_tstamp = now_sec()},
        ?assertMatch({continue, _}, process_auth_failure(4, State))
    end).

t_process_ok_update_cookie() ->
    ?_test(begin
        Headers = [{"set-CookiE", "AuthSession=xyz; Path=/;"}, {"X", "y"}],
        Res = process_response(200, Headers, 1, #state{}),
        ?assertMatch({continue, #state{cookie = "xyz", epoch = 1}}, Res),
        State = #state{cookie = "xyz", refresh_tstamp = 42, epoch = 2},
        Res2 = process_response(200, Headers, 1, State),
        ?assertMatch({continue, #state{cookie = "xyz", epoch = 2}}, Res2)
    end).

t_process_ok_no_cookie() ->
    ?_test(begin
        Headers = [{"X", "y"}],
        State = #state{cookie = "old", epoch = 3, refresh_tstamp = 42},
        Res = process_response(200, Headers, 1, State),
        ?assertMatch({continue, State}, Res)
    end).

t_init_state_fails_on_401() ->
    ?_test(begin
        mock_http_401_response(),
        {error, Error} = init_state(httpdb("http://u:p@h")),
        SessionUrl = "http://h/_session",
        ?assertEqual({session_request_unauthorized, SessionUrl, "u"}, Error)
    end).

t_init_state_401_with_require_valid_user() ->
    ?_test(begin
        mock_http_401_response_with_require_valid_user(),
        ?assertMatch(
            {ok, #httpdb{}, #state{cookie = "Cookie"}},
            init_state(httpdb("http://u:p@h"))
        )
    end).

t_init_state_404() ->
    ?_test(begin
        mock_http_404_response(),
        ?assertEqual(ignore, init_state(httpdb("http://u:p@h")))
    end).

t_init_state_no_creds() ->
    ?_test(begin
        ?_assertEqual(ignore, init_state(httpdb("http://h")))
    end).

t_init_state_http_error() ->
    ?_test(begin
        mock_http_error_response(),
        {error, Error} = init_state(httpdb("http://u:p@h")),
        SessionUrl = "http://h/_session",
        ?assertEqual({session_request_failed, SessionUrl, "u", x}, Error)
    end).

httpdb(Url) ->
    couch_replicator_utils:normalize_basic_auth(#httpdb{url = Url}).

setup_all() ->
    meck:expect(couch_replicator_httpc_pool, get_worker, 1, {ok, worker}),
    meck:expect(couch_replicator_httpc_pool, release_worker_sync, 2, ok),
    meck:expect(config, get, fun(_, _, Default) -> Default end),
    mock_http_cookie_response("Abc"),
    ok.

teardown_all(_) ->
    meck:unload().

setup() ->
    meck:reset([
        config,
        couch_replicator_httpc_pool,
        ibrowse
    ]).

teardown(_) ->
    ok.

mock_http_cookie_response(Cookie) ->
    Resp = {ok, "200", [{"Set-Cookie", "AuthSession=" ++ Cookie}], []},
    meck:expect(ibrowse, send_req_direct, 7, Resp).

mock_http_cookie_response_with_age(Cookie, Age) ->
    AgeKV = "Max-Age=" ++ Age,
    CookieKV = "AuthSession=" ++ Cookie,
    Resp = {ok, "200", [{"Set-Cookie", CookieKV ++ ";" ++ AgeKV}], []},
    meck:expect(ibrowse, send_req_direct, 7, Resp).

mock_http_401_response() ->
    meck:expect(ibrowse, send_req_direct, 7, {ok, "401", [], []}).

mock_http_401_response_with_require_valid_user() ->
    Resp1 = {ok, "401", [{"WWW-Authenticate", "Basic realm=\"server\""}], []},
    Resp2 = {ok, "200", [{"Set-Cookie", "AuthSession=Cookie"}], []},
    meck:expect(ibrowse, send_req_direct, 7, meck:seq([Resp1, Resp2])).

mock_http_404_response() ->
    meck:expect(ibrowse, send_req_direct, 7, {ok, "404", [], []}).

mock_http_error_response() ->
    meck:expect(ibrowse, send_req_direct, 7, {error, x}).

parse_max_age_test_() ->
    [
        ?_assertEqual(R, parse_max_age(mochiweb_headers:make([{"Max-Age", A}])))
     || {A, R} <- [
            {"-10", undefined},
            {"\ufeff", undefined},
            {"*", undefined},
            {"\n1", undefined},
            {"1", 1},
            {"1 1", undefined},
            {"2", 2},
            {"100", 100},
            {"1234567890", 1234567890}
        ]
    ].

-endif.
