%% @author Bob Ippolito <bob@mochimedia.com>
%% @copyright 2007 Mochi Media, Inc.

%% @doc MochiWeb socket server.

-module(mochiweb_socket_server).
-author('bob@mochimedia.com').
-behaviour(gen_server).

-include("internal.hrl").

-export([start/1, start_link/1, stop/1]).
-export([init/1, handle_call/3, handle_cast/2, terminate/2, code_change/3,
         handle_info/2]).
-export([get/2, set/3]).

-record(mochiweb_socket_server,
        {port,
         loop,
         name=undefined,
         %% NOTE: This is currently ignored.
         max=2048,
         ip=any,
         listen=null,
         nodelay=false,
         backlog=128,
         active_sockets=0,
         acceptor_pool_size=16,
         ssl=false,
         ssl_opts=[{ssl_imp, new}],
         acceptor_pool=sets:new(),
         profile_fun=undefined}).

-define(is_old_state(State), not is_record(State, mochiweb_socket_server)).

start_link(Options) ->
    start_server(start_link, parse_options(Options)).

start(Options) ->
    case lists:keytake(link, 1, Options) of
        {value, {_Key, false}, Options1} ->
            start_server(start, parse_options(Options1));
        _ ->
            %% TODO: https://github.com/mochi/mochiweb/issues/58
            %%   [X] Phase 1: Add new APIs (Sep 2011)
            %%   [_] Phase 2: Add deprecation warning
            %%   [_] Phase 3: Change default to {link, false} and ignore link
            %%   [_] Phase 4: Add deprecation warning for {link, _} option
            %%   [_] Phase 5: Remove support for {link, _} option
            start_link(Options)
    end.

get(Name, Property) ->
    gen_server:call(Name, {get, Property}).

set(Name, profile_fun, Fun) ->
    gen_server:cast(Name, {set, profile_fun, Fun});
set(Name, Property, _Value) ->
    error_logger:info_msg("?MODULE:set for ~p with ~p not implemented~n",
                          [Name, Property]).

stop(Name) when is_atom(Name) ->
    gen_server:cast(Name, stop);
stop(Pid) when is_pid(Pid) ->
    gen_server:cast(Pid, stop);
stop({local, Name}) ->
    stop(Name);
stop({global, Name}) ->
    stop(Name);
stop(Options) ->
    State = parse_options(Options),
    stop(State#mochiweb_socket_server.name).

%% Internal API

parse_options(State=#mochiweb_socket_server{}) ->
    State;
parse_options(Options) ->
    parse_options(Options, #mochiweb_socket_server{}).

parse_options([], State) ->
    State;
parse_options([{name, L} | Rest], State) when is_list(L) ->
    Name = {local, list_to_atom(L)},
    parse_options(Rest, State#mochiweb_socket_server{name=Name});
parse_options([{name, A} | Rest], State) when A =:= undefined ->
    parse_options(Rest, State#mochiweb_socket_server{name=A});
parse_options([{name, A} | Rest], State) when is_atom(A) ->
    Name = {local, A},
    parse_options(Rest, State#mochiweb_socket_server{name=Name});
parse_options([{name, Name} | Rest], State) ->
    parse_options(Rest, State#mochiweb_socket_server{name=Name});
parse_options([{port, L} | Rest], State) when is_list(L) ->
    Port = list_to_integer(L),
    parse_options(Rest, State#mochiweb_socket_server{port=Port});
parse_options([{port, Port} | Rest], State) ->
    parse_options(Rest, State#mochiweb_socket_server{port=Port});
parse_options([{ip, Ip} | Rest], State) ->
    ParsedIp = case Ip of
                   any ->
                       any;
                   Ip when is_tuple(Ip) ->
                       Ip;
                   Ip when is_list(Ip) ->
                       {ok, IpTuple} = inet_parse:address(Ip),
                       IpTuple
               end,
    parse_options(Rest, State#mochiweb_socket_server{ip=ParsedIp});
parse_options([{loop, Loop} | Rest], State) ->
    parse_options(Rest, State#mochiweb_socket_server{loop=Loop});
parse_options([{backlog, Backlog} | Rest], State) ->
    parse_options(Rest, State#mochiweb_socket_server{backlog=Backlog});
parse_options([{nodelay, NoDelay} | Rest], State) ->
    parse_options(Rest, State#mochiweb_socket_server{nodelay=NoDelay});
parse_options([{acceptor_pool_size, Max} | Rest], State) ->
    MaxInt = ensure_int(Max),
    parse_options(Rest,
                  State#mochiweb_socket_server{acceptor_pool_size=MaxInt});
parse_options([{max, Max} | Rest], State) ->
    error_logger:info_report([{warning, "TODO: max is currently unsupported"},
                              {max, Max}]),
    MaxInt = ensure_int(Max),
    parse_options(Rest, State#mochiweb_socket_server{max=MaxInt});
parse_options([{ssl, Ssl} | Rest], State) when is_boolean(Ssl) ->
    parse_options(Rest, State#mochiweb_socket_server{ssl=Ssl});
parse_options([{ssl_opts, SslOpts} | Rest], State) when is_list(SslOpts) ->
    SslOpts1 = [{ssl_imp, new} | proplists:delete(ssl_imp, SslOpts)],
    parse_options(Rest, State#mochiweb_socket_server{ssl_opts=SslOpts1});
parse_options([{profile_fun, ProfileFun} | Rest], State) when is_function(ProfileFun) ->
    parse_options(Rest, State#mochiweb_socket_server{profile_fun=ProfileFun}).


start_server(F, State=#mochiweb_socket_server{ssl=Ssl, name=Name}) ->
    ok = prep_ssl(Ssl),
    case Name of
        undefined ->
            gen_server:F(?MODULE, State, []);
        _ ->
            gen_server:F(Name, ?MODULE, State, [])
    end.

prep_ssl(true) ->
    ok = mochiweb:ensure_started(crypto),
    ok = mochiweb:ensure_started(public_key),
    ok = mochiweb:ensure_started(ssl);
prep_ssl(false) ->
    ok.

ensure_int(N) when is_integer(N) ->
    N;
ensure_int(S) when is_list(S) ->
    list_to_integer(S).

ipv6_supported() ->
    case (catch inet:getaddr("localhost", inet6)) of
        {ok, _Addr} ->
            true;
        {error, _} ->
            false
    end.

init(State=#mochiweb_socket_server{ip=Ip, port=Port, backlog=Backlog, nodelay=NoDelay}) ->
    process_flag(trap_exit, true),
    BaseOpts = [binary,
                {reuseaddr, true},
                {packet, 0},
                {backlog, Backlog},
                {recbuf, ?RECBUF_SIZE},
                {active, false},
                {nodelay, NoDelay}],
    Opts = case Ip of
        any ->
            case ipv6_supported() of % IPv4, and IPv6 if supported
                true -> [inet, inet6 | BaseOpts];
                _ -> BaseOpts
            end;
        {_, _, _, _} -> % IPv4
            [inet, {ip, Ip} | BaseOpts];
        {_, _, _, _, _, _, _, _} -> % IPv6
            [inet6, {ip, Ip} | BaseOpts]
    end,
    listen(Port, Opts, State).

new_acceptor_pool(Listen,
                  State=#mochiweb_socket_server{acceptor_pool=Pool,
                                                acceptor_pool_size=Size,
                                                loop=Loop}) ->
    F = fun (_, S) ->
                Pid = mochiweb_acceptor:start_link(self(), Listen, Loop),
                sets:add_element(Pid, S)
        end,
    Pool1 = lists:foldl(F, Pool, lists:seq(1, Size)),
    State#mochiweb_socket_server{acceptor_pool=Pool1}.

listen(Port, Opts, State=#mochiweb_socket_server{ssl=Ssl, ssl_opts=SslOpts}) ->
    case mochiweb_socket:listen(Ssl, Port, Opts, SslOpts) of
        {ok, Listen} ->
            {ok, ListenPort} = mochiweb_socket:port(Listen),
            {ok, new_acceptor_pool(
                   Listen,
                   State#mochiweb_socket_server{listen=Listen,
                                                port=ListenPort})};
        {error, Reason} ->
            {stop, Reason}
    end.

do_get(port, #mochiweb_socket_server{port=Port}) ->
    Port;
do_get(active_sockets, #mochiweb_socket_server{active_sockets=ActiveSockets}) ->
    ActiveSockets.


state_to_proplist(#mochiweb_socket_server{name=Name,
                                          port=Port,
                                          active_sockets=ActiveSockets}) ->
    [{name, Name}, {port, Port}, {active_sockets, ActiveSockets}].

upgrade_state(State = #mochiweb_socket_server{}) ->
    State;
upgrade_state({mochiweb_socket_server, Port, Loop, Name,
             Max, IP, Listen, NoDelay, Backlog, ActiveSockets,
             AcceptorPoolSize, SSL, SSL_opts,
             AcceptorPool}) ->
    #mochiweb_socket_server{port=Port, loop=Loop, name=Name, max=Max, ip=IP,
                            listen=Listen, nodelay=NoDelay, backlog=Backlog,
                            active_sockets=ActiveSockets,
                            acceptor_pool_size=AcceptorPoolSize,
                            ssl=SSL,
                            ssl_opts=SSL_opts,
                            acceptor_pool=AcceptorPool}.

handle_call(Req, From, State) when ?is_old_state(State) ->
    handle_call(Req, From, upgrade_state(State));
handle_call({get, Property}, _From, State) ->
    Res = do_get(Property, State),
    {reply, Res, State};
handle_call(_Message, _From, State) ->
    Res = error,
    {reply, Res, State}.


handle_cast(Req, State) when ?is_old_state(State) ->
    handle_cast(Req, upgrade_state(State));
handle_cast({accepted, Pid, Timing},
            State=#mochiweb_socket_server{active_sockets=ActiveSockets}) ->
    State1 = State#mochiweb_socket_server{active_sockets=1 + ActiveSockets},
    case State#mochiweb_socket_server.profile_fun of
        undefined ->
            undefined;
        F when is_function(F) ->
            catch F([{timing, Timing} | state_to_proplist(State1)])
    end,
    {noreply, recycle_acceptor(Pid, State1)};
handle_cast({set, profile_fun, ProfileFun}, State) ->
    State1 = case ProfileFun of
                 ProfileFun when is_function(ProfileFun); ProfileFun =:= undefined ->
                     State#mochiweb_socket_server{profile_fun=ProfileFun};
                 _ ->
                     State
             end,
    {noreply, State1};
handle_cast(stop, State) ->
    {stop, normal, State}.


terminate(Reason, State) when ?is_old_state(State) ->
    terminate(Reason, upgrade_state(State));
terminate(_Reason, #mochiweb_socket_server{listen=Listen}) ->
    mochiweb_socket:close(Listen).

code_change(_OldVsn, State, _Extra) ->
    State.

recycle_acceptor(Pid, State=#mochiweb_socket_server{
                        acceptor_pool=Pool,
                        listen=Listen,
                        loop=Loop,
                        active_sockets=ActiveSockets}) ->
    case sets:is_element(Pid, Pool) of
        true ->
            Acceptor = mochiweb_acceptor:start_link(self(), Listen, Loop),
            Pool1 = sets:add_element(Acceptor, sets:del_element(Pid, Pool)),
            State#mochiweb_socket_server{acceptor_pool=Pool1};
        false ->
            State#mochiweb_socket_server{active_sockets=ActiveSockets - 1}
    end.

handle_info(Msg, State) when ?is_old_state(State) ->
    handle_info(Msg, upgrade_state(State));
handle_info({'EXIT', Pid, normal}, State) ->
    {noreply, recycle_acceptor(Pid, State)};
handle_info({'EXIT', Pid, Reason},
            State=#mochiweb_socket_server{acceptor_pool=Pool}) ->
    case sets:is_element(Pid, Pool) of
        true ->
            %% If there was an unexpected error accepting, log and sleep.
            error_logger:error_report({?MODULE, ?LINE,
                                       {acceptor_error, Reason}}),
            timer:sleep(100);
        false ->
            ok
    end,
    {noreply, recycle_acceptor(Pid, State)};

% this is what release_handler needs to get a list of modules,
% since our supervisor modules list is set to 'dynamic'
% see sasl-2.1.9.2/src/release_handler_1.erl get_dynamic_mods
handle_info({From, Tag, get_modules}, State = #mochiweb_socket_server{name={local,Mod}}) ->
    From ! {element(2,Tag), [Mod]},
    {noreply, State};

% If for some reason we can't get the module name, send empty list to avoid release_handler timeout:
handle_info({From, Tag, get_modules}, State) ->
    error_logger:info_msg("mochiweb_socket_server replying to dynamic modules request as '[]'~n",[]),
    From ! {element(2,Tag), []},
    {noreply, State};

handle_info(Info, State) ->
    error_logger:info_report([{'INFO', Info}, {'State', State}]),
    {noreply, State}.



%%
%% Tests
%%
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

upgrade_state_test() ->
    OldState = {mochiweb_socket_server,
                port, loop, name,
                max, ip, listen,
                nodelay, backlog,
                active_sockets,
                acceptor_pool_size,
                ssl, ssl_opts, acceptor_pool},
    State = upgrade_state(OldState),
    CmpState = #mochiweb_socket_server{port=port, loop=loop,
                                       name=name, max=max, ip=ip,
                                       listen=listen, nodelay=nodelay,
                                       backlog=backlog,
                                       active_sockets=active_sockets,
                                       acceptor_pool_size=acceptor_pool_size,
                                       ssl=ssl, ssl_opts=ssl_opts,
                                       acceptor_pool=acceptor_pool,
                                       profile_fun=undefined},
    ?assertEqual(CmpState, State).

-endif.
