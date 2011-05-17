%% @author Bob Ippolito <bob@mochimedia.com>
%% @copyright 2007 Mochi Media, Inc.

%% @doc MochiWeb socket server.

-module(mochiweb_socket_server).
-author('bob@mochimedia.com').
-behaviour(gen_server).

-include("internal.hrl").

-export([start/1, stop/1]).
-export([init/1, handle_call/3, handle_cast/2, terminate/2, code_change/3,
         handle_info/2]).
-export([get/2]).

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
         acceptor_pool=sets:new()}).

start(State=#mochiweb_socket_server{}) ->
    start_server(State);
start(Options) ->
    start(parse_options(Options)).

get(Name, Property) ->
    gen_server:call(Name, {get, Property}).

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
    parse_options(Rest, State#mochiweb_socket_server{ssl_opts=SslOpts1}).

start_server(State=#mochiweb_socket_server{ssl=Ssl, name=Name}) ->
    case Ssl of
        true ->
            application:start(crypto),
            application:start(public_key),
            application:start(ssl);
        false ->
            void
    end,
    case Name of
        undefined ->
            gen_server:start_link(?MODULE, State, []);
        _ ->
            gen_server:start_link(Name, ?MODULE, State, [])
    end.

ensure_int(N) when is_integer(N) ->
    N;
ensure_int(S) when is_list(S) ->
    integer_to_list(S).

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
    case listen(Port, Opts, State) of
        {stop, eacces} ->
            case Port < 1024 of
                true ->
                    case fdsrv:start() of
                        {ok, _} ->
                            case fdsrv:bind_socket(tcp, Port) of
                                {ok, Fd} ->
                                    listen(Port, [{fd, Fd} | Opts], State);
                                _ ->
                                    {stop, fdsrv_bind_failed}
                            end;
                        _ ->
                            {stop, fdsrv_start_failed}
                    end;
                false ->
                    {stop, eacces}
            end;
        Other ->
            Other
    end.

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

handle_call({get, Property}, _From, State) ->
    Res = do_get(Property, State),
    {reply, Res, State};
handle_call(_Message, _From, State) ->
    Res = error,
    {reply, Res, State}.

handle_cast({accepted, Pid, _Timing},
            State=#mochiweb_socket_server{active_sockets=ActiveSockets}) ->
    State1 = State#mochiweb_socket_server{active_sockets=1 + ActiveSockets},
    {noreply, recycle_acceptor(Pid, State1)};
handle_cast(stop, State) ->
    {stop, normal, State}.

terminate(_Reason, #mochiweb_socket_server{listen=Listen, port=Port}) ->
    mochiweb_socket:close(Listen),
    case Port < 1024 of
        true ->
            catch fdsrv:stop(),
            ok;
        false ->
            ok
    end.

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
handle_info(Info, State) ->
    error_logger:info_report([{'INFO', Info}, {'State', State}]),
    {noreply, State}.



%%
%% Tests
%%
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).
-endif.
