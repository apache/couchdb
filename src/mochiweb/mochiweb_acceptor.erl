%% @author Bob Ippolito <bob@mochimedia.com>
%% @copyright 2010 Mochi Media, Inc.

%% @doc MochiWeb acceptor.

-module(mochiweb_acceptor).
-author('bob@mochimedia.com').

-include("internal.hrl").

-export([start_link/3, init/3]).

start_link(Server, Listen, Loop) ->
    proc_lib:spawn_link(?MODULE, init, [Server, Listen, Loop]).

init(Server, Listen, Loop) ->
    T1 = now(),
    case catch mochiweb_socket:accept(Listen) of
        {ok, Socket} ->
            gen_server:cast(Server, {accepted, self(), timer:now_diff(now(), T1)}),
            call_loop(Loop, Socket);
        {error, closed} ->
            exit(normal);
        {error, timeout} ->
            init(Server, Listen, Loop);
        {error, esslaccept} ->
            exit(normal);
        Other ->
            error_logger:error_report(
              [{application, mochiweb},
               "Accept failed error",
               lists:flatten(io_lib:format("~p", [Other]))]),
            exit({error, accept_failed})
    end.

call_loop({M, F}, Socket) ->
    M:F(Socket);
call_loop({M, F, [A1]}, Socket) ->
    M:F(Socket, A1);
call_loop({M, F, A}, Socket) ->
    erlang:apply(M, F, [Socket | A]);
call_loop(Loop, Socket) ->
    Loop(Socket).

%%
%% Tests
%%
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.
