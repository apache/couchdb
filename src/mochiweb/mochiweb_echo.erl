%% @author Bob Ippolito <bob@mochimedia.com>
%% @copyright 2007 Mochi Media, Inc.

%% @doc Simple and stupid echo server to demo mochiweb_socket_server.

-module(mochiweb_echo).
-author('bob@mochimedia.com').
-export([start/0, stop/0, loop/1]).

stop() ->
    mochiweb_socket_server:stop(?MODULE).
    
start() ->
    mochiweb_socket_server:start([{name, ?MODULE},
                                  {port, 6789},
                                  {ip, "127.0.0.1"},
                                  {max, 1},
                                  {loop, {?MODULE, loop}}]).

loop(Socket) ->
    case gen_tcp:recv(Socket, 0, 30000) of
        {ok, Data} ->
            case gen_tcp:send(Socket, Data) of
                ok ->
                    loop(Socket);
                _ ->
                    exit(normal)
            end;
        _Other ->
            exit(normal)
    end.
