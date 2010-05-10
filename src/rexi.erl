-module(rexi).
-export([start/0, stop/0, restart/0]).
-export([cast/2, cast/3, kill/2]).
-export([reply/1]).
-export([async_server_call/2, async_server_call/3]).

-define(SERVER, rexi_server).

start() ->
    application:start(rexi).

stop() ->
    application:stop(rexi).

restart() ->
    stop(), start().

%% @equiv cast(Node, self(), MFA)
-spec cast(node(), mfa()) -> reference().
cast(Node, MFA) ->
    cast(Node, self(), MFA).

%% @doc Executes apply(M, F, A) on Node.
%% You might want to use this instead of rpc:cast/4 for two reasons.  First,
%% the Caller pid and the returned reference are inserted into the remote
%% process' dictionary as `rexi_from', so it has a way to communicate with you.
%% Second, the remote process is monitored. If it exits with a Reason other
%% than normal, Caller will receive a message of the form
%% `{rexi_EXIT, Ref, Reason}' where Ref is the returned reference.
-spec cast(node(), pid(), mfa()) -> reference().
cast(Node, Caller, MFA) ->
    Ref = make_ref(),
    ok = gen_server:cast({?SERVER, Node}, {doit, {Caller,Ref}, MFA}),
    Ref.

%% @doc Sends an async kill signal to the remote process associated with Ref.
%% No rexi_EXIT message will be sent.
-spec kill(node(), reference()) -> ok.
kill(Node, Ref) ->
    ok = gen_server:cast({?SERVER, Node}, {kill, Ref}).

%% @equiv async_server_call(Server, self(), Request)
-spec async_server_call(pid() | {atom(),node()}, any()) -> reference().
async_server_call(Server, Request) ->
    async_server_call(Server, self(), Request).

%% @doc Sends a properly formatted gen_server:call Request to the Server and
%% returns the reference which the Server will include in its reply.  The
%% function acts more like cast() than call() in that the server process
%% is not monitored.  Clients who want to know if the server is alive should
%% monitor it themselves before calling this function.
-spec async_server_call(pid() | {atom(),node()}, pid(), any()) -> reference().
async_server_call(Server, Caller, Request) ->
    Ref = make_ref(),
    do_send(Server, {'$gen_call', {Caller,Ref}, Request}),
    Ref.

%% @doc convenience function to reply to the original rexi Caller.
-spec reply(any()) -> any().
reply(Reply) ->
    {Caller, Ref} = get(rexi_from),
    erlang:send(Caller, {Ref,Reply}).

%% internal functions %%

% send a message as quickly as possible
do_send(Dest, Msg) ->
    case erlang:send(Dest, Msg, [noconnect]) of
    noconnect ->
        spawn(erlang, send, [Dest, Msg]);
    ok ->
        ok
    end.
