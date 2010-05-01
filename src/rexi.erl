-module(rexi).
-export([start/0, stop/0, restart/0]).
-export([cast/2, cast/3]).
-export([reply/1]).

-define(SERVER, rexi_server).

start() ->
    application:start(rexi).

stop() ->
    application:stop(rexi).

restart() ->
    stop(), start().

-spec cast(node(), mfa()) -> {ok, reference()}.
cast(Node, MFA) ->
    cast(Node, self(), MFA).

%% @doc Executes apply(M, F, A) on Node.
%% You might want to use this instead of rpc:cast/4 for two reasons.  First,
%% the Caller pid and the returned reference are inserted into the remote
%% process' dictionary as 'rexi_from', so it has a way to communicate with you.
%% Second, the remote process is monitored. If it dies, Caller will receive a
%% message of the form `{rexi_EXIT, Ref, Reason}' where Ref is the returned 
%% reference and Reason is the exit reason.
-spec cast(node(), pid(), mfa()) -> {ok, reference()}.
cast(Node, Caller, MFA) ->
    Ref = make_ref(),
    ok = gen_server:cast({?SERVER, Node}, {doit, {Caller,Ref}, MFA}),
    {ok, Ref}.

%% @doc convenience function to reply to the original rexi Caller.
-spec reply(any()) -> any().
reply(Reply) ->
    {Caller, Ref} = get(rexi_from),
    erlang:send(Caller, {Ref,Reply}).
