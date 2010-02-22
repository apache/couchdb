%%%-------------------------------------------------------------------
%%% File:      mock_genserver.erl
%%% @author    Cliff Moon <> []
%%% @copyright 2009 Cliff Moon
%%% @doc
%%%
%%% @end
%%%
%%% @since 2009-01-02 by Cliff Moon
%%%-------------------------------------------------------------------
-module(mock_genserver).
-author('cliff@powerset.com').

-behaviour(gen_server).

-include_lib("eunit/include/eunit.hrl").

%% API
-export([start_link/1, stub_call/3, expects_call/3, expects_call/4, stop/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {call_stubs=[], call_expects=[], cast_expectations, info_expectations}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% @spec start_link(Reference::atom()) -> {ok,Pid} | ignore | {error,Error}
%% @doc Starts the server
%% @end
%%--------------------------------------------------------------------
start_link(Reference) ->
  gen_server:start_link(Reference, ?MODULE, [], []).

stub_call(Server, Sym, Fun) when is_function(Fun) ->
  gen_server:call(Server, {mock_stub_call, Sym, Fun}).

expects_call(Server, Args, Fun) when is_function(Fun) ->
  gen_server:call(Server, {mock_expects_call, Args, Fun}).

expects_call(Server, Args, Fun, Times) when is_function(Fun) ->
  gen_server:call(Server, {mock_expects_call, Args, Fun, Times}).

stop(Server) ->
  gen_server:call(Server, mock_stop).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% @spec init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% @doc Initiates the server
%% @end
%%--------------------------------------------------------------------
init([]) ->
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% @spec
%% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% @doc Handling call messages
%% @end
%%--------------------------------------------------------------------
handle_call({mock_stub_call, Sym, Fun}, _From, State = #state{call_stubs=Stubs}) ->
  {reply, ok, State#state{call_stubs=[{Sym, Fun}|Stubs]}};

handle_call({mock_expects_call, Args, Fun}, _From, State = #state{call_expects=Expects}) ->
  {reply, ok, State#state{call_expects=add_expectation(Args, Fun, at_least_once, Expects)}};

handle_call({mock_expects_call, Args, Fun, Times}, _From, State = #state{call_expects=Expects}) ->
  {reply, ok, State#state{call_expects=add_expectation(Args, Fun, Times, Expects)}};

handle_call(mock_stop, _From, State) ->
  {stop, normal, ok, State};

handle_call(Request, _From, State = #state{call_stubs=Stubs,call_expects=Expects}) ->
  % expectations have a higher priority
  case find_expectation(Request, Expects) of
    {found, {_, Fun, Time}, NewExpects} -> {reply, Fun(Request, Time), State#state{call_expects=NewExpects}};
    not_found -> % look for a stub
      case find_stub(Request, Stubs) of
        {found, {_, Fun}} -> {reply, Fun(Request), State};
        not_found ->
          {stop, {unexpected_call, Request}, State}
      end
  end.

%%--------------------------------------------------------------------
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% @doc Handling cast messages
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% @doc Handling all non call/cast messages
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @spec terminate(Reason, State) -> void()
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @doc Convert process state when code is changed
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------


add_expectation(Args, Fun, Times, Expects) ->
  Expects ++ [{Args, Fun, Times}].

find_expectation(Request, Expects) ->
  find_expectation(Request, Expects, []).

find_expectation(_Request, [], _Rest) ->
  not_found;

find_expectation(Request, [{Args, Fun, Times}|Expects], Rest) ->
  MatchFun = generate_match_fun(Args),
  case MatchFun(Request) of
    true ->
      if
        Times == at_least_once -> {found, {Args, Fun, Times}, lists:reverse(Rest) ++ [{Args, Fun, Times}] ++ Expects};
        Times == 1 -> {found, {Args, Fun, Times}, lists:reverse(Rest) ++ Expects};
        true -> {found, {Args, Fun, Times}, lists:reverse(Rest) ++ [{Args, Fun, Times-1}] ++ Expects}
      end;
    false -> find_expectation(Request, Expects, [{Args, Fun, Times}|Rest])
  end.

find_stub(Request, Stub) when is_tuple(Request) ->
  Sym = element(1, Request),
  find_stub(Sym, Stub);

find_stub(_Sym, []) ->
  not_found;

find_stub(Sym, _Stubs) when not is_atom(Sym) ->
  not_found;

find_stub(Sym, [{Sym, Fun}|_Stubs]) ->
  {found, {Sym, Fun}};

find_stub(Sym, [_Stub|Stubs]) ->
  find_stub(Sym, Stubs).

generate_match_fun(Args) when is_tuple(Args) ->
  generate_match_fun(tuple_to_list(Args));

generate_match_fun(Args) when not is_list(Args) ->
  generate_match_fun([Args]);

generate_match_fun(Args) when is_list(Args) ->
  Src = generate_match_fun("fun({", Args),
  {ok, Tokens, _} = erl_scan:string(Src),
  {ok, [Form]} = erl_parse:parse_exprs(Tokens),
  {value, Fun, _} = erl_eval:expr(Form, erl_eval:new_bindings()),
  Fun.

generate_match_fun(Src, []) ->
  Src ++ "}) -> true; (_) -> false end.";

% unbound atom means you don't care about an arg
generate_match_fun(Src, [unbound|Args]) ->
  if
    length(Args) > 0 -> generate_match_fun(Src ++ "_,", Args);
    true -> generate_match_fun(Src ++ "_", Args)
  end;

generate_match_fun(Src, [Bound|Args]) ->
  Term = lists:flatten(io_lib:format("~w", [Bound])),
  if
    length(Args) > 0 -> generate_match_fun(Src ++ Term ++ ",", Args);
    true -> generate_match_fun(Src ++ Term, Args)
  end.
