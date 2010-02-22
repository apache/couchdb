%%%-------------------------------------------------------------------
%%% File:      stub.erl
%%% @author    Cliff Moon <> []
%%% @copyright 2009 Cliff Moon
%%% @doc
%%%
%%% @end
%%%
%%% @since 2009-05-10 by Cliff Moon
%%%-------------------------------------------------------------------
-module(stub).
-author('cliff@powerset.com').

-behaviour(gen_server).

%% API
-export([stub/3, stub/4, proxy_call/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include_lib("eunit/include/eunit.hrl").
-include("../include/common.hrl").

-record(state, {old_code, module, stub, times}).

%%====================================================================
%% API
%%====================================================================

stub(Module, Function, Fun) ->
  stub(Module, Function, Fun, 1).

stub(Module, Function, Fun, Times) when is_function(Fun) ->
  gen_server:start({local, name(Module, Function)}, ?MODULE, [Module, Function, Fun, Times], []).

proxy_call(_, Name, Args) ->
  {Times, Reply} = gen_server:call(Name, {proxy_call, Args}),
  if
    Times =< 0 -> gen_server:cast(Name, stop);
    true -> ok
  end,
  Reply.

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
init([Module, Function, Fun, Times]) ->
  case code:get_object_code(Module) of
    {Module, Bin, Filename} ->
      ?debugMsg("stubbing"),
      stub_function(Module, Function, arity(Fun)),
      {ok, #state{module=Module,old_code={Module,Bin,Filename},times=Times,stub=Fun}};
    error -> {stop, ?fmt("Could not get object code for module ~p", [Module])}
  end.

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
handle_call({proxy_call, Args}, _From, State = #state{stub=Fun, times=Times}) ->
  Reply = apply(Fun, tuple_to_list(Args)),
  {reply, {Times-1, Reply}, State#state{times=Times-1}}.

%%--------------------------------------------------------------------
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% @doc Handling cast messages
%% @end
%%--------------------------------------------------------------------
handle_cast(stop, State) ->
  sleep:timer(10),
  {stop, normal, State}.

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
terminate(_Reason, #state{old_code={_Module,_Bin,_Filename}}) ->
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
name(Module, Function) ->
  list_to_atom(lists:concat([Module, Function, "stub"])).

stub_function(Module, Function, Arity) ->
  {_, Bin, _} = code:get_object_code(Module),
  {ok, {Module,[{abstract_code,{raw_abstract_v1,Forms}}]}} = beam_lib:chunks(Bin, [abstract_code]),
  ?debugMsg("replacing function"),
  StubbedForms = replace_function(Module, Function, Arity, Forms),
  case compile:forms(StubbedForms, [binary]) of
    {ok, Module, Binary} -> code:load_binary(Module, atom_to_list(Module) ++ ".erl", Binary);
    Other -> Other
  end.

arity(Fun) when is_function(Fun) ->
  Props = erlang:fun_info(Fun),
  proplists:get_value(arity, Props).

replace_function(Module, Function, Arity, Forms) ->
  replace_function(Module, Function, Arity, Forms, []).

replace_function(_Module, _Function, _Arity, [], Acc) ->
  lists:reverse(Acc);
replace_function(Module, Function, Arity, [{function, Line, Function, Arity, _Clauses}|Forms], Acc) ->
  lists:reverse(Acc) ++ [{function, Line, Function, Arity, [
    {clause,
      Line,
      generate_variables(Arity),
      [],
      generate_expression(stub,proxy_call,Module,name(Module,Function),Arity)}]}] ++ Forms;
replace_function(Module, Function, Arity, [Form|Forms], Acc) ->
  replace_function(Module, Function, Arity, Forms, [Form|Acc]).

generate_variables(0) -> [];
generate_variables(Arity) ->
  lists:map(fun(N) ->
      {var, 1, list_to_atom(lists:concat(['Arg', N]))}
    end, lists:seq(1, Arity)).

generate_expression(M, F, Module, Name, 0) ->
  [{call,1,{remote,1,{atom,1,M},{atom,1,F}}, [{atom,1,Module}, {atom,1,Name}]}];
generate_expression(M, F, Module, Name, Arity) ->
  [{call,1,{remote,1,{atom,1,M},{atom,1,F}}, [{atom,1,Module}, {atom,1,Name}, {tuple,1,lists:map(fun(N) ->
      {var, 1, list_to_atom(lists:concat(['Arg', N]))}
    end, lists:seq(1, Arity))}]}].
