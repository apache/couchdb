%%% -*-  erlang-indent-level:2  -*-
%%%-------------------------------------------------------------------
%%% File:      mock.erl
%%% @author    Cliff Moon <> []
%%% @copyright 2009 Cliff Moon
%%% @doc
%%%
%%% @end
%%%
%%% @since 2009-01-04 by Cliff Moon
%%%-------------------------------------------------------------------
-module(mock).
-author('cliff@powerset.com').

%% API
-export([mock/1, proxy_call/2, proxy_call/3, expects/4, expects/5,
         verify_and_stop/1, verify/1, stub_proxy_call/3, stop/1]).

-include_lib("eunit/include/eunit.hrl").
-include("../include/common.hrl").

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(mockstate, {old_code, module, expectations=[]}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% @spec mock(Module::atom()) -> {ok,Mock::record()} | ignore | {error,Error}
%% @doc Starts the server
%% @end
%%--------------------------------------------------------------------
mock(Module) ->
  case gen_server:start_link({local, mod_to_name(Module)}, mock, Module, []) of
    {ok, Pid} -> {ok, Pid};
    {error, Reason} -> {error, Reason}
  end.

%% @spec proxy_call(Module::atom(), Function::atom()) -> term()
%% @doc Proxies a call to the mock server for Module without arguments
%% @end
proxy_call(Module, Function) ->
  gen_server:call(mod_to_name(Module), {proxy_call, Function, {}}).

%% @spec proxy_call(Module::atom(), Function::atom(), Args::tuple()) -> term()
%% @doc Proxies a call to the mock server for Module with arguments
%% @end
proxy_call(Module, Function, Args) ->
  gen_server:call(mod_to_name(Module), {proxy_call, Function, Args}).

stub_proxy_call(Module, Function, Args) ->
  RegName = list_to_atom(lists:concat([Module, "_", Function, "_stub"])),
  Ref = make_ref(),
  RegName ! {Ref, self(), Args},
  ?debugFmt("sending {~p,~p,~p}", [Ref, self(), Args]),
  receive
    {Ref, Answer} -> Answer
  end.

%% @spec expects(Module::atom(),
%%               Function::atom(),
%%               Args::function(),
%%               Ret::function() | term() ) -> term()

%%               Times:: {at_least, integer()} | never | {no_more_than, integer()} | integer()) -> term()

%% @doc Sets the expectation that Function of Module will be called during a
%% test with Args.   Args should be a fun predicate that will return true or
%% false whether or not the argument list matches.  The argument list of the
%% function is passed in as a tuple.  Ret is either a value to return or a fun
%% of arity 2 to be evaluated in response to a proxied call.  The first argument
%% is the actual args from the call, the second is the call count starting
%% with 1.
expects(Module, Function, Args, Ret) ->
  gen_server:call(mod_to_name(Module), {expects, Function, Args, Ret, 1}).

expects(Module, Function, Args, Ret, Times) ->
  gen_server:call(mod_to_name(Module), {expects, Function, Args, Ret, Times}).

%% stub(Module, Function, Args, Ret) ->
%%   gen_server:call(mod_to_name(Module), {stub, Function, Args, Ret}).

verify_and_stop(Module) ->
  verify(Module),
  stop(Module).

verify(Module) ->
  ?assertEqual(ok, gen_server:call(mod_to_name(Module), verify)).

stop(Module) ->
  gen_server:cast(mod_to_name(Module), stop),
  timer:sleep(10).


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
init(Module) ->
  case code:get_object_code(Module) of
    {Module, Bin, Filename} ->
      case replace_code(Module) of
        ok -> {ok, #mockstate{module=Module,old_code={Module, Bin, Filename}}};
        {error, Reason} -> {stop, Reason}
      end;
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
handle_call({proxy_call, Function, Args}, _From,
            State = #mockstate{module=Mod,expectations=Expects}) ->
  case match_expectation(Function, Args, Expects) of
    {matched, ReturnTerm, NewExpects} ->
      {reply, ReturnTerm, State#mockstate{expectations=NewExpects}};
    unmatched ->
      {stop, ?fmt("got unexpected call to ~p:~p", [Mod,Function])}
  end;

handle_call({expects, Function, Args, Ret, Times}, _From,
            State = #mockstate{expectations=Expects}) ->
  {reply, ok, State#mockstate{
          expectations=add_expectation(Function, Args, Ret, Times, Expects)}};

handle_call(verify, _From, State = #mockstate{expectations=Expects,module=Mod}) ->
  ?infoFmt("verifying ~p~n", [Mod]),
  if
    length(Expects) > 0 ->
      {reply, {mismatch, format_missing_expectations(Expects, Mod)}, State};
    true -> {reply, ok, State}
  end.

%%--------------------------------------------------------------------
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% @doc Handling cast messages
%% @end
%%--------------------------------------------------------------------
handle_cast(stop, State) ->
  timer:sleep(10),
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
terminate(_Reason, #mockstate{old_code={Module, Binary, Filename}}) ->
  code:purge(Module),
  code:delete(Module),
  code:load_binary(Module, Filename, Binary),
  timer:sleep(10).

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
format_missing_expectations(Expects, Mod) ->
  format_missing_expectations(Expects, Mod, []).

format_missing_expectations([], _, Msgs) ->
  lists:reverse(Msgs);

format_missing_expectations([{Function, _Args, _Ret, Times, Called}|Expects], Mod, Msgs) ->
  Msgs1 = [?fmt("expected ~p:~p to be called ~p times but was called ~p", [Mod,Function,Times,Called])|Msgs],
  format_missing_expectations(Expects, Mod, Msgs1).

add_expectation(Function, Args, Ret, Times, Expects) ->
  Expects ++ [{Function, Args, Ret, Times, 0}].

match_expectation(Function, Args, Expectations) ->
  match_expectation(Function, Args, Expectations, []).

match_expectation(_Function, _Args, [], _Rest) ->
  unmatched;

match_expectation(Function, Args, [{Function, Matcher, Ret, MaxTimes, Invoked}|Expects], Rest) ->
  case Matcher(Args) of
    true ->
      ReturnTerm = prepare_return(Args, Ret, Invoked+1),
      if
        Invoked + 1 >= MaxTimes -> {matched, ReturnTerm, lists:reverse(Rest) ++ Expects};
        true -> {matched, ReturnTerm, lists:reverse(Rest) ++ [{Function, Matcher, Ret, MaxTimes, Invoked+1}] ++ Expects}
      end;
    false -> match_expectation(Function, Args, Expects, [{Function,Matcher,Ret,MaxTimes,Invoked}|Rest])
  end;

match_expectation(Function, Args, [Expect|Expects], Rest) ->
  match_expectation(Function, Args, Expects, [Expect|Rest]).

prepare_return(Args, Ret, Invoked) when is_function(Ret) ->
  Ret(Args, Invoked);

prepare_return(_Args, Ret, _Invoked) ->
  Ret.

replace_code(Module) ->
  Info = Module:module_info(),
  Exports = get_exports(Info),
  unload_code(Module),
  NewFunctions = generate_functions(Module, Exports),
  Forms = [
    {attribute,1,module,Module},
    {attribute,2,export,Exports}
  ] ++ NewFunctions,
  case compile:forms(Forms, [binary]) of
    {ok, Module, Binary} -> case code:load_binary(Module, atom_to_list(Module) ++ ".erl", Binary) of
      {module, Module} -> ok;
      {error, Reason} -> {error, Reason}
    end;
    error -> {error, "An undefined error happened when compiling."};
    {error, Errors, Warnings} -> {error, Errors ++ Warnings}
  end.

unload_code(Module) ->
  code:purge(Module),
  code:delete(Module).

get_exports(Info) ->
  get_exports(Info, []).

get_exports(Info, Acc) ->
  case lists:keytake(exports, 1, Info) of
    {value, {exports, Exports}, ModInfo} ->
      get_exports(ModInfo, Acc ++ lists:filter(fun({module_info, _}) -> false; (_) -> true end, Exports));
    _ -> Acc
  end.

%% stub_function_loop(Fun) ->
%%   receive
%%     {Ref, Pid, Args} ->
%%       ?debugFmt("received {~p,~p,~p}", [Ref, Pid, Args]),
%%       Ret = (catch Fun(Args) ),
%%       ?debugFmt("sending {~p,~p}", [Ref,Ret]),
%%       Pid ! {Ref, Ret},
%%       stub_function_loop(Fun)
%%   end.

% Function -> {function, Lineno, Name, Arity, [Clauses]}
% Clause -> {clause, Lineno, [Variables], [Guards], [Expressions]}
% Variable -> {var, Line, Name}
%
generate_functions(Module, Exports) ->
  generate_functions(Module, Exports, []).

generate_functions(_Module, [], FunctionForms) ->
  lists:reverse(FunctionForms);

generate_functions(Module, [{Name,Arity}|Exports], FunctionForms) ->
  generate_functions(Module, Exports, [generate_function(Module, Name, Arity)|FunctionForms]).

generate_function(Module, Name, Arity) ->
  {function, 1, Name, Arity, [{clause, 1, generate_variables(Arity), [], generate_expression(mock, proxy_call, Module, Name, Arity)}]}.

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

mod_to_name(Module) ->
  list_to_atom(lists:concat([mock_, Module])).

%% replace_function(FF, Forms) ->
%%   replace_function(FF, Forms, []).

%% replace_function(FF, [], Ret) ->
%%   [FF|lists:reverse(Ret)];

%% replace_function({function,_,Name,Arity,Clauses}, [{function,Line,Name,Arity,_}|Forms], Ret) ->
%%   lists:reverse(Ret) ++ [{function,Line,Name,Arity,Clauses}|Forms];

%% replace_function(FF, [FD|Forms], Ret) ->
%%   replace_function(FF, Forms, [FD|Ret]).
