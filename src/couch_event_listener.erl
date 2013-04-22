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

-module(couch_event_listener).
-behavior(gen_server).


-export([
    start/3,
    start/4,
    start_link/3,
    start_link/4
]).

-export([
    behaviour_info/1
]).

-export([
    init/1,
    terminate/2,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    code_change/3
]).


behaviour_info(callbacks) ->
    [
        {init,1},
        {terminate/2},
        {handle_event/2}
    ];
behaviour_info(_) ->
    undefined.


start(Mod, Args, Options) ->
    gen_server:start(?MODULE, {Mod, Args}, Options).


start(Name, Mod, Args, Options) ->
    gen_server:start(Name, ?MODULE, {Mod, Args}, Options).


start_link(Mod, Args, Options) ->
    gen_server:start_link(?MODULE, {Mod, Args}, Options).


start_link(Name, Mod, Args, Options) ->
    gen_server:start_link(Name, ?MODULE, {Mod, Args}, Options).


init({Mod, Args}) ->
    case Mod:init(Args) of
        {ok, St} ->
            {ok, {Mod, St}};
        {ok, St, Timeout} ->
            {ok, {Mod, St}, Timeout};
        {stop, Reason} ->
            {stop, Reason};
        ignore ->
            ignore;
        Else ->
            erlang:error({bad_return, Else})
    end.


terminate(Reason, {Mod, St}) ->
    Mod:terminate(Reason, St).


handle_call(Msg, From, {Mod, St}) ->
    case erlang:function_exported(Mod, handle_call, 3) of
        true ->
            case Mod:handle_call(Msg, From, St) of
                {reply, Reply, NewState} ->
                    {reply, Reply, {Mod, NewState}};
                {reply, Reply, NewState, Timeout} ->
                    {reply, Reply, {Mod, NewState}, Timeout};
                {noreply, NewState} ->
                    {noreply, {Mod, NewState}};
                {noreply, NewState, Timeout} ->
                    {noreply, {Mod, NewState}, Timeout};
                {stop, Reason, Reply, NewState} ->
                    {stop, Reason, Reply, {Mod, NewState}};
                {stop, Reason, NewState} ->
                    {stop, Reason, {Mod, NewState}};
                Else ->
                    erlang:error({bad_return, Else})
            end;
        false ->
            {stop, {invalid_call, Msg}, invalid_call, St}
    end.


handle_cast(Msg, {Mod, St}) ->
    case erlang:function_exported(Mod, handle_cast, 2) of
        true ->
            case Mod:handle_cast(Msg, St) of
                {noreply, NewState} ->
                    {noreply, {Mod, NewState}};
                {noreply, NewState, Timeout} ->
                    {noreply, {Mod, NewState}, Timeout};
                {stop, Reason, NewState} ->
                    {stop, Reason, {Mod, NewState}};
                Else ->
                    erlang:error({bad_return, Else})
            end;
        false ->
            {stop, {invalid_cast, Msg}, St}
    end.


handle_info({'$couch_event', DbName, Event}, {Mod, St}) ->
    case Mod:handle_event(DbName, Event, St) of
        {noreply, NewState} ->
            {noreply, {Mod, NewState}};
        {noreply, NewState, Timeout} ->
            {noreply, {Mod, NewState}, Timeout};
        {stop, Reason, NewState} ->
            {stop, Reason, {Mod, NewState}};
        Else ->
            erlang:error({bad_return, Else})
    end;

handle_info(Msg, {Mod, St}) ->
    case erlang:function_export(Mod, handle_info, 2) of
        true ->
            case Mod:handle_info(Msg, St) of
                {noreply, NewState} ->
                    {noreply, {Mod, NewState}};
                {noreply, NewState, Timeout} ->
                    {noreply, {Mod, NewState}, Timeout};
                {stop, Reason, NewState} ->
                    {stop, Reason, {Mod, NewState}};
                Else ->
                    erlang:error({bad_return, Else})
            end;
        false ->
            {stop, {invalid_info, Msg}, St}
    end.


code_change(OldVsn, {Mod, St}, Extra) ->
    case erlang:function_exported(Mod, code_change, 3) of
        true ->
            case Mod:code_change(OldVsn, St, Extra) of
                {ok, NewState} ->
                    {ok, {Mod, NewState}};
                {error, Reason} ->
                    {error, Reason};
                Else ->
                    erlang:error({bad_return, Else})
            end;
        false ->
            {ok, {Mod, St}}
    end.

