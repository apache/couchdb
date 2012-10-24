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

-module(couch_config_event).
-behaviour(gen_event).
-export([init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2, 
    code_change/3]).

-export([start_link/0, register/2]).

start_link() ->
    gen_event:start_link({local, ?MODULE}).

register(Fun, Pid) ->
    gen_event:add_handler(?MODULE, {?MODULE, Fun}, [Fun, Pid]).

init([Fun, Pid]) ->
    Ref = erlang:monitor(process, Pid),
    {ok, {Fun, Ref}}.

handle_event({config_change,Sec,_,_,_}, {F,_}=St) when is_function(F,1) ->
    catch F(Sec),
    {ok, St};
handle_event({config_change,Sec,K,_,_}, {F,_}=St) when is_function(F,2) ->
    catch F(Sec,K),
    {ok, St};
handle_event({config_change,Sec,K,V,_}, {F,_}=St) when is_function(F,3) ->
    catch F(Sec,K,V),
    {ok, St};
handle_event({config_change,Sec,K,V,Write}, {F,_}=St) when is_function(F,4) ->
    catch F(Sec,K,V,Write),
    {ok, St}.

handle_call(_Request, St) ->
    {ok, ok, St}.

handle_info({'DOWN', Ref, _, _, _}, {_, Ref}) ->
    remove_handler;
handle_info(_Info, St) ->
    {ok, St}.

terminate(Reason, St) ->
    ok.

code_change(_OldVsn, St, _Extra) ->
    {ok, St}.
