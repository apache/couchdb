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

-module(couch_password_server).

-behaviour(gen_server).

-include_lib("couch/include/couch_db.hrl").

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

-export([hash/0]).

-record(state, {}).

%%%===================================================================
%%% Public functions
%%%===================================================================

hash() ->
  gen_server:call(?MODULE, {hash_passwords}).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_Args) ->
  {ok, #state{}}.

handle_call({hash_passwords}, _From, _State) ->
  hash_admin_passwords();
handle_call(_Request, _From, State = #state{}) ->
  {reply, ok, State}.

handle_cast(_Request, State = #state{}) ->
  {noreply, State}.

handle_info(_Info, State = #state{}) ->
  {noreply, State}.

terminate(_Reason, _State = #state{}) ->
  ok.

code_change(_OldVsn, State = #state{}, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

hash_admin_passwords() ->
  hash_admin_passwords(true).

hash_admin_passwords(Persist) ->
  lists:foreach(
    fun({User, ClearPassword}) ->
      HashedPassword = couch_passwords:hash_admin_password(ClearPassword),
      config:set("admins", User, ?b2l(HashedPassword), Persist)
    end,
    couch_passwords:get_unhashed_admins()
  ).