% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License. You may obtain a copy of
% the License at
%
% http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
% License for the specific language governing permissions and limitations under
% the License.


%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-

-module(dreyfus_sup).
-behaviour(supervisor).
-behaviour(config_listener).

-export([start_link/0, init/1, handle_config_change/5, handle_config_terminate/3]).

-define(DEFAULT_ENABLE_SEARCH, false).


start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).


init(_Args) ->
    Children = [
        child(config_listener_mon),
        child(dreyfus_index_manager)
    ],
    {ok, {{one_for_one, 10, 1},
        couch_epi:register_service(dreyfus_epi, Children)}}.


handle_config_change("dreyfus", "enable", _, _, _) ->
    case config:get_boolean("dreyfus", "enable", ?DEFAULT_ENABLE_SEARCH) andalso clouseau_rpc:connected() of
        true -> config:enable_feature(search);
        false -> config:disable_feature(search)
    end;
handle_config_change(_, _, _, _, Settings) ->
    {ok, Settings}.


handle_config_terminate(_Server, _Reason, _State) ->
    ok.


child(Child) when Child =:= config_listener_mon ->
    {Child, {Child, start_link,
        [?MODULE, settings()]}, permanent, 5000, worker, [Child]};
child(Child) ->
    {Child, {Child, start_link, []}, permanent, 1000, worker, [Child]}.


settings() ->
    [{enable, config:get("dreyfus", "enable", ?DEFAULT_ENABLE_SEARCH)}].
