% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License.  You may obtain a copy of
% the License at
%
%   http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.  See the
% License for the specific language governing permissions and limitations under
% the License.

-module(chttpd_sup).
-behaviour(supervisor).
-export([init/1]).

-export([start_link/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 100, Type, [I]}).

start_link(Args) ->
    supervisor:start_link({local,?MODULE}, ?MODULE, Args).

init([]) ->
    chttpd_config_listener:subscribe(),
    {ok, {{one_for_one, 3, 10}, [
        ?CHILD(chttpd, worker),
        ?CHILD(chttpd_auth_cache, worker),
        chttpd_handlers:provider(chttpd, chttpd_httpd_handlers),
        {chttpd_auth_cache_lru,
	 {ets_lru, start_link, [chttpd_auth_cache_lru, lru_opts()]},
	 permanent, 5000, worker, [ets_lru]}
    ]}}.

lru_opts() ->
    case config:get("chttpd_auth_cache", "max_objects") of
        MxObjs when is_integer(MxObjs), MxObjs > 0 ->
            [{max_objects, MxObjs}];
        _ ->
            []
    end ++
    case config:get("chttpd_auth_cache", "max_size", "104857600") of
        MxSize when is_integer(MxSize), MxSize > 0 ->
            [{max_size, MxSize}];
        _ ->
            []
    end ++
    case config:get("chttpd_auth_cache", "max_lifetime", "600000") of
        MxLT when is_integer(MxLT), MxLT > 0 ->
            [{max_lifetime, MxLT}];
        _ ->
            []
    end.
