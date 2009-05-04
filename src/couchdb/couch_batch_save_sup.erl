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

-module(couch_batch_save_sup).

-behaviour(supervisor).

-export([start_link/0,init/1]).

start_link() ->
    supervisor:start_link({local, couch_batch_save_sup},
        couch_batch_save_sup, []).

init([]) ->
    Self = self(),
    ok = couch_config:register(
        fun("couchdb", _) ->
            exit(Self, reload_config)
        end),

    BatchSize = list_to_integer(couch_config:get("couchdb", 
        "batch_save_size","1000")),
    BatchInterval = list_to_integer(couch_config:get("couchdb", 
        "batch_save_interval","1000")),

    Batch = {batch, {couch_batch_save, start_link, [BatchSize, BatchInterval]},
            permanent, 1000, worker, [batch_save]},
    {ok, {{one_for_one, 10, 3600}, [Batch]}}.
