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

-module(fabric2_sup).
-behaviour(supervisor).
-vsn(1).


-export([
    start_link/1
]).

-export([
    init/1
]).


start_link(Args) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, Args).


init([]) ->
    config:enable_feature(fdb),
    Flags = {one_for_one, 1, 5},
    Children = [
        {
            fabric2_server,
            {fabric2_server, start_link, []},
            permanent,
            5000,
            worker,
            [fabric2_server]
        },
        {
            fabric2_txids,
            {fabric2_txids, start_link, []},
            permanent,
            5000,
            worker,
            [fabric2_server]
        },
        {
            fabric2_index,
            {fabric2_index, start_link, []},
            permanent,
            5000,
            worker,
            [fabric2_index]
        }
    ],
    ChildrenWithEpi = couch_epi:register_service(fabric2_epi, Children),
    {ok, {Flags, ChildrenWithEpi}}.
