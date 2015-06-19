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

-module(couch_index_plugin).

-export([index_update/4]).

-include_lib("couch/include/couch_db.hrl").

-define(SERVICE_ID, couch_index).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

index_update(State, View, Updated, Removed) ->
    Handle = couch_epi:get_handle(?SERVICE_ID),
    Args = [State, View, Updated, Removed],
    couch_epi:apply(Handle, ?SERVICE_ID, index_update, Args, [ignore_providers]).
