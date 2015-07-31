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

-module(global_changes_plugin).

-export([transform_change/3]).

-include_lib("couch/include/couch_db.hrl").

-define(SERVICE_ID, global_changes).


%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

transform_change(Username, Change, Default) ->
    maybe_handle(transform_change, [Username, Change], Default).

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

maybe_handle(Func, Args, Default) ->
    Handle = couch_epi:get_handle(?SERVICE_ID),
    case couch_epi:apply(Handle, ?SERVICE_ID, Func, Args, [ignore_providers]) of
        [] ->
            apply(Default, Args);
        [Result] ->
            Result
    end.
