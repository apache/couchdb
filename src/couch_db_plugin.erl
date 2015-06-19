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

-module(couch_db_plugin).

-export([validate_dbname/2]).

-define(SERVICE_ID, couch_db).

-include_lib("couch/include/couch_db.hrl").

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

validate_dbname(DbName, Normalized) ->
    Handle = couch_epi:get_handle(?SERVICE_ID),
    %% callbacks return true only if it specifically allow the given Id
    couch_epi:any(Handle, ?SERVICE_ID, validate_dbname, [DbName, Normalized],
        [ignore_providers]).


%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
