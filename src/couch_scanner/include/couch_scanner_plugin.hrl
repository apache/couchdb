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

-include_lib("couch/include/couch_db.hrl").
-include_lib("mem3/include/mem3.hrl").

-define(LOG(LVL, FMT, ARGS, META), couch_scanner_util:log(LVL, ?MODULE, FMT, ARGS, META)).
-define(INFO(FMT, ARGS, META), ?LOG(info, FMT, ARGS, META)).
-define(INFO(FMT, ARGS), ?LOG(info, FMT, ARGS, #{})).
-define(INFO(META), ?LOG(info, "", [], META)).
-define(INFO(), ?LOG(info, "", [], #{})).
-define(WARN(FMT, ARGS), ?LOG(warning, FMT, ARGS, #{})).
-define(WARN(FMT, ARGS, META), ?LOG(warning, FMT, ARGS, META)).
-define(ERR(FMT, ARGS), ?LOG(error, FMT, ARGS, #{})).
-define(ERR(FMT, ARGS, META), ?LOG(error, FMT, ARGS, META)).
-define(DEBUG(FMT, ARGS), ?LOG(debug, FMT, ARGS, #{fn => ?FUNCTION_NAME})).
-define(DEBUG(FMT, ARGS, META), ?LOG(debug, FMT, ARGS, maps:merge(META, #{fn => ?FUNCTION_NAME}))).
