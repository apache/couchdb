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

-module(fabric_auto_purge).

-export([get/1, set/2]).

-include_lib("couch/include/couch_db.hrl").
-define(KEY, <<"auto_purge">>).
-define(PROPS, <<"props">>).

get(DbName) when is_binary(DbName) ->
    Props = mem3:props(DbName),
    {AutoPurgeProps} = couch_util:get_value(?KEY, Props, {[]}),
    {ok, AutoPurgeProps}.

set(DbName, AutoPurgeProps) when is_binary(DbName) ->
    {ok, #doc{} = Doc0} = mem3:get_db_doc(DbName),
    {DocProps0} = couch_doc:to_json_obj(Doc0, []),
    {Props0} = couch_util:get_value(?PROPS, DocProps0, {[]}),
    Props1 = lists:keystore(?KEY, 1, Props0, {?KEY, {AutoPurgeProps}}),
    DocProps1 = lists:keystore(?PROPS, 1, DocProps0, {?PROPS, {Props1}}),
    Doc1 = couch_doc:from_json_obj({DocProps1}),
    case mem3:update_db_doc(Doc1) of
        {ok, _NewRev} ->
            ok;
        {error, Reason} ->
            {error, Reason}
    end.
