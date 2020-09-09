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


-module(couch_js).
-behavior(couch_eval).


-export([
    acquire_map_context/1,
    release_map_context/1,
    map_docs/2
]).


-include_lib("couch/include/couch_db.hrl").


-define(JS, <<"javascript">>).


acquire_map_context(Opts) ->
    #{
        map_funs := MapFuns,
        lib := Lib
    } = Opts,
    couch_js_query_servers:start_doc_map(?JS, MapFuns, Lib).


release_map_context(Proc) ->
    couch_js_query_servers:stop_doc_map(Proc).


map_docs(Proc, Docs) ->
    {ok, lists:map(fun(Doc) ->
        {ok, RawResults} = couch_js_query_servers:map_doc_raw(Proc, Doc),
        Results = couch_js_query_servers:raw_to_ejson(RawResults),
        Tupled = lists:map(fun(ViewResult) ->
            lists:map(fun([K, V]) -> {K, V} end, ViewResult)
        end, Results),
        {Doc#doc.id, Tupled}
    end, Docs)}.
