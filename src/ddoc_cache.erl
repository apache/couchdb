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

-module(ddoc_cache).


-export([
    start/0,
    stop/0,
    
    open/2,
    evict/2
]).


-define(CACHE, ddoc_cache_lru).
-define(OPENER, ddoc_cache_opener).


start() ->
    application:start(ddoc_cache).


stop() ->
    application:stop(ddoc_cache).


open(DbName, validation_funs) ->
    open({DbName, validation_funs});
open(DbName, <<"_design/", _/binary>>=DDocId) when is_binary(DbName) ->
    open({DbName, DDocId});
open(DbName, DDocId) when is_binary(DDocId) ->
    open({DbName, <<"_design/", DDocId/binary>>}).


open(Key) ->
    try ets_lru:lookup_d(?CACHE, Key) of
        {ok, _} = Resp ->
            Resp;
        _ ->
            case gen_server:call(?OPENER, {open, Key}, infinity) of
                {open_ok, Resp} ->
                    Resp;
                {open_error, throw, Error} ->
                    throw(Error);
                {open_error, error, Error} ->
                    erlang:error(Error);
                {open_error, exit, Error} ->
                    exit(Error)
            end
    catch
        error:badarg ->
            recover(Key)
    end.


evict(ShardDbName, DDocIds) ->
    DbName = mem3:dbname(ShardDbName),
    gen_server:cast(?OPENER, {evict, DbName, DDocIds}).


recover({DbName, validation_funs}) ->
    {ok, DDocs} = fabric:design_docs(mem3:dbname(DbName)),
    Funs = lists:flatmap(fun(DDoc) ->
        case couch_doc:get_validate_doc_fun(DDoc) of
            nil -> [];
            Fun -> [Fun]
        end
    end, DDocs),
    {ok, Funs};
recover({DbName, DDocId}) ->
    fabric:open_doc(DbName, DDocId, [ejson_body]).
