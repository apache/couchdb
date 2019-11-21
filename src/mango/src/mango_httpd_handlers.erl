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

-module(mango_httpd_handlers).

-export([url_handler/1, db_handler/1, design_handler/1, handler_info/3]).

url_handler(_) -> no_match.

db_handler(<<"_index">>)        -> fun mango_httpd:handle_req/2;
db_handler(<<"_explain">>)      -> fun mango_httpd:handle_req/2;
db_handler(<<"_find">>)         -> fun mango_httpd:handle_req/2;
db_handler(_) -> no_match.

design_handler(_) -> no_match.

handler_info('GET', [Db, <<"_index">>], _) ->
    {'db.mango.index.read', #{'db.name' => Db}};

handler_info('POST', [Db, <<"_index">>], _) ->
    {'db.mango.index.create', #{'db.name' => Db}};

handler_info('POST', [Db, <<"_index">>, <<"_bulk_delete">>], _) ->
    {'db.mango.index.delete', #{'db.name' => Db, multi => true}};

handler_info('DELETE', [Db, <<"_index">>, <<"_design">>, Name, Type, Idx], _) ->
    {'db.mango.index.delete', #{
        'db.name' => Db,
        'design.id' => Name,
        'index.type' => Type,
        'index.name' => Idx
    }};

handler_info(M, [Db, <<"_index">>, <<"_design/", N/binary>>, T, I], R) ->
    handler_info(M, [Db, <<"_index">>, <<"_design">>, N, T, I], R);

handler_info('POST', [Db, <<"_explain">>], _) ->
    {'db.mango.explain.execute', #{'db.name' => Db}};

handler_info('POST', [Db, <<"_find">>], _) ->
    {'db.mango.find.execute', #{'db.name' => Db}};

handler_info(_, _, _) ->
    no_match.