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

-module(couch_replicator_auth_noop).

-behavior(couch_replicator_auth).

-export([
    initialize/1,
    update_headers/2,
    handle_response/3,
    cleanup/1
]).

-include_lib("couch_replicator/include/couch_replicator_api_wrap.hrl").

-type headers() :: [{string(), string()}].
-type code() :: non_neg_integer().

-spec initialize(#httpdb{}) -> {ok, #httpdb{}, term()} | ignore.
initialize(#httpdb{} = HttpDb) ->
    {ok, HttpDb, nil}.

-spec update_headers(term(), headers()) -> {headers(), term()}.
update_headers(Context, Headers) ->
    {Headers, Context}.

-spec handle_response(term(), code(), headers()) ->
    {continue | retry, term()}.
handle_response(Context, _Code, _Headers) ->
    {continue, Context}.

-spec cleanup(term()) -> ok.
cleanup(_Context) ->
    ok.
