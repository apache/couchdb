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

-module(couch_hash).

-export([md5_hash/1, md5_hash_final/1, md5_hash_init/0, md5_hash_update/2]).

-ifdef(ERLANG_MD5).

md5_hash(Data) ->
    erlang:md5(Data).

md5_hash_final(Context) ->
    erlang:md5_final(Context).

md5_hash_init() ->
    erlang:md5_init().

md5_hash_update(Context, Data) ->
    erlang:md5_update(Context, Data).

-else.

md5_hash(Data) ->
    crypto:hash(md5, Data).

md5_hash_final(Context) ->
    crypto:hash_final(Context).

md5_hash_init() ->
    crypto:hash_init(md5).

md5_hash_update(Context, Data) ->
    crypto:hash_update(Context, Data).

-endif.
