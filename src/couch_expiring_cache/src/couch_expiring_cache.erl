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

-module(couch_expiring_cache).

-export([
    insert/5,
    insert/6,
    lookup/2,
    lookup/3
]).


-include_lib("couch_expiring_cache/include/couch_expiring_cache.hrl").


-spec insert(Name :: binary(), Key :: binary(), Value :: binary(),
    StaleTS :: ?TIME_UNIT(), ExpiresTS :: ?TIME_UNIT()) -> ok.
insert(Name, Key, Value, StaleTS, ExpiresTS)
        when is_binary(Name), is_binary(Key), is_binary(Value),
        is_integer(StaleTS), is_integer(ExpiresTS) ->
    insert(undefined, Name, Key, Value, StaleTS, ExpiresTS).


-spec insert(Tx :: jtx(), Name :: binary(), Key :: binary(), Value :: binary(),
    StaleTS :: ?TIME_UNIT(), ExpiresTS :: ?TIME_UNIT()) -> ok.
insert(Tx, Name, Key, Value, StaleTS, ExpiresTS)
        when is_binary(Name), is_binary(Key), is_binary(Value),
        is_integer(StaleTS), is_integer(ExpiresTS) ->
    couch_jobs_fdb:tx(couch_jobs_fdb:get_jtx(Tx), fun(JTx) ->
        couch_expiring_cache_fdb:insert(
            JTx, Name, Key, Value, StaleTS, ExpiresTS)
    end).


-spec lookup(Name :: binary(), Key :: binary()) ->
    not_found | {fresh, Val :: binary()} | {stale, Val :: binary()} | expired.
lookup(Name, Key) when is_binary(Name), is_binary(Key) ->
    lookup(undefined, Name, Key).


-spec lookup(Tx :: jtx(), Name :: binary(), Key :: binary()) ->
    not_found | {fresh, Val :: binary()} | {stale, Val :: binary()} | expired.
lookup(Tx, Name, Key) when is_binary(Name), is_binary(Key) ->
    couch_jobs_fdb:tx(couch_jobs_fdb:get_jtx(Tx), fun(JTx) ->
        couch_expiring_cache_fdb:lookup(JTx, Name, Key)
    end).
