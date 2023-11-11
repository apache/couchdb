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
%
% password hashes on disk can take a long time to verify. This is by design, to
% guard against offline attacks. This module adds an in-memory cache for password
% verification to speed up verification after a successful slow verification from
% the hash stored on disk, in order not to transfer the deliberate offline attack
% protection to database requests.
%
% In memory we record a PBKDF_SHA256 derivation using a low number of iterations
% and check against this if present. Entries in couch_passwords_cache expire automatically
% and the maximum number of cached entries is configurable.

-module(couch_passwords_cache).

-define(CACHE, couch_passwords_cache_lru).

% in-memory cache needs to be fast, keep this value low.
-define(FAST_ITERATIONS, 5).

-define(SHA256_OUTPUT_LEN, 32).

% public api
-export([authenticate/4, insert/4]).

% public functions
-spec authenticate(
    AuthModule :: atom(), UserName :: binary(), Password :: binary(), Salt :: binary()
) ->
    not_found | boolean().
authenticate(AuthModule, UserName, Password, Salt) when
    is_atom(AuthModule), is_binary(UserName), is_binary(Password), is_binary(Salt)
->
    case config:get_boolean("couch_passwords_cache", "enable", true) of
        true ->
            authenticate_int(AuthModule, UserName, Password, Salt);
        false ->
            not_found
    end.

authenticate_int(AuthModule, UserName, Password, Salt) ->
    % salt in key to ensure entry is not a match if user changes their password
    % (as salt is re-randomised in that event)
    case ets_lru:lookup_d(?CACHE, {AuthModule, UserName, Salt}) of
        not_found ->
            not_found;
        {ok, ExpectedHash} ->
            ActualHash = hash(Password, Salt),
            couch_passwords:verify(ExpectedHash, ActualHash)
    end.

-spec insert(AuthModule :: atom(), UserName :: binary(), Password :: binary(), Salt :: binary()) ->
    ok.
insert(AuthModule, UserName, Password, Salt) when
    is_atom(AuthModule), is_binary(UserName), is_binary(Password), is_binary(Salt)
->
    case config:get_boolean("couch_passwords_cache", "enable", true) of
        true ->
            ets_lru:insert(?CACHE, {AuthModule, UserName, Salt}, hash(Password, Salt));
        false ->
            ok
    end.

hash(Password, Salt) ->
    fast_pbkdf2:pbkdf2(sha256, Password, Salt, ?FAST_ITERATIONS, ?SHA256_OUTPUT_LEN).
