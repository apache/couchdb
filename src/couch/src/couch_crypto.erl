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

-module(couch_crypto).

-export([hash/2, hash_init/1, hash_update/3, hash_final/2]).
-export([hmac/3]).

-compile([nowarn_deprecated_function]).

hash(Alg, Data) ->
    case {Alg, erlang:function_exported(crypto, hash, 2)} of
        {_, true} ->
            crypto:hash(Alg, Data);
        {sha, false} ->
            crypto:sha(Data);
        {md5, false} ->
            crypto:md5(Data);
        {Alg, false} ->
            throw({unsupported, Alg})
    end.

hash_init(Alg) ->
    case {Alg, erlang:function_exported(crypto, hash_init, 1)} of
        {_, true} ->
            crypto:hash_init(Alg);
        {sha, false} ->
            crypto:sha_init();
        {md5, false} ->
            crypto:md5_init();
        {Alg, false} ->
            throw({unsupported, Alg})
    end.


hash_update(Alg, Context, Data) ->
    case {Alg, erlang:function_exported(crypto, hash_update, 2)} of
        {_, true} ->
            crypto:hash_update(Context, Data);
        {sha, false} ->
            crypto:sha_update(Context, Data);
        {md5, false} ->
            crypto:md5_update(Context, Data);
        {Alg, false} ->
            throw({unsupported, Alg})
    end.


hash_final(Alg, Context) ->
    case {Alg, erlang:function_exported(crypto, hash_final, 1)} of
        {_, true} ->
            crypto:hash_final(Context);
        {sha, false} ->
            crypto:sha_final(Context);
        {md5, false} ->
            crypto:md5_final(Context);
        {Alg, false} ->
            throw({unsupported, Alg})
    end.


hmac(Alg, Key, Data) ->
    case {Alg, erlang:function_exported(crypto, hmac, 3)} of
        {_, true} ->
            crypto:hmac(Alg, Key, Data);
        {sha, false} ->
            crypto:sha_mac(Key, Data);
        {Alg, false} ->
            throw({unsupported, Alg})
    end.
