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

-module(fabric2_encryption_plugin).

-export([
    get_wrapped_kek/1,
    unwrap_kek/1
]).


-define(SERVICE_ID, fabric2_encryption).


-spec get_wrapped_kek(DbName :: binary()) ->
    {ok, KEK :: binary(), WrappedKEK :: binary()} | {error, Error :: term()}.
get_wrapped_kek(DbName) ->
    Default = boom,
    maybe_handle(get_kek, [DbName], Default).


-spec unwrap_kek(WrappedKEK :: binary()) ->
    {ok, KEK :: binary(), WrappedKEK :: binary()} | {error, Error :: term()}.
unwrap_kek(WrappedKEK) ->
    Default = boom,
    maybe_handle(unwrap_kek, [WrappedKEK], Default).



maybe_handle(Func, Args, Default) ->
    Handle = couch_epi:get_handle(?SERVICE_ID),
    case couch_epi:decide(Handle, ?SERVICE_ID, Func, Args, []) of
        no_decision when is_function(Default) ->
            apply(Default, Args);
        no_decision ->
            Default;
        {decided, Result} ->
            Result
    end.
