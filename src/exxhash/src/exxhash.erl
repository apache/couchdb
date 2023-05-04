% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License. You may obtain a copy of
% the License at
%
% http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
% License for the specific language governing permissions and limitations under
% the License.

-module(exxhash).

-on_load(init/0).

-export([
    xxhash128/1
]).

-nifs([
    xxhash128_plain_nif/1,
    xxhash128_dirty_nif/1
]).

% It takes about 50 usec to hash a 1MB block
-define(DIRTY_THRESHOLD, 1048576).

xxhash128(Bin) when is_binary(Bin) ->
    case byte_size(Bin) =< ?DIRTY_THRESHOLD of
        true ->
            {H, L} = xxhash128_plain_nif(Bin),
            <<H:64, L:64>>;
        false ->
            {H, L} = xxhash128_dirty_nif(Bin),
            <<H:64, L:64>>
    end.

init() ->
    PrivDir =
        case code:priv_dir(?MODULE) of
            {error, _} ->
                EbinDir = filename:dirname(code:which(?MODULE)),
                AppPath = filename:dirname(EbinDir),
                filename:join(AppPath, "priv");
            Path ->
                Path
        end,
    erlang:load_nif(filename:join(PrivDir, "exxhash"), 0).

xxhash128_plain_nif(_) ->
    erlang:nif_error(nif_not_loaded).

xxhash128_dirty_nif(_) ->
    erlang:nif_error(nif_not_loaded).
