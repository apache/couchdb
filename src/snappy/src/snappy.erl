%% Copyright 2011,  Filipe David Manana  <fdmanana@apache.org>
%% Web:  http://github.com/fdmanana/snappy-erlang-nif
%%
%% Licensed under the Apache License, Version 2.0 (the "License"); you may not
%% use this file except in compliance with the License. You may obtain a copy of
%% the License at
%%
%%  http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
%% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
%% License for the specific language governing permissions and limitations under
%% the License.

-module(snappy).

-export([compress/1, decompress/1]).
-export([uncompressed_length/1, is_valid/1]).

-on_load(init/0).


init() ->
    SoName = case code:priv_dir(?MODULE) of
    {error, bad_name} ->
        case filelib:is_dir(filename:join(["..", "priv"])) of
        true ->
            filename:join(["..", "priv", "snappy_nif"]);
        false ->
            filename:join(["priv", "snappy_nif"])
        end;
    Dir ->
        filename:join(Dir, "snappy_nif")
    end,
    (catch erlang:load_nif(SoName, 0)),
    case erlang:system_info(otp_release) of
    "R13B03" -> true;
    _ -> ok
    end.


compress(_IoList) ->
    exit(snappy_nif_not_loaded).


decompress(_IoList) ->
    exit(snappy_nif_not_loaded).


uncompressed_length(_IoList) ->
    exit(snappy_nif_not_loaded).


is_valid(_IoList) ->
    exit(snappy_nif_not_loaded).
