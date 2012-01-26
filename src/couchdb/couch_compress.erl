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

-module(couch_compress).

-export([compress/2, decompress/1, is_compressed/2]).
-export([get_compression_method/0]).

-include("couch_db.hrl").

% binaries compressed with snappy have their first byte set to this value
-define(SNAPPY_PREFIX, 1).
% Term prefixes documented at:
%      http://www.erlang.org/doc/apps/erts/erl_ext_dist.html
-define(TERM_PREFIX, 131).
-define(COMPRESSED_TERM_PREFIX, 131, 80).


get_compression_method() ->
    case couch_config:get("couchdb", "file_compression") of
    undefined ->
        ?DEFAULT_COMPRESSION;
    Method1 ->
        case string:tokens(Method1, "_") of
        [Method] ->
            list_to_existing_atom(Method);
        [Method, Level] ->
            {list_to_existing_atom(Method), list_to_integer(Level)}
        end
    end.


compress(<<?SNAPPY_PREFIX, _/binary>> = Bin, snappy) ->
    Bin;
compress(<<?SNAPPY_PREFIX, _/binary>> = Bin, Method) ->
    compress(decompress(Bin), Method);
compress(<<?TERM_PREFIX, _/binary>> = Bin, Method) ->
    compress(decompress(Bin), Method);
compress(Term, none) ->
    ?term_to_bin(Term);
compress(Term, {deflate, Level}) ->
    term_to_binary(Term, [{minor_version, 1}, {compressed, Level}]);
compress(Term, snappy) ->
    Bin = ?term_to_bin(Term),
    try
        {ok, CompressedBin} = snappy:compress(Bin),
        case byte_size(CompressedBin) < byte_size(Bin) of
        true ->
            <<?SNAPPY_PREFIX, CompressedBin/binary>>;
        false ->
            Bin
        end
    catch exit:snappy_nif_not_loaded ->
        Bin
    end.


decompress(<<?SNAPPY_PREFIX, Rest/binary>>) ->
    {ok, TermBin} = snappy:decompress(Rest),
    binary_to_term(TermBin);
decompress(<<?TERM_PREFIX, _/binary>> = Bin) ->
    binary_to_term(Bin).


is_compressed(<<?SNAPPY_PREFIX, _/binary>>, Method) ->
    Method =:= snappy;
is_compressed(<<?COMPRESSED_TERM_PREFIX, _/binary>>, {deflate, _Level}) ->
    true;
is_compressed(<<?COMPRESSED_TERM_PREFIX, _/binary>>, _Method) ->
    false;
is_compressed(<<?TERM_PREFIX, _/binary>>, Method) ->
    Method =:= none;
is_compressed(Term, _Method) when not is_binary(Term) ->
    false.

