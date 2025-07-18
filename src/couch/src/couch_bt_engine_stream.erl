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

-module(couch_bt_engine_stream).

-export([
    foldl/3,
    seek/2,
    write/2,
    finalize/1,
    to_disk_term/1
]).

foldl({_Fd, _Gen, []}, _Fun, Acc) ->
    Acc;
foldl({Fd, Gen, [{Pos, _} | Rest]}, Fun, Acc) ->
    foldl({Fd, Gen, [Pos | Rest]}, Fun, Acc);
foldl({Fd, Gen, [Bin | Rest]}, Fun, Acc) when is_binary(Bin) ->
    % We're processing the first bit of data
    % after we did a seek for a range fold.
    foldl({Fd, Gen, Rest}, Fun, Fun(Bin, Acc));
foldl({Fd, Gen, [Pos | Rest]}, Fun, Acc) when is_integer(Pos) ->
    {ok, Bin} = couch_file:pread_binary(Fd, Pos),
    foldl({Fd, Gen, Rest}, Fun, Fun(Bin, Acc)).

seek({Fd, Gen, [{Pos, Length} | Rest]}, Offset) ->
    case Length =< Offset of
        true ->
            seek({Fd, Gen, Rest}, Offset - Length);
        false ->
            seek({Fd, Gen, [Pos | Rest]}, Offset)
    end;
seek({Fd, Gen, [Pos | Rest]}, Offset) when is_integer(Pos) ->
    {ok, Bin} = couch_file:pread_binary(Fd, Pos),
    case iolist_size(Bin) =< Offset of
        true ->
            seek({Fd, Gen, Rest}, Offset - size(Bin));
        false ->
            <<_:Offset/binary, Tail/binary>> = Bin,
            {ok, {Fd, Gen, [Tail | Rest]}}
    end.

write({Fd, Gen, Written}, Data) when is_pid(Fd) ->
    {ok, Pos, _} = couch_file:append_binary(Fd, Data),
    {ok, {Fd, Gen, [{Pos, iolist_size(Data)} | Written]}}.

finalize({Fd, Gen, Written}) ->
    {ok, {Fd, Gen, lists:reverse(Written)}}.

to_disk_term({_Fd, 0, Written}) ->
    {ok, Written};
to_disk_term({_Fd, Gen, Written}) ->
    {ok, {Gen, Written}}.
