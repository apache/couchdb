#! /usr/bin/env escript

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

read() ->
    case io:get_line('') of
        eof -> stop;
        Data -> couch_util:json_decode(Data)
    end.

send(Data) when is_binary(Data) ->
    send(binary_to_list(Data));
send(Data) when is_list(Data) ->
    io:format(Data ++ "\n", []).

write(Data) ->
    % log("~p", [Data]),
    case (catch couch_util:json_encode(Data)) of
        % when testing, this is what prints your errors
        {json_encode, Error} -> write({[{<<"error">>, Error}]});
        Json -> send(Json)
    end.

% log(Mesg) ->
%    log(Mesg, []).
% log(Mesg, Params) ->
%    io:format(standard_error, Mesg, Params).
% jlog(Mesg) ->
%     write([<<"log">>, list_to_binary(io_lib:format("~p",[Mesg]))]).

loop(Pid) ->
    case read() of
        stop -> ok;
        Json ->
            case (catch couch_native_process:prompt(Pid, Json)) of
                {error, Reason} ->
                    ok = write([error, Reason, Reason]);
                Resp ->
                    ok = write(Resp),
                    loop(Pid)
            end
    end.

main([]) ->
    code:add_pathz("src/couchdb"),
    code:add_pathz("src/mochiweb"),
    {ok, Pid} = couch_native_process:start_link(),
    loop(Pid).

