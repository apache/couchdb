#! /usr/bin/env escript

read() ->
    case io:get_line('') of
        eof -> stop;
        Data -> mochijson2:decode(Data)
    end.

send(Data) when is_binary(Data) ->
    send(binary_to_list(Data));
send(Data) when is_list(Data) ->
    io:format(Data ++ "\n", []).

write(Data) ->
    case (catch mochijson2:encode(Data)) of
        {json_encode, Error} -> write({[{<<"error">>, Error}]});
        Json -> send(Json)
    end.

%log(Mesg) ->
%    log(Mesg, []).
%log(Mesg, Params) ->
%    io:format(standard_error, Mesg, Params).

loop(Pid) ->
    case read() of
        stop -> ok;
        Json ->
            case (catch couch_native_process:prompt(Pid, Json)) of
                {error, Reason} ->
                    ok = write({[{error, Reason}]});
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

