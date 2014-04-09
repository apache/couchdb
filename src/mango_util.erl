-module(mango_util).


-export([
    defer/3,
    do_defer/3,

    cloudant_dbname/2,
    maybe_create_db/2,
    maybe_create_db/1,

    to_lower/1,
    
    enc_dbname/1,
    dec_dbname/1,
    
    enc_hex/1,
    dec_hex/1
]).


defer(Mod, Fun, Args) ->
    {Pid, Ref} = erlang:spawn_monitor(?MODULE, do_defer, [Mod, Fun, Args]),
    receive
        {'DOWN', Ref, process, Pid, {mango_defer_ok, Value}} ->
            Value;
        {'DOWN', Ref, process, Pid, {mango_defer_throw, Value}} ->
            erlang:throw(Value);
        {'DOWN', Ref, process, Pid, {mango_defer_error, Value}} ->
            erlang:error(Value);
        {'DOWN', Ref, process, Pid, {mango_defer_exit, Value}} ->
            erlang:exit(Value)
    end.


do_defer(Mod, Fun, Args) ->
    try erlang:apply(Mod, Fun, Args) of
        Resp ->
            erlang:exit({mango_defer_ok, Resp})
    catch
        throw:Error ->
            erlang:exit({mango_defer_throw, Error});
        error:Error ->
            erlang:exit({mango_defer_error, Error});
        exit:Error ->
            erlang:exit({mango_defer_exit, Error})
    end.


cloudant_dbname(Msg, Ctx) ->
    Username = mango_ctx:username(Ctx),
    Collection = mango_msg:prop(collection, Msg),
    RawDbName = <<Username/binary, "/", Collection/binary>>,
    enc_dbname(RawDbName).


maybe_create_db(Msg, Ctx) ->
    maybe_create_db(cloudant_dbname(Msg, Ctx)).


maybe_create_db(DbName) ->
    try
        mem3:shards(DbName),
        {ok, DbName}
    catch
        error:database_does_not_exist ->        
            case mango_util:defer(fabric, create_db, [DbName]) of
                ok ->
                    {ok, DbName};
                accepted ->
                    {ok, DbName};
                Error ->
                    throw(Error)
            end
    end.


to_lower(Key) when is_binary(Key) ->
    KStr = binary_to_list(Key),
    KLower = string:to_lower(KStr),
    list_to_binary(KLower).


enc_dbname(<<>>) ->
    <<>>;
enc_dbname(<<A:8/integer, Rest/binary>>) ->
    Bytes = enc_db_byte(A),
    Tail = enc_dbname(Rest),
    <<Bytes/binary, Tail/binary>>.


enc_db_byte(N) when N >= $a, N =< $z -> <<N>>;
enc_db_byte(N) when N >= $0, N =< $9 -> <<N>>;
enc_db_byte(N) when N == $/; N == $_; N == $- -> <<N>>;
enc_db_byte(N) ->
    H = enc_hex_byte(N div 16),
    L = enc_hex_byte(N rem 16),
    <<$$, H:8/integer, L:8/integer>>.    


dec_dbname(<<>>) ->
    <<>>;
dec_dbname(<<$$, _:8/integer>>) ->
    throw(invalid_dbname_encoding);
dec_dbname(<<$$, H:8/integer, L:8/integer, Rest/binary>>) ->
    Byte = (dec_hex_byte(H) bsl 4) bor dec_hex_byte(L),
    Tail = dec_dbname(Rest),
    <<Byte:8/integer, Tail/binary>>;
dec_dbname(<<N:8/integer, Rest/binary>>) ->
    Tail = dec_dbname(Rest),
    <<N:8/integer, Tail/binary>>.


enc_hex(<<>>) ->
    <<>>;
enc_hex(<<V:8/integer, Rest/binary>>) ->
    H = enc_hex_byte(V div 16),
    L = enc_hex_byte(V rem 16),
    Tail = enc_hex(Rest),
    <<H:8/integer, L:8/integer, Tail/binary>>.


enc_hex_byte(N) when N >= 0, N < 10 -> $0 + N;
enc_hex_byte(N) when N >= 10, N < 16 -> $a + (N - 10);
enc_hex_byte(N) -> throw({invalid_hex_value, N}).


dec_hex(<<>>) ->
    <<>>;
dec_hex(<<_:8/integer>>) ->
    throw(invalid_hex_string);
dec_hex(<<H:8/integer, L:8/integer, Rest/binary>>) ->
    Byte = (dec_hex_byte(H) bsl 4) bor dec_hex_byte(L),
    Tail = dec_hex(Rest),
    <<Byte:8/integer, Tail/binary>>.


dec_hex_byte(N) when N >= $0, N =< $9 -> (N - $0);
dec_hex_byte(N) when N >= $a, N =< $f -> (N - $a) + 10;
dec_hex_byte(N) when N >= $A, N =< $F -> (N - $A) + 10;
dec_hex_byte(N) -> throw({invalid_hex_character, N}).
