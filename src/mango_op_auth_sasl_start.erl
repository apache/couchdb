-module(mango_op_auth_sasl_start).

-export([
    run/2
]).


-define(AUTH_FAIL, <<"Name or password is incorrect">>).


run(Msg, Ctx) ->
    {Username, Password} = get_creds(Msg),
    case chttpd_auth:get_user(Username) of
        nil ->
            throw({authorization_failure, ?AUTH_FAIL});
        Props ->
            Salt = get_bin(<<"salt">>, Props),
            ExpectedHash = get_bin(<<"password_sha">>, Props),
            PasswordHash = hash_password(Password, Salt),
            case couch_util:verify(ExpectedHash, PasswordHash) of
                true ->
                    Roles = couch_util:get_value(<<"roles">>, Props, []),
                    NewCtx = mango_ctx:set_auth(Ctx, Username, Roles),
                    {ok, {[{<<"ok">>, 1}]}, NewCtx};
                false ->
                    throw({authorization_failure, ?AUTH_FAIL})
            end
    end.


get_bin(Key, Props) ->
    case couch_util:get_value(Key, Props) of
        Bin when is_binary(Bin) ->
            Bin;
        _ ->
            throw(invalid_user_record)
    end.


hash_password(Pass, Salt) ->
    Hex = couch_util:to_hex(crypto:sha(<<Pass/binary, Salt/binary>>)),
    list_to_binary(Hex).


get_creds(Msg) ->
    {QProps} = mango_msg:prop('query', Msg),
    Mechanism = couch_util:get_value(<<"mechanism">>, QProps),
    if Mechanism == <<"PLAIN">> -> ok; true ->
        throw({invalid_sasl_mechanism, Mechanism})
    end,
    case couch_util:get_value(<<"payload">>, QProps) of
        {PProps} ->
            case couch_util:get_value(<<"$binary">>, PProps) of
                Bin when is_binary(Bin) ->
                    try
                        split_creds(base64:decode(Bin))
                    catch _:_ ->
                        throw(invalid_sasl_start_payload)
                    end;
                _ ->
                    throw(invalid_sasl_start_payload)
            end;
        _ ->
            throw(invalid_sasl_start_payload)
    end.


split_creds(Bin) ->
    {User, Rest} = null_prefixed(Bin, 0),
    {Pass, <<>>} = null_prefixed(Rest, 0),
    {User, Pass}.


null_prefixed(<<0:8/integer, Rest/binary>>, 0) ->
    null_prefixed(Rest, 0);
null_prefixed(Bin, N) ->
    case Bin of
        <<Ret:N/binary, 0:8/integer, Rest/binary>> ->
            {Ret, <<0:8/integer, Rest/binary>>};
        <<Ret:N/binary>> ->
            {Ret, <<>>};
        _ ->
            null_prefixed(Bin, N+1)
    end.
