-module(mango_error).

-export([
    format/1,
    as_doc/1
]).


format(undefined) ->
    null;
format({bad_request, Reason}) ->
    Reason;
format({doc_update_error, Id, Error}) ->
    {_Code, _Err, Msg} = chttpd:error_info(Error),
    fmt("Error updating doc ~s :: ~s", [Id, Msg]);
format(authorization_required) ->
    <<"You must be logged in to perform this request">>;
format({authorization_failure, Reason}) ->
    fmt("Error authorizing request: ~s", [couch_util:to_binary(Reason)]);
format(_E) ->
    fmt("Unknown error: ~p", [_E]).


as_doc(Ctx) ->
    {Error, Stack} = mango_ctx:last_error(Ctx),
    Msg = format(Error),
    StackId = erlang:phash2(Stack),
    {[
        {<<"ok">>, 0},
        {<<"err">>, Msg},
        {<<"code">>, StackId}
    ]}.


fmt(Format, Args) ->
    iolist_to_binary(io_lib:format(Format, Args)).
