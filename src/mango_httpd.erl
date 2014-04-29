-module(mango_httpd).


-export([
    handle_query_req/2,
    format_error/1
]).

-export([
    send_data/2
]).


-include_lib("couch/include/couch_db.hrl").
-include("mango.hrl").


handle_query_req(#httpd{method='POST'}=Req, Db0) ->
    Db = set_user_ctx(Req, Db0),
    couch_httpd:validate_ctype(Req, "application/json"),
    {ok, Actions} = try
        get_body_actions(Db, Req)
    catch throw:{mango_error, _, _} = Error ->
        Reason = mango_util:format_error(Error),
        throw({bad_request, Reason})
    end,
    {ok, Writer, Resp} = create_writer(Req),
    %% I think I may re-write this to be spawned off into
    %% its own process to make error handlier less iffy.
    try
        {ok, LastWriter} = handle_actions(Writer, Db, Actions),
        {ok, LastResp} = mango_writer:close(LastWriter),
        end_resp(LastResp)
    catch
        throw:{mango_error, _, _} = RuntimeError ->
            RReason = mango_util:format_error(RuntimeError),
            twig:log(err, "Mango runtime error: ~s", [RReason]),
            end_resp(Resp);
        T:E ->
            Stack = erlang:get_stacktrace(),
            twig:log(err, "Error handling request: ~p~n  ~p", [{T, E}, Stack]),
            % Send an error here?
            end_resp(Resp)
    end;
handle_query_req(Req, _Db) ->
    chttpd:send_method_not_allowed(Req, "POST").


format_error(body_not_an_array) ->
    <<"The request body must be an array of action objects.">>;
format_error(Else) ->
    mango_util:fmt("Unknown error: ~p", [Else]).


get_body_actions(Db, Req) ->
    case chttpd:json_body(Req) of
        Body0 when is_list(Body0) ->
            {ok, lists:map(fun(Item) ->
                mango_action:new(Db, Item)
            end, Body0)};
        _ ->
            ?MANGO_ERROR(body_not_an_array)
    end.


handle_actions(Writer, _Db, []) ->
    {ok, Writer};
handle_actions(Writer, Db, [Action | Rest]) ->
    {ok, NewWriter} = mango_action:run(Writer, Db, Action),
    handle_actions(NewWriter, Db, Rest).


set_user_ctx(#httpd{user_ctx=Ctx}, Db) ->
    Db#db{user_ctx=Ctx}.


create_writer(Req) ->
    {ok, Resp} = start_resp(Req),
    {ok, Writer0} = mango_writer:new(?MODULE, send_data, Resp),
    {ok, Writer1} = mango_writer:arr_open(Writer0),
    {ok, Writer1, Resp}.


% Eventually we'll want to look into buffering data here so we're
% sending as few chunks as possible.
send_data(Resp, close) ->
    {ok, Resp};
send_data(Resp, Data) ->
    chttpd:send_chunk(Resp, Data).


start_resp(Req) ->
    chttpd:start_chunked_response(Req, 200, [
            {"Content-Type", "application/json"}
        ]).


end_resp(Resp) ->
    chttpd:send_chunk(Resp, [$\n]),
    couch_httpd:last_chunk(Resp).
