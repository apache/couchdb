-module(mango_httpd).


-export([
    handle_query_req/2,
    format_error/1
]).


-include_lib("couch/include/couch_db.hrl").
-include("mango.hrl").


handle_query_req(#httpd{method='POST'}=Req, Db) ->
    couch_httpd:validate_ctype(Req, "application/json"),
    try
        {ok, Actions} = get_body_actions(Req),
    catch throw:Error ->
        Reason = mango_error:fmt(Error),
        throw({bad_request, Reason})
    end,
    {ok, Resp} = chttpd:start_json_resp(Req, 200, [
            {"Content-Type", "application/json"}
        ]),
    handle_actions(Resp, Db, Actions);
handle_query_req(Req, _Db) ->
    chttpd:send_method_not_allowed(Req, "POST").


format_error(body_not_an_array) ->
    <<"The request body must be an array of action objects.">>;
format_error(Else) ->
    mango_util:fmt("Unknown error: ~p", [Else]).


get_body_actions(Req) ->
    case chttpd:json_body(Req) of
        Body0 when is_list(Body0) ->
            {ok, lists:map(fun mango_action:new/1, Body0)};
        _ ->
            ?MANGO_ERROR(body_not_an_array)
    end.


handle_actions(Resp, _Db, []) ->
    {ok, Resp};
handle_actions(Resp, Db, [Action | Rest]) ->
    mango_action:run(Resp, Db, Action),
    handle_actions(Resp, Db, Rest).    

