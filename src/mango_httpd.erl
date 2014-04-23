-module(mango_httpd).

-export([
    handle_query_req/2
]).


-include_lib("couch/include/couch_db.hrl").


handle_query_req(#httpd{method='POST'}=Req, #db{name=_DbName}) ->
    couch_httpd:validate_ctype(Req, "application/json"),
    
    chttpd:send_json(Req, 200, {[{ok, true}]});
handle_query_req(Req, _Db) ->
    chttpd:send_method_not_allowed(Req, "POST").


get_body(Req) ->
    case chttpd:json_body(Req) of
        {_} = Body0 ->
            [Body0];
        Body0 when is_list(Body0) ->
            IsObj = fun({_}) -> true; (_) -> false end,
            case lists:all(IsObj, Body0) of
                true ->
                    Body0;
                false ->
                    throw({bad_request, invalid_body})
            end;
        _ ->
            throw({bad_request, invalid_body})
    end.



