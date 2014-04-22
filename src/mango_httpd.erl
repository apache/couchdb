-module(mango_httpd).

-export([
    handle_query_req/2
]).


handle_query_req(#httpd{method='POST'}=Req, #db{name=DbName}) ->
    send_json(Req, 200, {[{ok, true}]});
handle_query_req(Req, _Db) ->
    chttpd:send_method_not_allowed(Req, 'POST').



