-module(couch_plugins_httpd).

-export([handle_req/1]).

-include_lib("couch_db.hrl").

handle_req(#httpd{method='PUT'}=Req) ->
    couch_httpd:send_json(Req, 202, {[{ok, true}]});
handle_req(Req) ->
    couch_httpd:send_method_not_allowed(Req, "PUT").
