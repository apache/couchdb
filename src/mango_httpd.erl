-module(mango_httpd).


-export([
    handle_index_req/2,
	handle_find_req/2,
    format_error/1
]).

-export([
    send_data/2
]).


-include_lib("couch/include/couch_db.hrl").
-include("mango.hrl").


handle_index_req(#httpd{method='GET'}=Req, _Db) ->
    % List indexes
	chttpd:send_json(Req, {[{ok, true}]});
handle_index_req(#httpd{method='POST'}=Req, _Db) ->
    % Create index
	chttpd:send_json(Req, {[{ok, true}]});
handle_index_req(#httpd{method='DELETE'}=Req, _Db) ->
    % Delete index
	chttpd:send_json(Req, {[{ok, true}]});
handle_index_req(Req, _Db) ->
    chttpd:send_method_not_allowed(Req, "GET,POST,DELETE").


handle_find_req(#httpd{method='POST'}=Req, _Db) ->
    % Execute query
	chttpd:send_json(Req, {[{ok, true}]});
handle_find_req(Req, _Db) ->
    chttpd:send_method_not_allowed(Req, "POST").


format_error(body_not_an_array) ->
    <<"The request body must be an array of action objects.">>;
format_error(Else) ->
    mango_util:fmt("Unknown error: ~p", [Else]).


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
