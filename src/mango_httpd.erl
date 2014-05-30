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


handle_index_req(#httpd{method='GET', path_parts=[_, _]}=Req, Db0) ->
    Db = set_user_ctx(Req, Db0),
    Idxs = mango_idx:list(Db),
    JsonIdxs = lists:map(fun mango_idx:to_json/1, Idxs),
    twig:log(err, "Json Idxs: ~p", [JsonIdxs]),
    % List indexes
	chttpd:send_json(Req, {[
	        {ok, true},
	        {indexes, JsonIdxs}
	    ]});

handle_index_req(#httpd{method='POST', path_parts=[_, _]}=Req, Db0) ->
    Db = set_user_ctx(Req, Db0),
    {ok, Opts} = mango_opts:validate_idx_create(chttpd:json_body_obj(Req)),
    {ok, Idx0} = mango_idx:new(Db, Opts),
    {ok, Idx} = mango_idx:validate(Idx0),
    {ok, DDoc} = mango_util:load_ddoc(Db, mango_idx:ddoc(Idx)),
    Status = case mango_idx:add(DDoc, Idx) of
        {ok, DDoc} ->
            <<"exists">>;
        {ok, NewDDoc} ->
            case mango_crud:insert(Db, NewDDoc, Opts) of
                {ok, Resp} ->
                    twig:log(err, "Resp: ~p", [Resp]),
                    <<"created">>;
                _ ->
                    ?MANGO_ERROR(error_saving_ddoc)
            end
    end,
	chttpd:send_json(Req, {[
	        {ok, true},
	        {result, Status}
	    ]});

handle_index_req(#httpd{method='DELETE',
        path_parts=[_, _, Type, DDoc, Name]}=Req, Db0) ->
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
