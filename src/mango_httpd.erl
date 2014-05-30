-module(mango_httpd).


-export([
    handle_index_req/2,
	handle_find_req/2,
    format_error/1
]).


-include_lib("couch/include/couch_db.hrl").
-include("mango.hrl").


handle_index_req(#httpd{method='GET', path_parts=[_, _]}=Req, Db0) ->
    Db = set_user_ctx(Req, Db0),
    Idxs = lists:sort(mango_idx:list(Db)),
    JsonIdxs = lists:map(fun mango_idx:to_json/1, Idxs),
    % List indexes
	chttpd:send_json(Req, {[{indexes, JsonIdxs}]});

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
            case mango_crud:insert(Db, NewDDoc, [{w, "3"} | Opts]) of
                {ok, _} ->
                    <<"created">>;
                _ ->
                    ?MANGO_ERROR(error_saving_ddoc)
            end
    end,
	chttpd:send_json(Req, {[{result, Status}]});

handle_index_req(#httpd{method='DELETE',
        path_parts=[_, _, DDocId0, Type, Name]}=Req, Db0) ->
    Db = set_user_ctx(Req, Db0),
    DDocId = <<"_design/", DDocId0/binary>>,
    Idxs = mango_idx:list(Db),
    Filt = fun(Idx) ->
        IsDDoc = mango_idx:ddoc(Idx) == DDocId,
        IsType = mango_idx:type(Idx) == Type,
        IsName = mango_idx:name(Idx) == Name,
        IsDDoc andalso IsType andalso IsName
    end,
    case lists:filter(Filt, Idxs) of
        [Idx] ->
            {ok, DDoc} = mango_util:load_ddoc(Db, mango_idx:ddoc(Idx)),
            {ok, NewDDoc} = mango_idx:remove(DDoc, Idx),
            FinalDDoc = case NewDDoc#doc.body of
                {[{<<"language">>, <<"query">>}]} ->
                    NewDDoc#doc{deleted = true, body = {[]}};
                _ ->
                    NewDDoc
            end,
            case mango_crud:insert(Db, FinalDDoc, [{w, "3"}]) of
                {ok, _} ->
                    chttpd:send_json(Req, {[{ok, true}]});
                _ ->
                    ?MANGO_ERROR(error_saving_ddoc)
            end;
        [] ->
            throw(not_found)
    end;

handle_index_req(Req, _Db) ->
    chttpd:send_method_not_allowed(Req, "GET,POST,DELETE").


handle_find_req(#httpd{method='POST'}=Req, Db0) ->
    Db = set_user_ctx(Req, Db0),
    {ok, Opts0} = mango_opts:validate_find(chttpd:json_body_obj(Req)),
    {value, {selector, Sel}, Opts} = lists:keytake(selector, 1, Opts0),
    {ok, Resp0} = start_find_resp(Req),
    {ok, {Resp1, _}} = run_find(Resp0, Db, Sel, Opts),
    end_find_resp(Resp1);

handle_find_req(Req, _Db) ->
    chttpd:send_method_not_allowed(Req, "POST").


format_error(Else) ->
    mango_util:fmt("Unknown error: ~p", [Else]).


set_user_ctx(#httpd{user_ctx=Ctx}, Db) ->
    Db#db{user_ctx=Ctx}.


start_find_resp(Req) ->
    chttpd:start_delayed_json_response(Req, 200, [], "{\"docs\":[").


end_find_resp(Resp) ->
    chttpd:send_delayed_chunk(Resp, "\r\n]}\r\n"),
    chttpd:end_delayed_json_response(Resp).


run_find(Resp, Db, Sel, Opts) ->
    mango_crud:find(Db, Sel, fun handle_doc/2, {Resp, "\r\n"}, Opts).


handle_doc({row, Doc}, {Resp0, Prepend}) ->
    Chunk = [Prepend, ?JSON_ENCODE(Doc)],
    {ok, Resp1} = chttpd:send_delayed_chunk(Resp0, Chunk),
    {ok, {Resp1, ",\r\n"}}.
