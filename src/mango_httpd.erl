-module(mango_httpd).


-export([
    handle_req/2
]).


-include_lib("couch/include/couch_db.hrl").
-include("mango.hrl").


handle_req(#httpd{} = Req, Db0) ->
    try
        Db = set_user_ctx(Req, Db0),
        handle_req_int(Req, Db)
    catch
        throw:{mango_error, Module, Reason} ->
            %Stack = erlang:get_stacktrace(),
            %twig:log(err, "Error: ~s :: ~w~n~p", [Module, Reason, Stack]),
            {Code, ErrorStr, ReasonStr} = mango_error:info(Module, Reason),
            Resp = {[
                {<<"error">>, ErrorStr},
                {<<"reason">>, ReasonStr}
            ]},
            chttpd:send_json(Req, Code, Resp)
    end.


handle_req_int(#httpd{path_parts=[_, <<"_index">> | _]} = Req, Db) ->
    handle_index_req(Req, Db);
handle_req_int(#httpd{path_parts=[_, <<"_find">> | _]} = Req, Db) ->
    handle_find_req(Req, Db);
handle_req_int(_, _) ->
    throw({not_found, missing}).


handle_index_req(#httpd{method='GET', path_parts=[_, _]}=Req, Db) ->
    Idxs = lists:sort(mango_idx:list(Db)),
    JsonIdxs = lists:map(fun mango_idx:to_json/1, Idxs),
	chttpd:send_json(Req, {[{indexes, JsonIdxs}]});

handle_index_req(#httpd{method='POST', path_parts=[_, _]}=Req, Db) ->
    {ok, Opts} = mango_opts:validate_idx_create(chttpd:json_body_obj(Req)),
    {ok, Idx0} = mango_idx:new(Db, Opts),
    {ok, Idx} = mango_idx:validate(Idx0),
    {ok, DDoc} = mango_util:load_ddoc(Db, mango_idx:ddoc(Idx)),
    Status = case mango_idx:add(DDoc, Idx) of
        {ok, DDoc} ->
            <<"exists">>;
        {ok, NewDDoc} ->
            CreateOpts = get_idx_create_opts(Opts),
            case mango_crud:insert(Db, NewDDoc, CreateOpts) of
                {ok, [{RespProps}]} ->
                    case lists:keyfind(error, 1, RespProps) of
                        {error, Reason} ->
                            ?MANGO_ERROR({error_saving_ddoc, Reason});
                        _ ->
                            <<"created">>
                    end;
                _ ->
                    ?MANGO_ERROR(error_saving_ddoc)
            end
    end,
	chttpd:send_json(Req, {[{result, Status}]});

handle_index_req(#httpd{method='DELETE',
        path_parts=[A, B, <<"_design">>, DDocId0, Type, Name]}=Req, Db) ->
    PathParts = [A, B, <<"_design/", DDocId0/binary>>, Type, Name],
    handle_index_req(Req#httpd{path_parts=PathParts}, Db);

handle_index_req(#httpd{method='DELETE',
        path_parts=[_, _, DDocId0, Type, Name]}=Req, Db) ->
    DDocId = case DDocId0 of
        <<"_design/", _/binary>> -> DDocId0;
        _ -> <<"_design/", DDocId0/binary>>
    end,
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
            DelOpts = get_idx_del_opts(Req),
            case mango_crud:insert(Db, FinalDDoc, DelOpts) of
                {ok, _} ->
                    chttpd:send_json(Req, {[{ok, true}]});
                _ ->
                    ?MANGO_ERROR(error_saving_ddoc)
            end;
        [] ->
            throw({not_found, missing})
    end;

handle_index_req(Req, _Db) ->
    chttpd:send_method_not_allowed(Req, "GET,POST,DELETE").


handle_find_req(#httpd{method='POST'}=Req, Db) ->
    {ok, Opts0} = mango_opts:validate_find(chttpd:json_body_obj(Req)),
    {value, {selector, Sel}, Opts} = lists:keytake(selector, 1, Opts0),
    {ok, Resp0} = start_find_resp(Req),
    {ok, {Resp1, _}} = run_find(Resp0, Db, Sel, Opts),
    end_find_resp(Resp1);

handle_find_req(Req, _Db) ->
    chttpd:send_method_not_allowed(Req, "POST").


set_user_ctx(#httpd{user_ctx=Ctx}, Db) ->
    Db#db{user_ctx=Ctx}.


get_idx_create_opts(Opts) ->
    case lists:keyfind(w, 1, Opts) of
        {w, N} when is_integer(N), N > 0 ->
            [{w, integer_to_list(N)}];
        _ ->
            [{w, "2"}]
    end.


get_idx_del_opts(Req) ->
    try
        WStr = chttpd:qs_value(Req, "w", "2"),
        _ = list_to_integer(WStr),
        [{w, WStr}]
    catch _:_ ->
        [{w, "2"}]
    end.


start_find_resp(Req) ->
    chttpd:start_delayed_json_response(Req, 200, [], "{\"docs\":[").


end_find_resp(Resp0) ->
    {ok, Resp1} = chttpd:send_delayed_chunk(Resp0, "\r\n]}"),
    chttpd:end_delayed_json_response(Resp1).


run_find(Resp, Db, Sel, Opts) ->
    mango_crud:find(Db, Sel, fun handle_doc/2, {Resp, "\r\n"}, Opts).


handle_doc({row, Doc}, {Resp0, Prepend}) ->
    Chunk = [Prepend, ?JSON_ENCODE(Doc)],
    {ok, Resp1} = chttpd:send_delayed_chunk(Resp0, Chunk),
    {ok, {Resp1, ",\r\n"}}.
